# ########################################################################### #
# Title:        spec_tools.R – preregister, fit, and report one specification
# Purpose:      The specification menu, its validation, and the four helpers the
#               report and working script use. prereg() records one declared
#               specification; build_model() checks a hand-written formula
#               against it; run_spec() fits the real model and prints its
#               claim-aligned result; report_result() sends that result to the
#               live class chart. Every fit is a live estimation on `euframes` –
#               there is no pre-computed grid behind these numbers.
# Reads:        the `euframes` object in the calling environment (the corrected
#               EU-frames country-year panel)
# Sourced by:   index.qmd and working_script.R (source("R/spec_tools.R"))
# Author:       Chris Moreh
# Last updated: 2026-07-19
# ########################################################################### #

# The specification menu – one axis per line, every option a defensible choice.
# The outcome family (gaussian | beta) and the predictor (unemp | growth) are
# full first-class axes here, on the same footing as the estimator or the
# sample. Multi100's Task 2 fixed unemployment on a gaussian scale; the workshop
# opens both, and Task 1 of Multi100 already shows three of five analysts
# reaching for GDP measures when the choice was theirs.
.neg_framings <- c("mcomm", "mlib", "mneg")

.menu <- list(
  my_outcome     = c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg"),
  my_family      = c("gaussian", "beta"),
  my_predictor   = c("unemp", "growth"),
  my_pred_form   = c("raw", "log"),
  my_copredictor = c("none", "growth", "unemp"),
  my_estimator   = c("fe_twoway", "fe_country", "re", "pooled_cl"),
  my_sample      = c("all", "y2004_2008", "y2009_2013", "no_bailout", "no_GR_ES"),
  my_weights     = c("none", "n_cy")
)

.est_lab <- function(e) c(
  fe_twoway  = "two-way fixed effects (country + year)",
  fe_country = "country fixed effects",
  re         = "random effects",
  pooled_cl  = "pooled OLS, clustered by country")[[e]]

# The claim-carrying predictor's term as it appears in a formula and in the
# fitted coefficient table: log(unemp) when the log form is chosen, else the
# bare variable name.
.main_term <- function(ch) {
  if (ch$my_predictor == "unemp" && ch$my_pred_form == "log") "log(unemp)"
  else ch$my_predictor
}

# --- validation ------------------------------------------------------------
# Reads the eight my_* choices and claim_align from the caller's environment,
# checks each against the menu, and returns them as a named list. Every helper
# calls this first, so a stale or off-menu choice is caught before any fitting.
.check_choices <- function() {
  need <- names(.menu)
  missing <- need[!vapply(need, exists, logical(1))]
  if (length(missing) > 0)
    stop("Not declared yet: ", paste(missing, collapse = ", "),
         ". Fill in every choice in the preregistration cell and run it first.",
         call. = FALSE)
  if (!exists("claim_align"))
    stop("Also declare claim_align <- TRUE (the day's convention) in the preregistration cell.",
         call. = FALSE)
  ch <- lapply(need, function(k) get(k))
  names(ch) <- need
  for (k in need) {
    v <- ch[[k]]
    if (!is.character(v) || length(v) != 1 || !v %in% .menu[[k]])
      stop(sprintf("%s = \"%s\" is not on the menu. Options: %s.",
                   k, paste(v, collapse = ","),
                   paste(.menu[[k]], collapse = " | ")), call. = FALSE)
  }
  if (ch$my_estimator == "re" && ch$my_weights == "n_cy")
    stop("The random-effects estimator is never fitted with weights (the menu excludes re + n_cy). Set my_weights <- \"none\" or pick another estimator.",
         call. = FALSE)
  if (ch$my_predictor == "growth" && ch$my_pred_form == "log")
    stop("GDP growth enters raw only – it takes negative values, so the log is undefined. Set my_pred_form <- \"raw\".",
         call. = FALSE)
  if (ch$my_predictor == ch$my_copredictor)
    stop(sprintf("%s is already your claim-carrying predictor; my_copredictor must be \"none\" or the other measure.",
                 ch$my_predictor), call. = FALSE)
  ch
}

# --- the sample rules ------------------------------------------------------
.subset_data <- function(sample) {
  switch(sample,
    all        = euframes,
    y2004_2008 = euframes[euframes$year <= 2008, ],
    y2009_2013 = euframes[euframes$year >= 2009, ],
    no_bailout = euframes[euframes$bailout == 0, ],
    no_GR_ES   = euframes[!euframes$cntry %in% c("GR", "ES"), ]
  )
}

# The claim-alignment factor: negative framings are reverse-coded (1 - y), and
# growth's claim-expected sign is positive, so each flips the reported sign.
# Reverse-coding 1 - y is exact for both families (for beta, logit(1 - mu) =
# -logit(mu)), so multiplying the fitted b, statistic and r by this factor
# gives the same claim-aligned numbers as refitting on the reverse-coded
# outcome – the standard errors and |statistic| are untouched either way.
.flip <- function(ch)
  (if (ch$my_outcome %in% .neg_framings) -1 else 1) *
  (if (ch$my_predictor == "growth") -1 else 1)

# --- the live fit ----------------------------------------------------------
# Fits the specification `ch` describes as a real model on `euframes` and
# returns the claim-carrying predictor's coefficient on the RAW outcome scale.
# gaussian: fixest for the fixed-effects designs (classical SE, matching the
# published plm value) and the clustered pooled model, plm for random effects.
# beta (logit link): betareg with country/year dummies for the fixed-effects
# designs, cluster-robust by country for pooled, glmmTMB beta_family() for
# random effects. Weights are n_cy normalised to the sample size.
.fit_spec <- function(ch) {
  d    <- .subset_data(ch$my_sample)
  key  <- .main_term(ch)
  rhs  <- c(key, if (ch$my_copredictor != "none") ch$my_copredictor)
  wts  <- if (ch$my_weights == "n_cy") d$n_cy * nrow(d) / sum(d$n_cy) else NULL

  need_pkg <- function(p)
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("This specification needs the '%s' package. Install it with pak::pak(\"%s\"), then rerun.", p, p),
           call. = FALSE)

  if (ch$my_family == "gaussian") {
    need_pkg("fixest")
    if (ch$my_estimator %in% c("fe_twoway", "fe_country")) {
      fe  <- if (ch$my_estimator == "fe_twoway") "cntry + year" else "cntry"
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + "), "|", fe))
      m   <- fixest::feols(fml, data = d, weights = wts, vcov = "iid")
      ct  <- fixest::coeftable(m)
      out <- list(estimate = ct[key, "Estimate"], se = ct[key, "Std. Error"],
                  statistic = ct[key, "t value"],
                  df = fixest::degrees_freedom(m, type = "resid"),
                  n = as.integer(fixest::fitstat(m, "n", simplify = TRUE)))
    } else if (ch$my_estimator == "pooled_cl") {
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + ")))
      m   <- fixest::feols(fml, data = d, weights = wts, vcov = ~ cntry)
      ct  <- fixest::coeftable(m)
      out <- list(estimate = ct[key, "Estimate"], se = ct[key, "Std. Error"],
                  statistic = ct[key, "t value"],
                  df = fixest::degrees_freedom(m, type = "resid"),
                  n = as.integer(fixest::fitstat(m, "n", simplify = TRUE)))
    } else {                                            # random effects
      need_pkg("plm")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + ")))
      m   <- plm::plm(fml, data = d, index = c("cntry", "year"),
                      model = "random", effect = "individual")
      ct  <- summary(m)$coefficients
      out <- list(estimate = ct[key, 1L], se = ct[key, 2L], statistic = ct[key, 3L],
                  df = stats::df.residual(m), n = as.integer(stats::nobs(m)))
    }
  } else {                                              # beta (logit link)
    if (ch$my_estimator == "re") {
      need_pkg("glmmTMB")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + "), "+ (1 | cntry)"))
      m   <- glmmTMB::glmmTMB(fml, data = d, family = glmmTMB::beta_family(), weights = wts)
      ct  <- summary(m)$coefficients$cond
      out <- list(estimate = ct[key, 1L], se = ct[key, 2L], statistic = ct[key, 3L],
                  df = as.integer(stats::nobs(m) - nrow(ct)), n = as.integer(stats::nobs(m)))
    } else {
      need_pkg("betareg")
      dummies <- switch(ch$my_estimator,
        fe_twoway  = "+ factor(cntry) + factor(year)",
        fe_country = "+ factor(cntry)",
        pooled_cl  = "")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + "), dummies))
      m   <- betareg::betareg(fml, data = d, weights = wts, link = "logit")
      ct  <- summary(m)$coefficients$mean
      est <- ct[key, 1L]; se <- ct[key, 2L]; stat <- ct[key, 3L]
      if (ch$my_estimator == "pooled_cl") {             # cluster-robust by country
        need_pkg("sandwich")
        V  <- sandwich::vcovCL(m, cluster = d$cntry)
        se <- sqrt(V[key, key]); stat <- est / se
      }
      # df for the r conversion counts the mean-model coefficients only.
      out <- list(estimate = est, se = se, statistic = stat,
                  df = as.integer(stats::nobs(m) - nrow(ct)), n = as.integer(stats::nobs(m)))
    }
  }
  # Partial correlation on the raw scale; sign follows the coefficient.
  out$r <- out$statistic / sqrt(out$statistic^2 + out$df)
  out
}

# The one-line spec label the class chart reads – outcome (tagged _ca when a
# negative framing is reverse-coded), predictor and form, any co-predictor,
# estimator, sample, weights and family.
.spec_line <- function(ch) {
  neg <- ch$my_outcome %in% .neg_framings
  paste0(ch$my_outcome, if (neg) "_ca" else "", " ~ ",
         ch$my_predictor, "(", ch$my_pred_form, ")",
         if (ch$my_copredictor != "none") paste0(" + ", ch$my_copredictor) else "",
         " | ", ch$my_estimator, " | ", ch$my_sample,
         " | w:", ch$my_weights, " | ", ch$my_family)
}

# --- the four helpers the report uses --------------------------------------

# Task 5: record the one declared specification.
prereg <- function() {
  ch  <- .check_choices()
  neg <- ch$my_outcome %in% .neg_framings
  cat("DECLARED – one specification, recorded before seeing its result:\n\n")
  cat("  outcome    :", ch$my_outcome,
      if (neg) "(negative framing – reverse-coded automatically)" else "", "\n")
  cat("  family     :", ch$my_family, "\n")
  cat("  predictor  :", ch$my_predictor, paste0("(", ch$my_pred_form, ")"),
      if (ch$my_predictor == "growth") "(claim-expected sign flipped automatically)" else "", "\n")
  cat("  copredictor:", ch$my_copredictor, "\n")
  cat("  estimator  :", .est_lab(ch$my_estimator), "\n")
  cat("  sample     :", ch$my_sample, "   weights:", ch$my_weights, "\n")
  cat("  claim_align:", isTRUE(claim_align), "\n\n")
  cat("These choices are now fixed for the rest of the session – the later\n")
  cat("cells read them from here. This rendered cell is your preregistration\n")
  cat("record: it timestamps the choice before you see its result.\n")
  invisible(ch)
}

# Task 6a: check the hand-written formula against the preregistration. Returns
# a ticket the fit step consumes; it does not fit anything itself.
build_model <- function(formula, estimator) {
  ch <- .check_choices()
  if (missing(formula) || !inherits(formula, "formula"))
    stop("The first argument must be a formula: outcome ~ predictor terms, unquoted.",
         call. = FALSE)
  lhs <- all.vars(formula[[2]])
  if (!identical(lhs, ch$my_outcome))
    stop(sprintf("Your formula's outcome is '%s' but you preregistered '%s'. The preregistration is binding – fix the formula, not the prereg.",
                 deparse(formula[[2]]), ch$my_outcome), call. = FALSE)
  want <- c(.main_term(ch), if (ch$my_copredictor != "none") ch$my_copredictor)
  got  <- attr(terms(formula), "term.labels")
  if (!setequal(got, want))
    stop(sprintf("Right-hand side mismatch: you wrote { %s } but your preregistration implies { %s }.%s",
                 paste(got, collapse = ", "), paste(want, collapse = ", "),
                 if (.main_term(ch) == "log(unemp)" && !"log(unemp)" %in% got)
                   " You preregistered the log form – write log(unemp) in the formula." else ""),
         call. = FALSE)
  if (missing(estimator) || !identical(estimator, ch$my_estimator))
    stop(sprintf("estimator = \"%s\" does not match your preregistered \"%s\".",
                 if (missing(estimator)) "" else estimator, ch$my_estimator),
         call. = FALSE)
  cat("Model accepted – it matches your preregistration exactly:\n")
  cat("  ", deparse(formula), ", estimated by ", .est_lab(estimator), "\n", sep = "")
  cat("  Run it with run_spec(my_model).\n")
  structure(list(choices = ch, formula = formula, estimator = estimator),
            class = "spec_ticket")
}

# Task 6b: fit the real model the ticket describes and print its claim-aligned
# result. This is where the estimation actually happens.
run_spec <- function(spec) {
  if (missing(spec) || !inherits(spec, "spec_ticket"))
    stop("Pass the object build_model() returned, e.g. run_spec(my_model).",
         call. = FALSE)
  now <- .check_choices()
  if (!identical(spec$choices, now))
    stop("The preregistration cell has changed since you built this model – rerun prereg() and build_model() so they agree.",
         call. = FALSE)
  ch  <- spec$choices
  fit <- .fit_spec(ch)
  fl  <- .flip(ch)
  ca  <- isTRUE(claim_align)
  stat_lab <- if (ch$my_family == "beta") "z" else "t"
  b <- fit$estimate; tv <- fit$statistic; r <- fit$r
  if (ca) { b <- fl * b; tv <- fl * tv; r <- fl * r }
  p <- 2 * stats::pt(-abs(fit$statistic), fit$df)
  cat("Your preregistered specification, fitted:\n\n")
  cat("  ", .spec_line(ch), "\n", sep = "")
  if (ca && ch$my_outcome %in% .neg_framings)
    cat("  (negative framing – reverse-coded automatically: claim_align = TRUE)\n")
  if (ca && ch$my_predictor == "growth")
    cat("  (growth predictor – sign convention flipped automatically: claim_align = TRUE)\n")
  if (!ca)
    cat("  (claim_align = FALSE: shown on this outcome's own scale; the class chart converts)\n")
  cat(sprintf("\n  b = %.5f   se = %.5f   %s = %.3f on %d df   p %s\n",
              b, fit$se, stat_lab, tv, fit$df,
              if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)))
  cat(sprintf("  claim-aligned partial r = %.3f   (below zero supports the claim)\n",
              fl * fit$r))
  invisible(list(b = b, se = fit$se, stat = tv, df = fit$df, n = fit$n, r = fl * fit$r))
}

# Task 7: submit the preregistered specification to the live class chart. Takes
# no arguments – it reads the preregistration, refits, and always submits on
# the claim-aligned scale the chart is drawn on, whatever claim_align displays.
report_result <- function() {
  ch  <- .check_choices()
  fit <- .fit_spec(ch)
  fl  <- .flip(ch)
  b <- fl * fit$estimate; tv <- fl * fit$statistic; r <- fl * fit$r
  spec_label <- .spec_line(ch)

  FORM_ID <- "1FAIpQLSezoAEOnZUfP4pkfN28_XtEBwSyc2RLsXB-h8RPSegF7_t76A"
  field <- c(spec = "entry.832496914", b = "entry.1656311800", se = "entry.1337458262",
             t = "entry.1940779437", df = "entry.1682711955", n = "entry.1586392724",
             r = "entry.1660273001")
  value <- c(spec = spec_label, b = sprintf("%.5f", b),
             se = sprintf("%.5f", fit$se), t = sprintf("%.3f", tv),
             df = as.character(fit$df), n = as.character(fit$n),
             r = sprintf("%.3f", r))
  query <- paste(vapply(names(field), function(k)
    paste0(field[[k]], "=", utils::URLencode(value[[k]], reserved = TRUE)),
    character(1)), collapse = "&")
  url <- sprintf("https://docs.google.com/forms/d/e/%s/viewform?usp=pp_url&%s",
                 FORM_ID, query)

  cat("Your submission (claim-aligned, straight from your preregistration):\n\n")
  cat("  spec:", spec_label, "\n")
  cat(sprintf("  b = %.5f | se = %.5f | %s = %.3f | df = %d | n = %d | r = %.3f\n\n",
              b, fit$se, if (ch$my_family == "beta") "z" else "t", tv,
              fit$df, fit$n, r))
  cat("Open the link below (or copy the URL) – the form opens with every field\n")
  cat("pre-filled; just press Send.\n\n")
  cat(url, "\n")
  invisible(list(spec = spec_label, b = b, se = fit$se, statistic = tv,
                 df = fit$df, n = fit$n, r = r, url = url))
}
