# ########################################################################### #
# Title:        spec_tools.R – explore, preregister, fit and report
# Purpose:      The fourteen-decision specification menu, its validation, and
#               the helpers that the report and the working script use, in two
#               groups.
#
#               EXPLORATORY (Task 4), where nothing is committed: try_spec()
#               fits any specification from arguments that default to the
#               reproduced baseline, my_experiments() prints the running log,
#               and submit_exploration() sends the last one to the class chart.
#
#               DECLARED (Task 5), where the choice binds: prereg() records one
#               specification, build_model() checks a hand-written formula
#               against it, run_spec() fits the real model, report_result()
#               submits it.
#
#               Every fit is a live estimation. The six Level 1 decisions fix
#               the question; the Level 2 decisions select which version of the
#               country-year panel is fitted (how the outcome was built from the
#               items, what was done with respondents who mentioned nothing,
#               whether the "unemployment" item is in, whether cells are
#               design-weighted, which published series carries the predictor);
#               the Level 3 decisions fix the arithmetic. The one exception is
#               the multiple-imputation option of the no-opinion axis, which
#               needs the respondent-level file that GESIS does not let travel:
#               its pooled results are read from data/spec_grid_mi.csv and the
#               printout says so.
# Reads:        the `euframes` object in the calling environment (the base
#               country-year panel), data/teney_panel_measurement.csv,
#               data/teney_panel_weights.csv, data/predictor_measures.csv,
#               data/spec_grid_mi.csv
# Sourced by:   index.qmd and working_script.R (source("R/spec_tools.R"))
# Author:       Chris Moreh
# Last updated: 2026-09-06
# ########################################################################### #

.neg_framings <- c("mcomm", "mlib", "mneg")
.outcomes     <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")
# items in each set, for the per-item rate: all 13 items, or without the
# "unemployment" item, which leaves the communitarian set with three
.k_items <- list(all = c(mcosmo = 5, mutil = 2, mcomm = 4, mlib = 2, mpos = 7, mneg = 6),
                 un  = c(mcosmo = 5, mutil = 2, mcomm = 3, mlib = 2, mpos = 7, mneg = 5))

# The menu, grouped as the course groups it. Level 1 changes the question,
# Level 2 the measurement, Level 3 the estimation. The scoring rule is a
# Level 1 decision: a share of a respondent's own mentions and a count of the
# set's items are different quantities, and the per-item rate is the count
# divided by the number of items in the set.
.menu <- list(
  # Level 1 – Estimands
  my_outcome      = .outcomes,
  my_predictor    = c("unemp", "growth"),
  my_copredictor  = c("none", "other"),
  my_estimator    = c("fe_twoway", "fe_country", "re", "pooled_cl"),
  my_sample       = c("all", "y2004_2008", "y2009_2013", "no_bailout", "no_GR_ES"),
  my_weights      = c("none", "n_cell", "n_eff", "pop"),
  my_construction = c("share", "count", "rate"),
  # Level 2 – Measurement
  my_noop         = c("zero", "drop", "split", "mi_mice", "mi_amelia"),
  my_item_set     = c("all", "un"),
  my_cell_weight  = c("unweighted", "design"),
  my_measure      = c("wb_nat", "es_1574_act", "es_2064_pop", "es_2564_lfs", "es_1574_tips",
                      "wb_growth", "es_growth"),
  # Level 3 – Estimation
  my_pred_form    = c("raw", "log"),
  my_family       = c("gaussian", "beta"),
  my_se           = c("conventional", "clustered")
)
.level <- c(rep("estimand", 7), rep("measurement", 4), rep("estimation", 3))
names(.level) <- names(.menu)

.measures_for <- list(unemp  = c("wb_nat", "es_1574_act", "es_2064_pop", "es_2564_lfs", "es_1574_tips"),
                      growth = c("wb_growth", "es_growth"))

# the measurement settings at which the beta family and the imputed variants
# were pre-computed; anything else on those two options is not in the universe
.canonical_measurement <- function(ch)
  ch$my_construction %in% c("share", "rate") && ch$my_noop %in% c("zero", "mi_mice", "mi_amelia") &&
  ch$my_item_set == "all" && ch$my_cell_weight == "unweighted" &&
  ch$my_measure %in% c("wb_nat", "wb_growth")

.est_lab <- function(e) c(
  fe_twoway  = "two-way fixed effects (country + year)",
  fe_country = "country fixed effects",
  re         = "random effects",
  pooled_cl  = "pooled OLS")[[e]]

# The term for the claim-carrying predictor as it appears in a formula and in
# the fitted coefficient table: log(unemp) when the log form is chosen, else
# the bare variable name.
.main_term <- function(ch) {
  if (ch$my_predictor == "unemp" && ch$my_pred_form == "log") "log(unemp)"
  else ch$my_predictor
}
.copred_term <- function(ch) if (ch$my_predictor == "unemp") "growth" else "unemp"

# --- validation ------------------------------------------------------------
# Two entry points into the same rules. .validate() takes a named list, which
# is how the exploratory try_spec() supplies its arguments; .check_choices()
# reads the fourteen my_* choices and claim_align out of the calling
# environment, which is how the declared flow reads the preregistration block.
.validate <- function(ch, as_args = FALSE) {
  # the earlier menu named the co-predictor by its variable; both spellings
  # still work, and the old weight code n_cy means the respondent count
  if (identical(ch$my_copredictor, .copred_term(ch))) ch$my_copredictor <- "other"
  if (identical(ch$my_weights, "n_cy")) ch$my_weights <- "n_cell"
  for (k in names(.menu)) {
    v <- ch[[k]]
    shown <- if (as_args) sub("^my_", "", k) else k
    if (is.null(v) || !is.character(v) || length(v) != 1 || !v %in% .menu[[k]])
      stop(sprintf("%s = \"%s\" is not on the menu. Options: %s.",
                   shown, paste(v, collapse = ","),
                   paste(.menu[[k]], collapse = " | ")), call. = FALSE)
  }
  if (ch$my_estimator == "re" && ch$my_weights != "none")
    stop("The random-effects estimator is never fitted with weights. Use weights = \"none\" or pick another estimator.",
         call. = FALSE)
  if (ch$my_predictor == "growth" && ch$my_pred_form == "log")
    stop("GDP growth enters raw only – it takes negative values, so the log is undefined. Use pred_form = \"raw\".",
         call. = FALSE)
  if (!ch$my_measure %in% .measures_for[[ch$my_predictor]])
    stop(sprintf("measure = \"%s\" is a series for %s; with predictor = \"%s\" choose one of: %s.",
                 ch$my_measure, setdiff(names(.measures_for), ch$my_predictor), ch$my_predictor,
                 paste(.measures_for[[ch$my_predictor]], collapse = " | ")), call. = FALSE)
  if (ch$my_family == "beta" && !.canonical_measurement(ch))
    stop("The beta family is fitted at the canonical measurement settings only: construction = \"share\" or \"rate\" (a count is not a proportion), noop = \"zero\", item_set = \"all\", cell_weight = \"unweighted\", measure = the World Bank series.",
         call. = FALSE)
  if (ch$my_family == "beta" && ch$my_estimator == "re" && ch$my_se == "clustered")
    stop("Clustered errors are not available for the beta random-effects fit. Use se = \"conventional\" with that pairing.",
         call. = FALSE)
  if (ch$my_noop %in% c("mi_mice", "mi_amelia") &&
      !(ch$my_construction == "share" && ch$my_item_set == "all" &&
        ch$my_cell_weight == "unweighted" && ch$my_measure == "wb_nat" &&
        ch$my_predictor == "unemp" && ch$my_pred_form == "raw" && ch$my_family == "gaussian"))
    stop("The imputed no-opinion options were pre-computed for the unemployment arm at the canonical measurement and estimation settings only (share, all items, unweighted cells, World Bank series, raw form, gaussian).",
         call. = FALSE)
  ch
}

.check_choices <- function() {
  need <- names(.menu)
  missing <- need[!vapply(need, exists, logical(1))]
  if (length(missing) > 0)
    stop("Not declared yet: ", paste(missing, collapse = ", "),
         ". Fill in every choice in the preregistration cell and run it first.",
         call. = FALSE)
  if (!exists("claim_align"))
    stop("Also declare claim_align <- TRUE (the course convention) in the preregistration cell.",
         call. = FALSE)
  ch <- lapply(need, function(k) get(k))
  names(ch) <- need
  .validate(ch)
}

# --- the data behind a specification ---------------------------------------
# The base panel `euframes` supplies the cell identifiers, the bailout flag
# and the World Bank series; the measurement panel supplies the outcome under
# the chosen construction, item set, no-opinion rule and cell weighting, with
# the cell counts that go with them; the predictor series file supplies the
# chosen published series; the weights panel supplies adult population. Files
# are read once and cached.
.cache <- new.env(parent = emptyenv())
.read_once <- function(name, path) {
  if (is.null(.cache[[name]])) {
    if (!file.exists(path))
      stop(sprintf("%s is missing – the workspace ships it under data/.", path), call. = FALSE)
    .cache[[name]] <- utils::read.csv(path, stringsAsFactors = FALSE)
  }
  .cache[[name]]
}

.build_data <- function(ch) {
  me <- .read_once("measurement", "data/teney_panel_measurement.csv")
  pm <- .read_once("predictors",  "data/predictor_measures.csv")
  wt <- .read_once("weights",     "data/teney_panel_weights.csv")
  noop <- if (ch$my_noop %in% c("mi_mice", "mi_amelia")) "zero" else ch$my_noop
  # the per-item rate is the count divided by the number of items in the set
  cons <- if (ch$my_construction == "rate") "count" else ch$my_construction
  v <- me[me$construction == cons & me$item_set == ch$my_item_set &
          me$noop == noop & me$cell_weight == ch$my_cell_weight, ]
  wide <- stats::reshape(v[, c("cntry", "year", "n_cell", "n_eff_cell", "outcome", "value")],
                         idvar = c("cntry", "year", "n_cell", "n_eff_cell"),
                         timevar = "outcome", direction = "wide")
  names(wide) <- sub("^value\\.", "", names(wide))
  if (ch$my_construction == "rate") {
    k <- .k_items[[ch$my_item_set]]
    for (o in names(k)) if (o %in% names(wide)) wide[[o]] <- wide[[o]] / k[[o]]
  }
  d <- merge(euframes[, c("cntry", "year", "bailout", "unemp", "growth", "n_cy")],
             wide, by = c("cntry", "year"))
  d <- merge(d, wt[, c("cntry", "year", "pop_15plus")], by = c("cntry", "year"), all.x = TRUE)
  series <- pm[pm$measure == ch$my_measure, c("cntry", "year", "value")]
  d <- merge(d, series, by = c("cntry", "year"))      # a short series shortens the panel
  if (ch$my_predictor == "unemp") d$unemp <- d$value else d$growth <- d$value
  d$value <- NULL
  d <- switch(ch$my_sample,
    all        = d,
    y2004_2008 = d[d$year <= 2008, ],
    y2009_2013 = d[d$year >= 2009, ],
    no_bailout = d[!d$cntry %in% unique(d$cntry[d$bailout == 1]), ],
    no_GR_ES   = d[!d$cntry %in% c("GR", "ES"), ])
  d$w <- switch(ch$my_weights, none = rep(1, nrow(d)), n_cell = d$n_cell,
                n_eff = d$n_eff_cell, pop = d$pop_15plus)
  d
}

# The claim-alignment factor: negative framings are reverse-coded (1 - y), and
# the claim-expected sign for growth is positive, so each flips the reported
# sign. Reverse-coding is exact for both families (for beta,
# logit(1 - mu) = -logit(mu)), so multiplying b, statistic and r by this factor
# gives the claim-aligned numbers without refitting.
.flip <- function(ch)
  (if (ch$my_outcome %in% .neg_framings) -1 else 1) *
  (if (ch$my_predictor == "growth") -1 else 1)

.need_pkg <- function(p)
  if (!requireNamespace(p, quietly = TRUE))
    stop(sprintf("This specification needs the '%s' package. Install it with pak::pak(\"%s\"), then rerun.", p, p),
         call. = FALSE)

# --- the live fit ----------------------------------------------------------
# gaussian: fixest for the fixed-effects and pooled designs, plm for random
# effects; beta (logit link): betareg with country/year dummies, glmmTMB for
# random effects. Regression weights are normalised to the sample size for the
# beta family, as case weights need to be. The standard-error convention is
# applied to the same fit: conventional, or clustered by country.
.fit_spec <- function(ch) {
  if (ch$my_noop %in% c("mi_mice", "mi_amelia")) return(.lookup_mi(ch))
  d   <- .build_data(ch)
  key <- .main_term(ch)
  rhs <- c(key, if (ch$my_copredictor == "other") .copred_term(ch))
  wts <- if (ch$my_weights == "none") NULL else d$w
  clustered <- ch$my_se == "clustered"

  if (ch$my_family == "gaussian") {
    .need_pkg("fixest")
    if (ch$my_estimator != "re") {
      fe  <- switch(ch$my_estimator, fe_twoway = "| cntry + year", fe_country = "| cntry", pooled_cl = "")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + "), fe))
      m   <- fixest::feols(fml, data = d, weights = wts)
      ct  <- fixest::coeftable(m, vcov = if (clustered) ~cntry else "iid")
      out <- list(estimate = ct[key, "Estimate"], se = ct[key, "Std. Error"],
                  statistic = ct[key, "t value"],
                  df = fixest::degrees_freedom(m, type = "resid"), n = stats::nobs(m))
    } else {
      .need_pkg("plm")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + ")))
      m   <- plm::plm(fml, data = d, index = c("cntry", "year"), model = "random")
      b   <- stats::coef(m)[[key]]
      V   <- if (clustered) plm::vcovHC(m, method = "arellano", cluster = "group") else stats::vcov(m)
      se  <- sqrt(diag(V))[[key]]
      out <- list(estimate = b, se = se, statistic = b / se,
                  df = nrow(d) - length(stats::coef(m)), n = nrow(d))
    }
  } else {
    # as.numeric() because the population column is integer, and its product
    # with the row count overflows R's integer range for the large countries
    wts_b <- if (is.null(wts)) NULL else as.numeric(wts) * nrow(d) / sum(as.numeric(wts))
    if (ch$my_estimator == "re") {
      .need_pkg("glmmTMB")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + "), "+ (1 | cntry)"))
      m   <- glmmTMB::glmmTMB(fml, data = d, family = glmmTMB::beta_family(), weights = wts_b)
      ct  <- summary(m)$coefficients$cond
      out <- list(estimate = ct[key, 1L], se = ct[key, 2L], statistic = ct[key, 3L],
                  df = as.integer(stats::nobs(m) - nrow(ct)), n = as.integer(stats::nobs(m)))
    } else {
      .need_pkg("betareg")
      dummies <- switch(ch$my_estimator, fe_twoway = "+ factor(cntry) + factor(year)",
                        fe_country = "+ factor(cntry)", pooled_cl = "")
      fml <- stats::as.formula(paste(ch$my_outcome, "~", paste(rhs, collapse = " + "), dummies))
      m   <- betareg::betareg(fml, data = d, weights = wts_b, link = "logit")
      ct  <- summary(m)$coefficients$mean
      est <- ct[key, 1L]; se <- ct[key, 2L]
      if (clustered) { .need_pkg("sandwich"); se <- sqrt(sandwich::vcovCL(m, cluster = d$cntry)[key, key]) }
      out <- list(estimate = est, se = se, statistic = est / se,
                  df = as.integer(stats::nobs(m) - nrow(ct)), n = as.integer(stats::nobs(m)))
    }
  }
  out$r <- out$statistic / sqrt(out$statistic^2 + out$df)
  out$source <- "fitted live"
  out
}

# The imputed variants cannot be fitted here: they need the respondent-level
# file. Their pooled results (ten imputations, Rubin's rules) are read instead.
.lookup_mi <- function(ch) {
  mi <- .read_once("mi", "data/spec_grid_mi.csv")
  engine <- if (ch$my_noop == "mi_mice") "mice_pmm" else "amelia_emb"
  hit <- mi[mi$engine == engine & mi$outcome == ch$my_outcome &
            mi$copredictor == ch$my_copredictor & mi$estimator == ch$my_estimator &
            mi$sample == ch$my_sample & mi$reg_weight == ch$my_weights & mi$se == ch$my_se, ]
  if (nrow(hit) != 1) stop("That imputed variant is not in the pre-computed results.", call. = FALSE)
  if (is.na(hit$estimate) || is.na(hit$statistic))
    stop("That imputed variant did not pool when the results were computed – choose the other engine or another no-opinion rule.",
         call. = FALSE)
  list(estimate = hit$estimate, se = hit$std_error, statistic = hit$statistic,
       df = hit$df, n = hit$n, r = hit$r, source = "read from the pre-computed imputation tier")
}

# The one-line spec label that the class chart reads: the six Level 1 choices,
# then the measurement and estimation choices in brackets.
.spec_line <- function(ch) {
  neg <- ch$my_outcome %in% .neg_framings
  paste0(ch$my_outcome, if (neg) "_ca" else "", " ~ ", ch$my_predictor, "(", ch$my_pred_form, ")",
         if (ch$my_copredictor == "other") paste0(" + ", .copred_term(ch)) else "",
         " | ", ch$my_estimator, " | ", ch$my_sample, " | w:", ch$my_weights,
         " [", ch$my_construction, ", ", ch$my_noop, ", items:", ch$my_item_set, ", cells:",
         ch$my_cell_weight, ", ", ch$my_measure, " | ", ch$my_family, ", se:", ch$my_se, "]")
}

# --- Task 4: the exploratory helpers ---------------------------------------
.tried <- new.env(parent = emptyenv())
.tried$log <- NULL
.tried$last <- NULL

try_spec <- function(outcome = "mcosmo", predictor = "unemp", copredictor = "none",
                     estimator = "fe_twoway", sample = "all", weights = "none",
                     construction = "share", noop = "zero", item_set = "all",
                     cell_weight = "unweighted", measure = NULL,
                     pred_form = "raw", family = "gaussian", se = "conventional") {
  if (is.null(measure)) measure <- if (predictor == "unemp") "wb_nat" else "wb_growth"
  ch <- .validate(list(my_outcome = outcome, my_predictor = predictor, my_copredictor = copredictor,
                       my_estimator = estimator, my_sample = sample, my_weights = weights,
                       my_construction = construction, my_noop = noop, my_item_set = item_set,
                       my_cell_weight = cell_weight, my_measure = measure,
                       my_pred_form = pred_form, my_family = family, my_se = se), as_args = TRUE)
  fit <- .fit_spec(ch)
  fl  <- .flip(ch)
  p   <- 2 * stats::pt(-abs(fit$statistic), fit$df)
  stat_lab <- if (ch$my_family == "beta") "z" else "t"

  cat(.spec_line(ch), "\n")
  cat(sprintf("  b = %.5f   se = %.5f   %s = %.3f on %d df   p %s   (%s)\n",
              fl * fit$estimate, fit$se, stat_lab, fl * fit$statistic, round(fit$df),
              if (p < 0.001) "< 0.001" else sprintf("= %.3f", p), fit$source))
  cat(sprintf("  claim-aligned partial r = %.3f   (below zero supports the claim)\n", fl * fit$r))

  row <- as.data.frame(setNames(as.list(unlist(ch)), sub("^my_", "", names(ch))),
                       stringsAsFactors = FALSE)
  row$b <- fl * fit$estimate; row$t <- fl * fit$statistic
  row$r_aligned <- fl * fit$r; row$sig <- p < 0.05
  .tried$log  <- rbind(.tried$log, row)
  .tried$last <- list(ch = ch, fit = fit)
  invisible(row)
}

# The running log of everything try_spec() has fitted. Only the axes that
# varied are shown; the constant ones are the baseline.
my_experiments <- function() {
  if (is.null(.tried$log)) {
    cat("Nothing tried yet – call try_spec() at least once.\n")
    return(invisible(NULL))
  }
  d <- .tried$log
  axes <- sub("^my_", "", names(.menu))
  varied <- axes[vapply(d[axes], function(x) length(unique(x)) > 1, logical(1))]
  show <- d[, c(varied, "b", "t", "r_aligned", "sig"), drop = FALSE]
  show$b <- signif(show$b, 3); show$t <- round(show$t, 2)
  show$r_aligned <- round(show$r_aligned, 3)
  cat("Specifications you have tried (", nrow(d), "):\n\n", sep = "")
  print(show, row.names = FALSE)
  cat("\nSpread of claim-aligned r: ",
      sprintf("%.3f to %.3f", min(d$r_aligned), max(d$r_aligned)),
      "  |  significant: ", sum(d$sig), " of ", nrow(d), "\n", sep = "")
  invisible(show)
}

submit_exploration <- function() {
  if (is.null(.tried$last))
    stop("Nothing to submit – call try_spec() first, ending with the specification you want on the chart.",
         call. = FALSE)
  .submit(.tried$last$ch, .tried$last$fit, "explore",
          "Your exploratory submission (claim-aligned, the last specification you tried):")
}

# --- the declared helpers that the report uses -----------------------------

prereg <- function() {
  ch  <- .check_choices()
  neg <- ch$my_outcome %in% .neg_framings
  cat("DECLARED – one specification, recorded before seeing its result:\n\n")
  cat("  The question (Level 1)\n")
  cat("    outcome    :", ch$my_outcome,
      if (neg) "(negative framing – reverse-coded automatically)" else "", "\n")
  cat("    predictor  :", ch$my_predictor,
      if (ch$my_predictor == "growth") "(claim-expected sign flipped automatically)" else "", "\n")
  cat("    adjusted   :", if (ch$my_copredictor == "other") .copred_term(ch) else "nothing", "\n")
  cat("    estimator  :", .est_lab(ch$my_estimator), "\n")
  cat("    sample     :", ch$my_sample, "   whose average:", ch$my_weights, "\n")
  cat("    scored as  :", ch$my_construction, "\n")
  cat("  The measurement (Level 2)\n")
  cat("    no-opinion rule:", ch$my_noop, "  items:", ch$my_item_set, "\n")
  cat("    cells:", ch$my_cell_weight, "  series:", ch$my_measure, "\n")
  cat("  The estimation (Level 3)\n")
  cat("    form:", ch$my_pred_form, "  family:", ch$my_family, "  errors:", ch$my_se, "\n")
  cat("  claim_align:", isTRUE(claim_align), "\n\n")
  cat("These choices are now fixed for the rest of the session – the later\n")
  cat("cells read them from here. This rendered cell is your preregistration\n")
  cat("record: it timestamps the choice before you see its result.\n")
  invisible(ch)
}

build_model <- function(formula, estimator) {
  ch <- .check_choices()
  if (missing(formula) || !inherits(formula, "formula"))
    stop("The first argument must be a formula: outcome ~ predictor terms, unquoted.",
         call. = FALSE)
  lhs <- all.vars(formula[[2]])
  if (!identical(lhs, ch$my_outcome))
    stop(sprintf("The outcome in your formula is '%s' but you preregistered '%s'. The preregistration is binding – fix the formula, not the prereg.",
                 deparse(formula[[2]]), ch$my_outcome), call. = FALSE)
  want <- c(.main_term(ch), if (ch$my_copredictor == "other") .copred_term(ch))
  got  <- attr(terms(formula), "term.labels")
  if (!setequal(got, want))
    stop(sprintf("Right-hand side mismatch: you wrote { %s } but your preregistration implies { %s }.%s",
                 paste(got, collapse = ", "), paste(want, collapse = ", "),
                 if (.main_term(ch) == "log(unemp)" && !"log(unemp)" %in% got)
                   " You preregistered the log form – write log(unemp) in the formula." else ""),
         call. = FALSE)
  if (missing(estimator) || !identical(estimator, ch$my_estimator))
    stop(sprintf("estimator = \"%s\" does not match your preregistered \"%s\".",
                 if (missing(estimator)) "" else estimator, ch$my_estimator), call. = FALSE)
  cat("Model accepted – it matches your preregistration exactly:\n")
  cat("  ", deparse(formula), ", estimated by ", .est_lab(estimator), "\n", sep = "")
  cat("  Run it with run_spec(my_model).\n")
  structure(list(choices = ch, formula = formula, estimator = estimator), class = "spec_ticket")
}

run_spec <- function(spec) {
  if (missing(spec) || !inherits(spec, "spec_ticket"))
    stop("Pass the object that build_model() returned, e.g. run_spec(my_model).", call. = FALSE)
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
  cat(paste0("Your preregistered specification, ", fit$source, ":\n\n"))
  cat("  ", .spec_line(ch), "\n", sep = "")
  if (ca && ch$my_outcome %in% .neg_framings)
    cat("  (negative framing – reverse-coded automatically: claim_align = TRUE)\n")
  if (ca && ch$my_predictor == "growth")
    cat("  (growth predictor – sign convention flipped automatically: claim_align = TRUE)\n")
  if (!ca)
    cat("  (claim_align = FALSE: shown on this outcome's own scale; the class chart converts)\n")
  cat(sprintf("\n  b = %.5f   se = %.5f   %s = %.3f on %d df   p %s\n",
              b, fit$se, stat_lab, tv, round(fit$df),
              if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)))
  cat(sprintf("  claim-aligned partial r = %.3f   (below zero supports the claim)\n", fl * fit$r))
  invisible(list(b = b, se = fit$se, stat = tv, df = fit$df, n = fit$n, r = fl * fit$r))
}

# One submission, used by both phases. The phase is prefixed to the free-text
# spec label, and all fourteen decisions travel inside that label; the form,
# its fields and the collection pipeline are frozen infrastructure and do not
# change.
.submit <- function(ch, fit, phase, lead) {
  fl <- .flip(ch)
  b <- fl * fit$estimate; tv <- fl * fit$statistic; r <- fl * fit$r
  spec_label <- paste0(phase, ": ", .spec_line(ch))

  FORM_ID <- "1FAIpQLSezoAEOnZUfP4pkfN28_XtEBwSyc2RLsXB-h8RPSegF7_t76A"
  field <- c(spec = "entry.832496914", b = "entry.1656311800", se = "entry.1337458262",
             t = "entry.1940779437", df = "entry.1682711955", n = "entry.1586392724",
             r = "entry.1660273001")
  value <- c(spec = spec_label, b = sprintf("%.5f", b),
             se = sprintf("%.5f", fit$se), t = sprintf("%.3f", tv),
             df = as.character(round(fit$df)), n = as.character(fit$n),
             r = sprintf("%.3f", r))
  query <- paste(vapply(names(field), function(k)
    paste0(field[[k]], "=", utils::URLencode(value[[k]], reserved = TRUE)),
    character(1)), collapse = "&")
  url <- sprintf("https://docs.google.com/forms/d/e/%s/viewform?usp=pp_url&%s", FORM_ID, query)

  cat(lead, "\n\n")
  cat("  spec:", spec_label, "\n")
  cat(sprintf("  b = %.5f | se = %.5f | %s = %.3f | df = %d | n = %d | r = %.3f\n\n",
              b, fit$se, if (ch$my_family == "beta") "z" else "t", tv, round(fit$df), fit$n, r))
  cat("Open the link below (or copy the URL) – the form opens with every field\n")
  cat("pre-filled; just press Send.\n\n")
  cat(url, "\n")
  invisible(list(spec = spec_label, b = b, se = fit$se, statistic = tv,
                 df = fit$df, n = fit$n, r = r, url = url))
}

report_result <- function() {
  ch  <- .check_choices()
  fit <- .fit_spec(ch)
  .submit(ch, fit, "declared",
          "Your declared submission (claim-aligned, straight from your preregistration):")
}
