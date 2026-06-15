# ============================================================================
# report_result() – format one model into a copy-pasteable result line
# ----------------------------------------------------------------------------
# Turns a fitted model into a one-click Google Form link that drops your result
# onto the live Multiverse chart. Because every pair reports the SAME quantities
# – coefficient, standard error, t, residual df, n, and the partial correlation
# r = t / sqrt(t^2 + df) – results on different outcome scales and from different
# estimators can sit on one axis, exactly as the five Multi100 analysts were
# compared. That r conversion is the same one Multi100 used.
#
# Works with plm, fixest (feols), and lm objects.
#
# Usage (three lines):
#   source("R/report_result.R")
#   m <- fixest::feols(mcosmo ~ unemp_c | cntry + year, data = ten)
#   report_result(m, spec = "mcosmo ~ unemp_c | twoway-FE | all 2004–2013")
# ============================================================================

report_result <- function(model, spec, term = NULL) {

  # --- 1. Pull the coefficient table in a model-class-aware way --------------
  # Each package exposes its coefficients slightly differently, so we normalise
  # to a small data frame with columns: term, estimate, se, statistic.
  cls <- class(model)[1]

  if (inherits(model, "fixest")) {
    ct <- as.data.frame(fixest::coeftable(model))
    tab <- data.frame(
      term      = rownames(ct),
      estimate  = ct[["Estimate"]],
      se        = ct[["Std. Error"]],
      statistic = ct[["t value"]],
      stringsAsFactors = FALSE
    )
    df_resid <- fixest::degrees_freedom(model, type = "resid")
    n_obs    <- as.integer(fixest::fitstat(model, "n", simplify = TRUE))

  } else if (inherits(model, "plm")) {
    ct <- summary(model)$coefficients
    tab <- data.frame(
      term      = rownames(ct),
      estimate  = ct[, 1L],
      se        = ct[, 2L],
      statistic = ct[, 3L],
      stringsAsFactors = FALSE
    )
    df_resid <- stats::df.residual(model)
    n_obs    <- stats::nobs(model)

  } else if (inherits(model, "lm")) {
    ct <- summary(model)$coefficients
    tab <- data.frame(
      term      = rownames(ct),
      estimate  = ct[, 1L],
      se        = ct[, 2L],
      statistic = ct[, 3L],
      stringsAsFactors = FALSE
    )
    df_resid <- stats::df.residual(model)
    n_obs    <- stats::nobs(model)

  } else {
    stop("report_result(): unsupported model class '", cls,
         "' – this helper handles plm, fixest, and lm objects.")
  }

  # --- 2. Choose which coefficient to report --------------------------------
  # Default behaviour: find the exposure automatically. We look for a term
  # whose name contains 'unemp' (the workshop exposure, in any of its forms –
  # unemp, unemp_c, log(unemp), …) or the bare 'x' that fixest sometimes uses.
  # A caller who deviates further can always name the term explicitly.
  if (is.null(term)) {
    hit <- grepl("unemp|^x$", tab$term)
    if (any(hit)) {
      row <- tab[which(hit)[1L], ]
    } else {
      # Fall back to the first non-intercept coefficient.
      keep <- tab$term != "(Intercept)"
      if (!any(keep)) {
        stop("report_result(): the model has no non-intercept coefficient to report.")
      }
      row <- tab[which(keep)[1L], ]
    }
  } else {
    if (!term %in% tab$term) {
      stop("report_result(): term '", term, "' not found. Available terms: ",
           paste(tab$term, collapse = ", "))
    }
    row <- tab[tab$term == term, ]
  }

  # --- 3. Derived quantities -------------------------------------------------
  est  <- unname(row$estimate)
  se   <- unname(row$se)
  stat <- unname(row$statistic)
  df_resid <- as.integer(round(df_resid))
  n_obs    <- as.integer(round(n_obs))

  # Partial correlation r from the test statistic and its residual df –
  # the Multi100 standardisation. Sign follows the coefficient.
  r <- stat / sqrt(stat^2 + df_resid)

  # --- 4. Build the one-click submission link and print it -------------------
  # The class-results Google Form turns these numbers into a dot on the live
  # Multiverse chart. The link opens the form with every field pre-filled, so you
  # just press Send. (Facilitator: FORM_ID and the entry.* codes come from the
  # Form – see facilitator/live-results-setup.md, step 1.)
  FORM_ID <- "1FAIpQLSezoAEOnZUfP4pkfN28_XtEBwSyc2RLsXB-h8RPSegF7_t76A"
  field <- c(spec = "entry.832496914", b = "entry.1656311800", se = "entry.1337458262",
             t = "entry.1940779437", df = "entry.1682711955", n = "entry.1586392724",
             r = "entry.1660273001")
  value <- c(spec = spec,
             b  = format(round(est,  5), nsmall = 5),
             se = format(round(se,   5), nsmall = 5),
             t  = format(round(stat, 3), nsmall = 3),
             df = as.character(df_resid),
             n  = as.character(n_obs),
             r  = format(round(r,    3), nsmall = 3))
  query <- paste(vapply(names(field), function(k)
    paste0(field[[k]], "=", utils::URLencode(value[[k]], reserved = TRUE)),
    character(1)), collapse = "&")
  url <- sprintf(
    "https://docs.google.com/forms/d/e/%s/viewform?usp=pp_url&%s", FORM_ID, query)

  cat(sprintf(paste0("Your result:  spec = %s\n",
              "  b = %s | se = %s | t = %s | df = %d | n = %d | r = %s\n\n"),
              spec,
              format(round(est,  5), nsmall = 5),
              format(round(se,   5), nsmall = 5),
              format(round(stat, 3), nsmall = 3),
              df_resid, n_obs,
              format(round(r,    3), nsmall = 3)))
  cat("Submit (opens the form with your numbers pre-filled - just press Send):\n")
  cat(url, "\n")

  # Return the pieces invisibly so the result can also be captured / tested.
  invisible(list(
    spec = spec, term = row$term, estimate = est, se = se,
    statistic = stat, df = df_resid, n = n_obs, r = r, url = url
  ))
}
