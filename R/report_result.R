# ============================================================================
# report_result() – format one model into a copy-pasteable result line
# ----------------------------------------------------------------------------
# Turns a fitted model into the single standardised line every pair pastes
# into the workshop Discussion thread. Because every pair reports the SAME
# quantities – coefficient, standard error, t, residual df, n, and the partial
# correlation r = t / sqrt(t^2 + df) – results on different outcome scales and
# from different estimators can sit on one axis, exactly as the five Multi100
# analysts were compared. That r conversion is the same one Multi100 used.
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

  # --- 4. Emit the standardised line ----------------------------------------
  cat(sprintf(
    "RESULT | spec: %s | b = %s | se = %s | t = %s | df = %d | n = %d | r = %s\n",
    spec,
    format(round(est,  5), nsmall = 5),
    format(round(se,   5), nsmall = 5),
    format(round(stat, 3), nsmall = 3),
    df_resid,
    n_obs,
    format(round(r,    3), nsmall = 3)
  ))

  # Return the pieces invisibly so the line can also be captured / tested.
  invisible(list(
    spec = spec, term = row$term, estimate = est, se = se,
    statistic = stat, df = df_resid, n = n_obs, r = r
  ))
}
