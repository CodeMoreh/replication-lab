##############################################################################
# Title:        spec_tools.R – explore, preregister, fit and report
# Purpose:      The specification menu, its validation, and the helpers that
#               the report and the working script use, in two groups.
#
#               EXPLORATORY (Task 4): try_spec() fits any specification on the
#               curve from arguments that default to the reproduced one,
#               my_experiments() prints the running log, and
#               submit_exploration() sends the last one to the class chart.
#
#               DECLARED (Task 5), where one specification is fixed in the
#               preregistration block before it is fitted: prereg() checks it
#               against the menu and prints it, build_model() checks a
#               hand-written formula against it, run_spec() fits it,
#               report_result() submits it.
#
#               The menu has fourteen decisions, and the curve of the claim
#               keeps 864 specifications of them. An option that is not on the
#               curve stops with the reason recorded in data/menu_options.csv
#               and the page of the course site that teaches it. Every
#               specification on the curve is a live fit on the country-year
#               panel that its measurement choices select, tested by the
#               restricted wild cluster bootstrap by country of
#               R/_wild_bootstrap.R, a verbatim copy of the file that classes
#               the curve, so that it returns the stored p-value exactly. The
#               CR2 p-value and the placement among the 864 are read from
#               data/spec_curve.csv, so that CR2 needs no further package.
#               Base R, dplyr, readr and fixest, all already required by the
#               report. Through it the report reads data/EUframes_cy.csv,
#               data/teney_panel_measurement.csv, data/predictor_measures.csv,
#               data/spec_curve.csv and data/menu_options.csv.
# Author:       Chris Moreh
# Last updated: 2026-10-25
# Sourced by:   index.qmd and working_script.R (source("R/spec_tools.R"))
##############################################################################

.wcr <- new.env()
sys.source("R/_wild_bootstrap.R", envir = .wcr)
.course_url <- "https://codemoreh.github.io/applied-replication"

# The thirteen axis codes that key the curve, in its column order; the
# inference rule is the fourteenth decision and is the same for every row.
.axis_codes <- c("outcome", "predictor", "copredictor", "estimator", "sample", "weights",
                 "construction", "noop", "item_set", "cell_weight", "measure", "pred_form",
                 "family")
.neg_framings <- c("mcomm", "mlib", "mneg")

# The options that the curve keeps, grouped as the course groups the menu.
# Level 1 changes the question, Level 2 the measurement, Level 3 the
# estimation. Every other option of the menu is listed in menu_options.csv.
.menu <- list(
  # Level 1 – Estimands
  my_outcome      = c("mcosmo", "mutil", "mpos"),
  my_predictor    = c("unemp", "growth"),
  my_copredictor  = c("none", "other"),
  my_estimator    = c("fe_twoway", "fe_country"),
  my_sample       = "all",
  my_weights      = "none",
  my_construction = "share",
  # Level 2 – Measurement
  my_noop         = c("zero", "drop"),
  my_item_set     = c("all", "un"),
  my_cell_weight  = c("unweighted", "design"),
  my_measure      = c("wb_nat", "es_1574_act", "es_2064_pop", "es_2564_lfs", "wb_growth"),
  # Level 3 – Estimation
  my_pred_form    = c("raw", "log"),
  my_family       = "gaussian",
  my_se           = "wcr"
)
.measures_for <- list(unemp  = c("wb_nat", "es_1574_act", "es_2064_pop", "es_2564_lfs"),
                      growth = "wb_growth")

.est_lab <- function(e) c(
  fe_twoway  = "two-way fixed effects (country + year)",
  fe_country = "country fixed effects")[[e]]
.se_lab <- "wild cluster bootstrap by country"

# The term for the predictor that the claim is about, as it appears in a
# formula: log(unemp) when the log form is chosen, else the bare variable name.
.main_term <- function(ch) {
  if (ch$my_predictor == "unemp" && ch$my_pred_form == "log") "log(unemp)"
  else ch$my_predictor
}
.copred_term <- function(ch) if (identical(ch$my_predictor, "unemp")) "growth" else "unemp"

# --- the shipped files -------------------------------------------------------
# Each file is read once and cached.
.cache <- new.env(parent = emptyenv())
.read_once <- function(name, path) {
  if (is.null(.cache[[name]])) {
    if (!file.exists(path))
      stop(sprintf("%s is missing – the workspace ships it under data/. Run from the workspace root folder.",
                   path), call. = FALSE)
    assign(name, readr::read_csv(path, show_col_types = FALSE, progress = FALSE), envir = .cache)
  }
  .cache[[name]]
}
.curve   <- function() .read_once("curve", "data/spec_curve.csv")
.options <- function() .read_once("options", "data/menu_options.csv")

# the sign matrix of the bootstrap, drawn once from its fixed seed
.signs <- function() {
  if (is.null(.cache$signs)) assign("signs", .wcr$wcr_signs("rademacher"), envir = .cache)
  .cache$signs
}

# --- validation --------------------------------------------------------------
# Two entry points into the same rules. .validate() takes a named list, which
# is how the exploratory try_spec() supplies its arguments; .check_choices()
# reads the fourteen my_* choices and claim_align out of the calling
# environment, which is how the declared flow reads the preregistration block.
# An option that is not on the curve stops with the reason from
# menu_options.csv, the page of the course that teaches it, and the options
# that stay on the curve.
.page_titles <- c(
  "companion/measurement.qmd"   = "The items behind the scales",
  "companion/stats-methods.qmd" = "The statistical methods behind the course",
  "companion/dags.qmd"          = "Causal graphs",
  "companion/multiverse.qmd"    = "The multiverse and its critics",
  "companion/weighting.qmd"     = "Weights",
  "companion/missingness.qmd"   = "Missing answers",
  "companion/data-sources.qmd"  = "Social science data sources",
  "results.qmd"                 = "Multiverse",
  "exercise/estimand-dial.qmd"  = "Decision map")
.page_ref <- function(path) {
  title <- if (path %in% names(.page_titles)) .page_titles[[path]] else path
  sprintf("%s, %s/%s", title, .course_url, sub("\\.qmd$", ".html", path))
}
.or_list <- function(x) {
  q <- paste0("\"", x, "\"")
  if (length(q) == 1) q else paste(paste(q[-length(q)], collapse = ", "), "or", q[length(q)])
}
# two older option codes that labels and calls may still use, mapped to the rows that hold their reasons
.legacy <- c("se:clustered" = "cr1", "weights:n_cy" = "n_cell")

.on_menu <- function(k, v) is.character(v) && length(v) == 1 && !is.na(v) && v %in% .menu[[k]]

.refuse <- function(k, v, ch, as_args) {
  shown <- function(a) if (as_args) a else paste0("my_", a)
  axis  <- sub("^my_", "", k)
  keep  <- if (k == "my_measure" && isTRUE(ch$my_predictor %in% names(.measures_for)))
    .measures_for[[ch$my_predictor]] else .menu[[k]]
  map   <- paste0("  Every decision and its options: ", .page_ref("exercise/estimand-dial.qmd"))
  if (!is.character(v) || length(v) != 1 || is.na(v))
    stop(sprintf("%s needs one option in quotes, for example %s = \"%s\".",
                 shown(axis), shown(axis), .menu[[k]][1]), call. = FALSE)
  look <- if (paste0(axis, ":", v) %in% names(.legacy)) .legacy[[paste0(axis, ":", v)]] else v
  opts <- .options()
  row  <- dplyr::filter(opts, .data$axis == .env$axis, .data$option == look, !.data$on_curve)
  off  <- dplyr::filter(opts, .data$axis == .env$axis, !.data$on_curve)$option
  msg <- if (nrow(row) == 1) c(
    sprintf("%s = \"%s\" (%s) is not on the curve of 864 specifications, so it has no result here.",
            shown(axis), v, row$label),
    paste0("  Why: ", row$reason),
    if (axis == "se")
      "  Inference is fixed: every result on the curve is tested by the wild cluster bootstrap by country, and the printout gives the conventional t and the CR2 p beside it.",
    paste0("  Where it is taught: ", .page_ref(row$taught_on)),
    map,
    sprintf("  To stay on the curve, choose %s = %s.", shown(axis), .or_list(keep))
  ) else c(
    sprintf("%s = \"%s\" is not an option on the menu.", shown(axis), v),
    sprintf("  On the curve: %s.", paste(.menu[[k]], collapse = " | ")),
    if (length(off) > 0)
      sprintf("  Off the curve, refused with the reason why: %s.", paste(off, collapse = " | ")),
    map
  )
  stop(paste(msg, collapse = "\n"), call. = FALSE)
}

.validate <- function(ch, as_args = FALSE) {
  shown <- function(a) if (as_args) a else paste0("my_", a)
  # the co-predictor may also be named by its variable, growth or unemp
  if (identical(ch$my_copredictor, .copred_term(ch)))
    ch <- utils::modifyList(ch, list(my_copredictor = "other"))
  bad <- Filter(\(k) !.on_menu(k, ch[[k]]), names(.menu))
  if (length(bad) > 0) .refuse(bad[1], ch[[bad[1]]], ch, as_args)
  if (ch$my_predictor == "growth" && ch$my_pred_form == "log")
    stop(sprintf("GDP growth enters raw only – it takes negative values, so its log is undefined. Use %s = \"raw\".",
                 shown("pred_form")), call. = FALSE)
  if (!ch$my_measure %in% .measures_for[[ch$my_predictor]])
    stop(sprintf("%s = \"%s\" is a series for %s; with %s = \"%s\" choose %s = %s.",
                 shown("measure"), ch$my_measure, setdiff(names(.measures_for), ch$my_predictor),
                 shown("predictor"), ch$my_predictor, shown("measure"),
                 .or_list(.measures_for[[ch$my_predictor]])), call. = FALSE)
  ch
}

.check_choices <- function() {
  need <- names(.menu)
  missing <- Filter(Negate(exists), need)
  if (length(missing) > 0)
    stop("Not declared yet: ", paste(missing, collapse = ", "),
         ". Fill in every choice in the preregistration cell and run it first.",
         call. = FALSE)
  if (!exists("claim_align"))
    stop("Also declare claim_align <- TRUE (the course convention) in the preregistration cell.",
         call. = FALSE)
  .validate(mget(need, inherits = TRUE))
}

# --- the data behind a specification -----------------------------------------
# The base panel supplies the cells and the World Bank series that the
# adjustment uses. The measurement panel supplies the outcome under the chosen
# item set, no-opinion rule and cell weighting, and the predictor series file
# the chosen published series. The cells are sorted by country and year, as
# in the build of the curve.
.build_data <- function(ch) {
  outcome <- .read_once("measurement", "data/teney_panel_measurement.csv") |>
    dplyr::filter(.data$construction == ch$my_construction, .data$item_set == ch$my_item_set,
                  .data$noop == ch$my_noop, .data$cell_weight == ch$my_cell_weight,
                  .data$outcome == ch$my_outcome) |>
    dplyr::select("cntry", "year", y = "value")
  series <- .read_once("predictors", "data/predictor_measures.csv") |>
    dplyr::filter(.data$measure == ch$my_measure) |>
    dplyr::select("cntry", "year", pred = "value")
  .read_once("panel", "data/EUframes_cy.csv") |>
    dplyr::select("cntry", "year", wb_unemp = "unemp", wb_growth = "growth") |>
    dplyr::inner_join(outcome, by = c("cntry", "year")) |>
    dplyr::inner_join(series, by = c("cntry", "year")) |>
    dplyr::mutate(x = if (ch$my_pred_form == "log") log(.data$pred) else .data$pred,
                  copred = if (ch$my_predictor == "unemp") .data$wb_growth else .data$wb_unemp) |>
    dplyr::arrange(.data$cntry, .data$year)
}

# The claim-alignment factor. The claim expects framing to rise with growth,
# so a growth slope is reported with its sign flipped; the negative framings,
# which are not on the curve, were reverse-coded the same way. The r_ca column
# of the curve is r times this factor.
.flip <- function(ch)
  (if (ch$my_outcome %in% .neg_framings) -1 else 1) *
  (if (ch$my_predictor == "growth") -1 else 1)

.key_of <- function(ch) paste(unlist(ch[paste0("my_", .axis_codes)]), collapse = "|")

.curve_row <- function(ch) {
  cv  <- .curve()
  hit <- cv[match(.key_of(ch), do.call(paste, c(unname(as.list(cv[.axis_codes])), sep = "|"))), ]
  if (is.na(hit$b))
    stop("That specification passed the checks but is not among the 864 on the curve. This should not happen; check that data/spec_curve.csv is the copy that came with the workspace.",
         call. = FALSE)
  hit
}

# --- the live fit --------------------------------------------------------------
# The conventional fit is fixest with country (and year) fixed effects; the
# bootstrap takes the same model as a design matrix with the fixed effects as
# dummies. The result has the form that the printout and the submission use:
# the conventional fit (b, se, t, df, r), the claim-aligned r, the two
# clustered tests, and the placement among the 864.
.result <- function(ch) {
  d   <- .build_data(ch)
  rhs <- paste(c("x", if (ch$my_copredictor == "other") "copred"), collapse = " + ")
  fe  <- c(fe_twoway = "cntry + year", fe_country = "cntry")[[ch$my_estimator]]
  m   <- fixest::feols(stats::as.formula(paste("y ~", rhs, "|", fe)), data = d, vcov = "iid")
  ct  <- fixest::coeftable(m)
  t   <- ct["x", "t value"]
  df  <- fixest::degrees_freedom(m, type = "resid")
  dummies <- c(fe_twoway = "factor(cntry) + factor(year)", fe_country = "factor(cntry)")[[ch$my_estimator]]
  X    <- stats::model.matrix(stats::as.formula(paste("y ~", rhs, "+", dummies)), data = d)
  boot <- .wcr$wcr_test(d$y, X, d$cntry, .signs(), term = "x")
  stored <- .curve_row(ch)
  if (abs(t - stored$t) > 1e-8 || abs(boot[["p"]] - stored$p_wcr) > 1e-9)
    warning(sprintf("The live fit (t = %.4f, bootstrap p = %.4f) differs from the curve shipped with the workspace (t = %.4f, p = %.4f). A file in data/ may have been edited; restore it from the course repository.",
                    t, boot[["p"]], stored$t, stored$p_wcr), call. = FALSE)
  r <- t / sqrt(t^2 + df)
  list(b = ct["x", "Estimate"], se = ct["x", "Std. Error"], t = t, df = df, n = stats::nobs(m),
       r = r, r_ca = .flip(ch) * r, p_cr2 = stored$p_cr2, p_wcr = boot[["p"]],
       placement = mean(.curve()$r_ca < stored$r_ca),
       source = "fitted live, with the bootstrap run here on 9,999 draws; the CR2 p is read from data/spec_curve.csv")
}

# --- the printout ----------------------------------------------------------------
# The one-line spec label that is parsed for the class chart: six of the
# Level 1 choices, then in brackets the scoring rule and the measurement and
# estimation choices, the inference rule last.
.spec_line <- function(ch) {
  neg <- ch$my_outcome %in% .neg_framings
  paste0(ch$my_outcome, if (neg) "_ca" else "", " ~ ", ch$my_predictor, "(", ch$my_pred_form, ")",
         if (ch$my_copredictor == "other") paste0(" + ", .copred_term(ch)) else "",
         " | ", ch$my_estimator, " | ", ch$my_sample, " | w:", ch$my_weights,
         " [", ch$my_construction, ", ", ch$my_noop, ", items:", ch$my_item_set, ", cells:",
         ch$my_cell_weight, ", ", ch$my_measure, " | ", ch$my_family, ", se:", ch$my_se, "]")
}

.fmt_p <- function(p) if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)

.class_line <- function(res) {
  below <- res$r_ca < 0
  sig   <- res$p_wcr < 0.05
  if (below && sig) "below zero and significant under the bootstrap (p < 0.05), in the direction of the claim"
  else if (below) "below zero, but not significant under the bootstrap (p >= 0.05)"
  else if (sig) "above zero and significant under the bootstrap, against the direction of the claim"
  else "above zero and not significant under the bootstrap"
}

.print_result <- function(ch, res, lead = NULL, aligned = TRUE) {
  fl <- if (aligned) .flip(ch) else 1
  writeLines(c(
    lead,
    paste0("  ", .spec_line(ch)),
    sprintf("  b = %.5f   se = %.5f   t = %.3f on %d df (conventional standard error)",
            fl * res$b, res$se, fl * res$t, round(res$df)),
    sprintf("  partial r = %.3f, %s", if (aligned) res$r_ca else res$r,
            if (aligned) "claim-aligned (below zero is the direction of the claim)"
            else "on the original scale of the outcome"),
    sprintf("  p %s with CR2 errors clustered by country   p %s by the wild cluster bootstrap",
            .fmt_p(res$p_cr2), .fmt_p(res$p_wcr)),
    paste0(if (aligned) "  Class: " else "  Class, on the claim-aligned scale: ", .class_line(res)),
    sprintf("  Placement: %.0f%% of the 864 specifications on the curve have a more negative claim-aligned r",
            100 * res$placement),
    if (aligned && ch$my_predictor == "growth")
      "  (growth: b, t and r are shown with their signs flipped, because the claim expects framing to rise with growth)",
    if (!aligned)
      "  (claim_align = FALSE: shown on the original scale; the class chart receives the claim-aligned values)",
    paste0("  (", res$source, ")")))
  invisible(res)
}

# --- Task 4: the exploratory helpers -------------------------------------------
.tried <- new.env(parent = emptyenv())
assign("log", NULL, envir = .tried)
assign("last", NULL, envir = .tried)

try_spec <- function(outcome = "mcosmo", predictor = "unemp", copredictor = "none",
                     estimator = "fe_twoway", sample = "all", weights = "none",
                     construction = "share", noop = "zero", item_set = "all",
                     cell_weight = "unweighted", measure = NULL,
                     pred_form = "raw", family = "gaussian", se = "wcr") {
  if (is.null(measure)) measure <- if (identical(predictor, "growth")) "wb_growth" else "wb_nat"
  ch <- .validate(list(my_outcome = outcome, my_predictor = predictor, my_copredictor = copredictor,
                       my_estimator = estimator, my_sample = sample, my_weights = weights,
                       my_construction = construction, my_noop = noop, my_item_set = item_set,
                       my_cell_weight = cell_weight, my_measure = measure,
                       my_pred_form = pred_form, my_family = family, my_se = se), as_args = TRUE)
  res <- .result(ch)
  .print_result(ch, res, lead = "")
  fl  <- .flip(ch)
  row <- data.frame(stats::setNames(ch, sub("^my_", "", names(ch))),
                    b = fl * res$b, t = fl * res$t, r_aligned = res$r_ca, p_boot = res$p_wcr,
                    stringsAsFactors = FALSE)
  assign("log", dplyr::bind_rows(.tried$log, row), envir = .tried)
  assign("last", list(ch = ch, res = res), envir = .tried)
  invisible(row)
}

# The running log of everything try_spec() has fitted. Only the decisions that
# varied are shown; the constant ones are those of the reproduced
# specification or of every call.
my_experiments <- function() {
  d <- .tried$log
  if (is.null(d)) {
    writeLines("Nothing tried yet – call try_spec() at least once.")
    return(invisible(NULL))
  }
  axes   <- sub("^my_", "", names(.menu))
  varied <- names(Filter(\(x) length(unique(x)) > 1, d[axes]))
  show <- d |>
    dplyr::select(dplyr::all_of(c(varied, "b", "t", "r_aligned", "p_boot"))) |>
    dplyr::mutate(b = signif(.data$b, 3), t = round(.data$t, 2),
                  r_aligned = round(.data$r_aligned, 3), p_boot = round(.data$p_boot, 3))
  writeLines(sprintf("Specifications you have tried (%d):\n", nrow(d)))
  print(show, row.names = FALSE)
  writeLines(sprintf("\nSpread of claim-aligned r: %.3f to %.3f  |  below zero and significant under the bootstrap: %d of %d",
                     min(d$r_aligned), max(d$r_aligned),
                     sum(d$r_aligned < 0 & d$p_boot < 0.05), nrow(d)))
  invisible(show)
}

submit_exploration <- function(nickname = NULL) {
  if (is.null(.tried$last))
    stop("Nothing to submit – call try_spec() first, ending with the specification you want on the chart.",
         call. = FALSE)
  .submit(.tried$last$ch, .tried$last$res, "explore",
          "Your exploratory submission (claim-aligned, the last specification you tried):",
          nickname)
}

# --- Task 5: the preregistration helpers ---------------------------------------

prereg <- function() {
  ch <- .check_choices()
  writeLines(c(
    "DECLARED – your preregistered specification, fixed before the model is fitted:",
    "",
    "  The question (Level 1)",
    paste("    outcome    :", ch$my_outcome),
    paste0("    predictor  : ", ch$my_predictor,
           if (ch$my_predictor == "growth") " (claim-expected sign flipped automatically)"),
    paste("    adjusted   :", if (ch$my_copredictor == "other") .copred_term(ch) else "nothing"),
    paste("    estimator  :", .est_lab(ch$my_estimator)),
    paste("    sample     :", ch$my_sample, "   whose average:", ch$my_weights),
    paste("    scored as  :", ch$my_construction),
    "  The measurement (Level 2)",
    paste("    no-opinion rule:", ch$my_noop, "  items:", ch$my_item_set),
    paste("    cells:", ch$my_cell_weight, "  series:", ch$my_measure),
    "  The estimation (Level 3)",
    paste("    form:", ch$my_pred_form, "  family:", ch$my_family, "  inference:", .se_lab),
    paste("  claim_align:", isTRUE(claim_align)),
    "",
    "build_model(), run_spec() and report_result() read these choices from",
    "here. Rendered into your report, this cell is your preregistration, and",
    "it sits above the cells that fit and submit this specification."))
  invisible(ch)
}

build_model <- function(formula, estimator) {
  ch <- .check_choices()
  if (missing(formula) || !inherits(formula, "formula"))
    stop("The first argument must be a formula: outcome ~ predictor terms, unquoted.",
         call. = FALSE)
  lhs <- all.vars(formula[[2]])
  if (!identical(lhs, ch$my_outcome))
    stop(sprintf("The outcome in your formula is '%s' but you preregistered '%s'. The preregistration is binding, so change the formula to match it.",
                 deparse(formula[[2]]), ch$my_outcome), call. = FALSE)
  want <- c(.main_term(ch), if (ch$my_copredictor == "other") .copred_term(ch))
  got  <- attr(stats::terms(formula), "term.labels")
  if (!setequal(got, want))
    stop(sprintf("Right-hand side mismatch: you wrote { %s } but your preregistration implies { %s }.%s",
                 paste(got, collapse = ", "), paste(want, collapse = ", "),
                 if (.main_term(ch) == "log(unemp)" && !"log(unemp)" %in% got)
                   " You preregistered the log form – write log(unemp) in the formula." else ""),
         call. = FALSE)
  if (missing(estimator) || !identical(estimator, ch$my_estimator))
    stop(sprintf("estimator = \"%s\" does not match your preregistered \"%s\".",
                 if (missing(estimator)) "" else estimator, ch$my_estimator), call. = FALSE)
  writeLines(c("Model accepted – it matches your preregistration:",
               paste0("  ", deparse(formula), ", estimated by ", .est_lab(estimator)),
               "  Run it with run_spec(my_model)."))
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
  res <- .result(ch)
  .print_result(ch, res, "Your preregistered specification, fitted live:",
                aligned = isTRUE(claim_align))
}

# --- Submission tags -------------------------------------------------------------
# Both submissions of a participant carry one short tag inside the spec label,
# "explore: [amber-otter-42] …", so that the participant can find their own
# circles on the class chart. The tag rides in the free-text label because the
# form and its fields are frozen infrastructure. It is drawn once and kept in
# .class-tag at the root of the workspace, so the explored and the declared
# submission share it across sessions; a nickname passed to either submission
# function replaces it. Base R throughout.
.tag_words <- list(
  adj  = c("amber", "azure", "bright", "calm", "coral", "crisp", "dusky", "early",
           "fair", "gentle", "golden", "hazel", "ivory", "jade", "keen", "lively",
           "lunar", "misty", "noble", "olive", "pale", "quiet", "rapid", "rosy",
           "russet", "silver", "snowy", "solar", "sunny", "swift", "velvet", "vivid"),
  noun = c("otter", "heron", "lynx", "badger", "finch", "hare", "ibex", "kestrel",
           "lark", "marten", "newt", "owl", "puffin", "quail", "raven", "seal",
           "stoat", "tern", "vole", "wren", "bison", "crane", "dove", "egret",
           "fox", "gecko", "koala", "lemur", "moose", "panda", "robin", "swan")
)
.tag_file <- ".class-tag"

# A fresh tag, drawn without touching the random-number stream of the session,
# so that a participant's set.seed() keeps reproducing the same draws.
.new_tag <- function() {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()))
  set.seed(as.integer((as.numeric(Sys.time()) * 1000) %% 1e9) + Sys.getpid() %% 1e5)
  paste(sample(.tag_words$adj, 1), sample(.tag_words$noun, 1), sample(10:99, 1), sep = "-")
}

# A nickname reduced to lower-case ASCII letters, digits and single hyphens,
# at most 24 characters – the only form that the class chart parses.
.clean_tag <- function(x) {
  # iconv on Windows turns these into "?": the Romanian comma-below letters and the eszett
  x <- gsub("ß", "ss", chartr("ȘșȚț", "SsTt", trimws(as.character(x)[1])))
  ascii <- suppressWarnings(iconv(x, to = "ASCII//TRANSLIT"))
  if (!is.na(ascii)) x <- ascii
  x <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(x)))
  x <- gsub("-+$", "", substr(x, 1, 24))
  if (is.na(x) || !nzchar(x))
    stop("A nickname needs at least one letter or digit, for example nickname = \"blue-heron\".",
         call. = FALSE)
  x
}

# The tag of this workspace: the nickname if one is given, otherwise the stored
# tag, otherwise a fresh one. Returns the tag and the one it replaced, if any.
.class_tag <- function(nickname = NULL) {
  stored <- if (file.exists(.tag_file))
    tryCatch(.clean_tag(readLines(.tag_file, warn = FALSE)[1]), error = function(e) NULL)
  tag <- if (!is.null(nickname)) .clean_tag(nickname) else if (!is.null(stored)) stored else .new_tag()
  if (!identical(tag, stored)) writeLines(tag, .tag_file)
  list(tag = tag, replaced = if (!is.null(stored) && !identical(tag, stored)) stored)
}

# One submission, used by both phases. The phase and the tag are prefixed to
# the free-text spec label, and all fourteen decisions are written into that
# label. The form, its fields and the collection pipeline are frozen
# infrastructure and do not change. The numbers are those of the conventional
# fit, claim-aligned.
.submit <- function(ch, res, phase, lead, nickname = NULL) {
  fl <- .flip(ch)
  b <- fl * res$b; tv <- fl * res$t; r <- res$r_ca
  tg <- .class_tag(nickname)
  spec_label <- paste0(phase, ": [", tg$tag, "] ", .spec_line(ch))

  FORM_ID <- "1FAIpQLSezoAEOnZUfP4pkfN28_XtEBwSyc2RLsXB-h8RPSegF7_t76A"
  field <- c(spec = "entry.832496914", b = "entry.1656311800", se = "entry.1337458262",
             t = "entry.1940779437", df = "entry.1682711955", n = "entry.1586392724",
             r = "entry.1660273001")
  value <- c(spec = spec_label, b = sprintf("%.5f", b),
             se = sprintf("%.5f", res$se), t = sprintf("%.3f", tv),
             df = as.character(round(res$df)), n = as.character(res$n),
             r = sprintf("%.3f", r))
  query <- paste(vapply(names(field), function(k) # r-style: keep – URLencode() takes one string, and purrr is not a dependency of the workspace
    paste0(field[[k]], "=", utils::URLencode(value[[k]], reserved = TRUE)),
    character(1)), collapse = "&")
  url <- sprintf("https://docs.google.com/forms/d/e/%s/viewform?usp=pp_url&%s", FORM_ID, query)

  writeLines(c(
    lead,
    "",
    paste("  spec:", spec_label),
    sprintf("  b = %.5f | se = %.5f | t = %.3f | df = %d | n = %d | r = %.3f",
            b, res$se, tv, round(res$df), res$n, r),
    paste0("  Class: ", .class_line(res)),
    "",
    "Open the link below (or copy the URL) – the form opens with every field",
    "pre-filled; just press Send.",
    "",
    url,
    "",
    sprintf("Your marks on the class chart carry the tag %s%s.", tg$tag,
            if (!is.null(tg$replaced)) sprintf(" (it replaces %s)", tg$replaced) else ""),
    "Type it into the my_tag line of the live cell on the Multiverse page to",
    "highlight them. Both of your submissions share it. To choose your own, pass",
    "nickname = \"...\" – pick a nickname rather than your name, because the chart",
    "is public."))
  invisible(list(spec = spec_label, tag = tg$tag, b = b, se = res$se, statistic = tv,
                 df = res$df, n = res$n, r = r, url = url))
}

report_result <- function(nickname = NULL) {
  ch <- .check_choices()
  .submit(ch, .result(ch), "declared",
          "Your declared submission (claim-aligned, straight from your preregistration):",
          nickname)
}
