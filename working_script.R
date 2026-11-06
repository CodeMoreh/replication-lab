##############################################################################
# Title:        Working script – the EU-frames robustness exercise, plain R
# Purpose:      Tasks 2 to 5 of index.qmd, as a script you can step through in
#               the console: look at the panel, reproduce one of the three
#               results recorded in Multi100 that rest on aggregate data,
#               explore the specification menu, then
#               preregister one specification and fit and report it. Every fit
#               is live, and each is tested by CR2 errors clustered by country
#               with Satterthwaite df, as on the curve of 864 specifications.
# Reads:        data/EUframes_cy.csv   (the EU-frames country-year panel);
#               data/analyst_KQXUE.csv and data/analyst_018OL.csv (the data of
#               two of the reproductions of Task 3);
#               through R/spec_tools.R, data/teney_panel_measurement.csv and
#               data/predictor_measures.csv (the measurement variants behind
#               the menu), data/spec_curve.csv (the curve, for the placement
#               and the check of each fit) and data/menu_options.csv (the reason for every
#               option that is not on the curve)
# Writes:       – (prints results to the console; the submit helpers print a
#               pre-filled form link)
# Author:       Chris Moreh
# Last updated: 2026-10-27
##############################################################################

# From Task 4 on, the code works on the country-year panel of analyst C6HJR,
# the facilitator, adapted so it runs from the committed CSV alone. The
# upstream data-preparation step, which assembles this panel from the raw GESIS
# Eurobarometer waves, is the read-only exhibit prep/prep_exhibit.R. It needs
# raw files that are not shipped here. Every function is called with the name of its package, so that
# you can see where each one comes from.

# The one working object, and the helpers that explore, preregister, build,
# run and report a specification.
euframes <- readr::read_csv("data/EUframes_cy.csv", show_col_types = FALSE)
source("R/spec_tools.R")

## ---- Task 2: meet the panel -----------------------------------------------

# One row per country-year: 27 EU member states x 10 years, 2004–2013.
euframes |>
  datawizard::data_summary(
    n_rows      = length(cntry),
    n_countries = insight::n_unique(cntry),
    n_years     = insight::n_unique(year),
    year_min    = min(year),
    year_max    = max(year)
  )

## ---- Task 3: reproduce a recorded result ----------------------------------

# Choose one of the three analysts whose results rest on aggregate data and
# step through its block; the codebooks in data/ say how each file was built.

# C6HJR – two-way (country + year) fixed effects on the course panel, with
# unemployment centred on its grand mean, which shifts only the intercept. On
# record: t = -3.804 on 233 df.
euframes <- euframes |>
  datawizard::data_modify(unemp_c = unemp - mean(unemp))
m1 <- fixest::feols(mcosmo ~ unemp_c | cntry + year, data = euframes, vcov = "iid")
print(parameters::model_parameters(m1), digits = 5)

# KQXUE – country fixed effects only, as xtreg cosmo unemployment, fe in Stata,
# on its own panel (EB 62.0 appended twice, unemployment as a share of the
# population aged 20-64). On record: t = -7.169 on 242 df.
kqxue <- readr::read_csv("data/analyst_KQXUE.csv", show_col_types = FALSE)
m_kqxue <- fixest::feols(cosmo ~ unemp | cntry, data = kqxue, vcov = "iid")
print(parameters::model_parameters(m_kqxue), digits = 5)

# 018OL – random intercepts for country, year and wave on 420 cells, with the
# Satterthwaite df of lmerTest; the analyst rescaled the outcome to 0-1, which
# leaves t unchanged. On record: t = -1.206 on 396.9 df.
o18ol <- readr::read_csv("data/analyst_018OL.csv", show_col_types = FALSE) |>
  datawizard::data_modify(pos01 = datawizard::rescale(pos, to = c(0, 1)))
m_018ol <- lmerTest::lmer(pos01 ~ unemp + (1 | cntry) + (1 | year) + (1 | wave), data = o18ol)
print(parameters::model_parameters(m_018ol, effects = "fixed", ci_method = "satterthwaite"), digits = 5)

## ---- Task 4: work the menu ------------------------------------------------

# The specification that you submit with submit_exploration() at the end of
# this task is labelled exploratory on the class chart. The one labelled
# declared is fixed later, in the Task 5 preregistration, before it is fitted.
# Both carry the tag that submit_exploration() prints and keeps in .class-tag;
# submit_exploration(nickname = "...") sets your own (a nickname, not your name).
# try_spec() takes one argument per decision of the menu, and every one
# defaults to the reproduced specification, the model of C6HJR in Task 3, so
# changing one thing means changing one argument. The arguments for decisions
# about the question change the question, the measurement arguments select
# which version of the panel is fitted, and pred_form, a decision about the
# model, sets the form of the predictor. Each call prints the conventional
# fit, the CR2 p-value with its Satterthwaite df, the class of the result and
# its placement among the 864 specifications on the curve.
try_spec()                              # the reproduced specification itself
try_spec(outcome     = "mpos")          # the composite of both positive framings
try_spec(estimator   = "fe_country")    # country effects only: changes common to all of Europe stay in
try_spec(predictor   = "growth")        # GDP growth instead of unemployment, in the World Bank series
try_spec(copredictor = "other")         # adjusted for the other economic measure, here GDP growth
try_spec(noop        = "drop")          # leave out the respondents who mentioned no frame
try_spec(measure     = "es_2064_pop")   # a Eurostat unemployment series instead of the World Bank one

# Options that are not on the curve are refused with the reason why and the
# page of the course site that teaches them; try() prints the refusal and lets
# the rest of the script run.
try(try_spec(estimator = "pooled_cl"))  # pooled OLS
try(try_spec(sample = "no_bailout"))    # without the eight bailout countries

# The running log of everything you have tried, a small multiverse of your
# own. Only the decisions that varied are printed.
my_experiments()

# Send the most recent one to the class chart, labelled as exploration.
submit_exploration()

## ---- Task 5: preregister one specification --------------------------------

# One choice on each of the fourteen decisions. The seven decisions about the
# question fix what you are asking, the four measurement decisions fix how that
# question is measured, and the three decisions about the model fix how it is
# estimated. The curve keeps two or more
# options on nine decisions and one option on the other five; an option that
# is not on the curve stops prereg() with the reason why. claim_align = TRUE
# is the course convention, applied automatically: the claim expects framing
# to rise with GDP growth, so the sign of a growth slope is flipped, and
# 'supports the claim' is a negative sign.

# The question (each of these changes what is being estimated)
my_outcome      <- "mcosmo"       # mcosmo | mutil | mpos   (the positive framings)
my_predictor    <- "unemp"        # unemp | growth    (the measure that the claim is about)
my_copredictor  <- "none"         # none | other      (the other economic measure, adjusted for)
my_estimator    <- "fe_twoway"    # fe_twoway | fe_country   (the comparison)
my_sample       <- "all"          # all    (the 27 member states, 2004–2013)
my_weights      <- "none"         # none   (every country-year counts once)
my_construction <- "share"        # share  (the share of a respondent's own mentions)

# Measurement (the same question, put to the data differently)
my_noop         <- "zero"         # zero | drop   (respondents who mentioned no frame at all)
my_item_set     <- "all"          # all | un   (with or without the "unemployment" item)
my_cell_weight  <- "unweighted"   # unweighted | design   (cells as interviewed, or weighted)
my_measure      <- "wb_nat"       # unemp:  wb_nat | es_1574_act | es_2064_pop | es_2564_lfs
                                  # growth: wb_growth

# The model (the same question and measures, another fit)
my_pred_form    <- "raw"          # raw | log   (growth: raw only; centring or z-scaling
                                  #              would leave t, r and p unchanged)
my_family       <- "gaussian"     # gaussian   (a linear model of the cell means)
my_se           <- "cr2"          # cr2   (fixed: CR2 errors clustered by country)

claim_align     <- TRUE           # the course convention, applied automatically

prereg()   # checks every choice against the menu and prints your preregistration

## ---- Task 5, continued: build, run and report -----------------------------

# Translate the preregistered specification into model syntax yourself: the
# outcome, the right-hand side, the estimator code. build_model() refuses
# anything that does not match the preregistration. run_spec() then expands
# the estimator code into the real fixest call, fitted on the version of the
# panel that the measurement choices select, and computes its CR2 test.
my_model <- build_model(mcosmo ~ unemp, estimator = "fe_twoway")
run_spec(my_model)

# report_result() needs no arguments, works from the preregistration and
# reuses the tag from Task 4. It refits the model, builds the spec label, and
# always submits on the claim-aligned scale on which the chart is drawn. Open
# the printed link and press Send.
report_result()
