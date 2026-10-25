##############################################################################
# Title:        Working script – the EU-frames robustness exercise, plain R
# Purpose:      Tasks 2 to 5 of index.qmd, as a script you can step through in
#               the console: look at the panel, reproduce the result recorded
#               in Multi100 (t = -3.804), explore the specification menu, then
#               preregister one specification and fit and report it. Every fit
#               is live, and each is tested by the wild cluster bootstrap by
#               country, as on the curve of 864 specifications.
# Reads:        data/EUframes_cy.csv   (the EU-frames country-year panel);
#               through R/spec_tools.R, data/teney_panel_measurement.csv and
#               data/predictor_measures.csv (the measurement variants behind
#               the menu), data/spec_curve.csv (the curve, for the CR2 p and
#               the placement) and data/menu_options.csv (the reason for every
#               option that is not on the curve)
# Writes:       – (prints results to the console; the submit helpers print a
#               pre-filled form link)
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

# The code you work through is analyst C6HJR's own submitted analysis, adapted
# so it runs from the committed CSV alone. The upstream data-preparation step,
# which assembles this panel from the raw GESIS Eurobarometer waves, is the
# read-only exhibit prep/prep_exhibit.R. It needs raw files that are not
# shipped here.

library(dplyr)
library(readr)
library(fixest)
library(broom)

# The one working object, and the helpers that explore, preregister, build,
# run and report a specification.
euframes <- read_csv("data/EUframes_cy.csv", show_col_types = FALSE)
source("R/spec_tools.R")

## ---- Task 2: meet the panel -----------------------------------------------

# One row per country-year: 27 EU member states x 10 years, 2004–2013.
euframes |>
  summarise(
    n_rows      = n(),
    n_countries = n_distinct(cntry),
    n_years     = n_distinct(year),
    year_min    = min(year),
    year_max    = max(year)
  )

## ---- Task 3: reproduce the recorded result --------------------------------

# Grand-mean centred unemployment, which is the transformation that the
# constrained specification names. With two-way fixed effects this shifts only
# the intercept and leaves the slope and its t unchanged.
euframes <- euframes |>
  mutate(unemp_c = unemp - mean(unemp))

# Two-way (country + year) fixed effects – the constrained Multi100 Task-2
# model. The coefficient is -0.00347, t = -3.804 on 233 df, which is the value
# recorded for analyst C6HJR in the Multi100 dataset, reproduced from the
# published data and the published model.
m1 <- feols(mcosmo ~ unemp_c | cntry + year, data = euframes)
tidy(m1)

## ---- Task 4: work the menu ------------------------------------------------

# The specification that you submit with submit_exploration() at the end of
# this task is labelled exploratory on the class chart. The one labelled
# declared is fixed later, in the Task 5 preregistration, before it is fitted.
# Both carry the tag that submit_exploration() prints and keeps in .class-tag;
# submit_exploration(nickname = "...") sets your own (a nickname, not your name).
# try_spec() takes one argument per decision of the menu, and every one
# defaults to the specification that you have just reproduced, so changing
# one thing means changing one argument. Level 1 arguments change the
# question, Level 2 arguments select which version of the panel is fitted,
# and the Level 3 argument sets the form of the predictor. Each call prints the
# conventional fit, the CR2 and bootstrap p-values, the class of the result and
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

# One choice on each of the fourteen decisions. The seven Level 1 choices fix
# the question, the four Level 2 choices fix how it is measured, and the three
# Level 3 choices fix how the model is estimated. The curve keeps two or more
# options on nine decisions and one option on the other five; an option that
# is not on the curve stops prereg() with the reason why. claim_align = TRUE
# is the course convention, applied automatically: the claim expects framing
# to rise with GDP growth, so the sign of a growth slope is flipped, and
# 'supports the claim' is a negative sign.

# Level 1 – Estimands (each of these changes what is being estimated)
my_outcome      <- "mcosmo"       # mcosmo | mutil | mpos   (the positive framings)
my_predictor    <- "unemp"        # unemp | growth    (the measure that the claim is about)
my_copredictor  <- "none"         # none | other      (the other economic measure, adjusted for)
my_estimator    <- "fe_twoway"    # fe_twoway | fe_country
my_sample       <- "all"          # all    (the 27 member states, 2004–2013)
my_weights      <- "none"         # none   (every country-year counts once)
my_construction <- "share"        # share  (the share of a respondent's own mentions)

# Level 2 – Measurement (the same question, put to the data differently)
my_noop         <- "zero"         # zero | drop   (respondents who mentioned no frame at all)
my_item_set     <- "all"          # all | un   (with or without the "unemployment" item)
my_cell_weight  <- "unweighted"   # unweighted | design   (cells as interviewed, or weighted)
my_measure      <- "wb_nat"       # unemp:  wb_nat | es_1574_act | es_2064_pop | es_2564_lfs
                                  # growth: wb_growth

# Level 3 – Estimation (the same question and measures, another fit)
my_pred_form    <- "raw"          # raw | log   (growth: raw only; centring or z-scaling
                                  #              would leave t, r and p unchanged)
my_family       <- "gaussian"     # gaussian   (a linear model of the cell means)
my_se           <- "wcr"          # wcr   (fixed: the wild cluster bootstrap by country)

claim_align     <- TRUE           # the course convention, applied automatically

prereg()   # checks every choice against the menu and prints your preregistration

## ---- Task 5, continued: build, run and report -----------------------------

# Translate the preregistered specification into model syntax yourself: the
# outcome, the right-hand side, the estimator code. build_model() refuses
# anything that does not match the preregistration. run_spec() then expands
# the estimator code into the real fixest call, fitted on the version of the
# panel that the measurement choices select, and runs the bootstrap on it.
my_model <- build_model(mcosmo ~ unemp, estimator = "fe_twoway")
run_spec(my_model)

# report_result() needs no arguments, works from the preregistration and
# reuses the tag from Task 4. It refits the model, builds the spec label, and
# always submits on the claim-aligned scale on which the chart is drawn. Open
# the printed link and press Send.
report_result()
