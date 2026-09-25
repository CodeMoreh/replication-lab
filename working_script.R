# ########################################################################### #
# Title:        Working script – the EU-frames robustness exercise, plain R
# Purpose:      Tasks 2 to 5 of index.qmd, as a script you can step through in
#               the console: look at the panel, reproduce the result recorded
#               in Multi100 (t = -3.804), explore the specification menu, then
#               preregister one specification and fit and report it. Every fit
#               is live, on `euframes`.
# Reads:        data/EUframes_cy.csv   (the EU-frames country-year panel);
#               through R/spec_tools.R, data/teney_panel_measurement.csv,
#               data/teney_panel_weights.csv, data/predictor_measures.csv and
#               data/spec_grid_mi.csv (the measurement variants behind the menu)
# Writes:       – (prints results to the console; the submit helpers print a
#               pre-filled form link)
# Author:       Chris Moreh
# Last updated: 2026-08-25
# ########################################################################### #

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
# try_spec() takes one argument per axis of the fourteen-axis menu, and every
# one defaults to the baseline that you have just reproduced, so changing one
# thing means changing one argument.
# Level 1 arguments change the question, Level 2 arguments select which
# version of the panel is fitted, and Level 3 arguments change how the model
# is estimated.
try_spec()                          # the baseline itself
try_spec(outcome   = "mpos")         # all positive framings, cosmopolitan and utilitarian together
try_spec(estimator = "pooled_cl")    # pooled: differences between countries are not absorbed
try_spec(sample    = "no_bailout")   # drop the assistance-programme countries
try_spec(weights   = "pop")          # let each country count by its adult population
try_spec(noop      = "drop")         # leave out the respondents who mentioned no frame
try_spec(measure   = "es_2064_pop")  # a Eurostat unemployment series instead of the World Bank one

# The running log of everything you have tried, a small multiverse of your
# own. Only the axes that varied are printed.
my_experiments()

# Send the most recent one to the class chart, labelled as exploration.
submit_exploration()

## ---- Task 5: preregister one specification --------------------------------

# One choice on each of the fourteen axes. The seven Level 1 choices fix the
# question, the four Level 2 choices fix how it is measured, and the three
# Level 3 choices fix how the model is estimated. claim_align = TRUE is the
# course convention, applied automatically: negative framings are
# reverse-coded (1 - y) and the claim-expected sign for growth is flipped, so
# 'supports the claim' is a negative sign.

# Level 1 – Estimands (each of these changes what is being estimated)
my_outcome      <- "mcosmo"       # mcosmo | mutil | mpos    (positive framings)
                                  # mcomm  | mlib  | mneg    (negative framings)
my_predictor    <- "unemp"        # unemp | growth    (the measure that the claim is about)
my_copredictor  <- "none"         # none | other      (the other macro measure, adjusted for)
my_estimator    <- "fe_twoway"    # fe_twoway | fe_country | re | pooled_cl
my_sample       <- "all"          # all | y2004_2008 | y2009_2013 | no_bailout | no_GR_ES
my_weights      <- "none"         # none | n_cell | n_eff | pop   (re is never weighted)
my_construction <- "share"        # share | count | rate   (share of own mentions, count of the
                                  #   items in the set, or count per item in the set)

# Level 2 – Measurement (the same question, put to the data differently)
my_noop         <- "zero"         # zero | drop | split | mi_mice | mi_amelia
                                  #   (respondents who mentioned no frame at all)
my_item_set     <- "all"          # all | un   (with or without the "unemployment" item)
my_cell_weight  <- "unweighted"   # unweighted | design   (cells as interviewed, or design-weighted)
my_measure      <- "wb_nat"       # unemp:  wb_nat | es_1574_act | es_2064_pop | es_2564_lfs | es_1574_tips
                                  # growth: wb_growth | es_growth

# Level 3 – Estimation (the same question and measures, another fit)
my_pred_form    <- "raw"          # raw | log   (growth: raw only; centring or z-scaling
                                  #              would leave t, r and p unchanged)
my_family       <- "gaussian"     # gaussian | beta   (logit link on the cell means; beta
                                  #                    reports z; standard measurement only)
my_se           <- "conventional" # conventional | clustered   (clustered by country)

claim_align     <- TRUE           # the course convention, applied automatically

prereg()   # checks every choice against the menu and prints your preregistration

## ---- Task 5, continued: build, run and report -----------------------------

# Translate the preregistered specification into model syntax yourself: the
# outcome, the right-hand side, the estimator code. build_model() refuses
# anything that does not match the preregistration. run_spec() then expands the
# estimator, family, sample and weights into the real fixest / plm / betareg /
# glmmTMB call, fitted on the version of the panel that the measurement
# choices select.
my_model <- build_model(mcosmo ~ unemp, estimator = "fe_twoway")
run_spec(my_model)


# report_result() needs no arguments, works from the preregistration and reuses
# the tag from Task 4. It
# refits the model, builds the spec label, and always submits on the
# claim-aligned scale on which the chart is drawn. Open the printed link and
# press Send.
report_result()
