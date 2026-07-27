# ########################################################################### #
# Title:        Working script – the EU-frames robustness exercise, plain R
# Purpose:      The same seven tasks as index.qmd, as a script you can step
#               through in the console: reproduce the recorded result on the
#               corrected panel (t = -3.804), then preregister one specification
#               and fit and report it. Every fit is live, on `euframes`.
# Reads:        data/EUframes_cy.csv   (the corrected EU-frames country-year panel)
# Writes:       – (prints results to the console; report_result() prints a link)
# Author:       Chris Moreh
# Last updated: 2026-07-19
# ########################################################################### #

# The code you work through is analyst C6HJR's own submitted analysis, adapted
# so it runs from the committed CSV alone. The upstream data-preparation step –
# which assembles this panel from the raw GESIS Eurobarometer waves – is the
# read-only exhibit prep/prep_exhibit.R; it needs raw files not shipped here.

library(dplyr)
library(readr)
library(fixest)
library(broom)

# The one working object, and the preregister -> build -> run -> report helpers.
euframes <- read_csv("data/EUframes_cy.csv", show_col_types = FALSE)
source("R/spec_tools.R")

## ---- Task 2: look at the panel --------------------------------------------

# One row per country-year: 27 EU member states x 10 years, 2004-2013.
euframes |>
  summarise(
    n_rows      = n(),
    n_countries = n_distinct(cntry),
    n_years     = n_distinct(year),
    year_min    = min(year),
    year_max    = max(year)
  )

## ---- Task 3: reproduce the recorded result --------------------------------

# Grand-mean centred unemployment, as the constrained specification names. With
# two-way fixed effects this shifts the intercept, not the slope or the t.
euframes <- euframes |>
  mutate(unemp_c = unemp - mean(unemp))

# Two-way (country + year) fixed effects – the constrained Multi100 Task-2
# model. The coefficient is -0.00347, t = -3.804 on 233 df: the value on record
# for analyst C6HJR, reproduced from the published data and the published model.
m1 <- feols(mcosmo ~ unemp_c | cntry + year, data = euframes)
tidy(m1)

# A second number sits in this analysis's public trail: t = -3.853, from the
# first submitted pipeline, which read one Eurobarometer wave twice and so
# duplicated 2008 into 2009. The analyst caught and corrected it during the
# project; the corrected build is the EUframes_cy.csv loaded above. forensics.R
# lays the two builds side by side if you want to see the difference yourself.

## ---- Task 5: preregister one specification --------------------------------

# One choice on each axis of the menu – every axis live, including the outcome
# family and the predictor. claim_align = TRUE is the day's convention, applied
# automatically: negative framings are reverse-coded (1 - y) and growth's
# claim-expected sign is flipped, so 'supports the claim' is a negative sign.

my_outcome     <- "mcosmo"     # mcosmo | mutil | mpos (positive) | mcomm | mlib | mneg (negative)
my_family      <- "gaussian"   # gaussian | beta (logit-link on the cell means; beta reports z)
my_predictor   <- "unemp"      # unemp | growth (the claim-carrying measure)
my_pred_form   <- "raw"        # raw | log (growth: raw only)
my_copredictor <- "none"       # none | growth (beside unemp) | unemp (beside growth)
my_estimator   <- "fe_twoway"  # fe_twoway | fe_country | re | pooled_cl
my_sample      <- "all"        # all | y2004_2008 | y2009_2013 | no_bailout | no_GR_ES
my_weights     <- "none"       # none | n_cy (re is never weighted)
claim_align    <- TRUE         # the day's convention, applied automatically

prereg()   # checks every choice against the menu and records the declaration

## ---- Task 6: build and run the specification ------------------------------

# Translate the declaration into model syntax yourself: the outcome, the
# right-hand side, the estimator code. build_model() refuses anything that does
# not match the preregistration; run_spec() then expands the estimator, family,
# sample and weights into the real fixest / plm / betareg / glmmTMB call.
my_model <- build_model(mcosmo ~ unemp, estimator = "fe_twoway")
run_spec(my_model)

## ---- Task 7: report to the class chart ------------------------------------

# Argument-free and prereg-driven: it refits, builds the spec label, and always
# submits on the claim-aligned scale the chart is drawn on. Open the printed
# link and press Send.
report_result()
