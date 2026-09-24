# ########################################################################### #
# Title:        02_scales_and_panel.R – from items to scales to the panel
# Purpose:      Rebuild the country-year panel that ships with the course from
#               the respondent file written by 01_build_respondents.R, one
#               decision at a time: which items form which scale, how a
#               respondent's answers are scored, what happens to a respondent
#               who named nothing, how respondents are averaged into a
#               country-year, and how the macro series are merged in. The
#               rebuilt panel is compared with data/EUframes_cy.csv, which it
#               reproduces to machine precision when the respondent file has
#               passed its checks. The last section builds the panel by
#               education band, which the individual-level extension of the
#               course uses.
# Reads:        data_private/eb_respondents.rds
#               data/EUframes_cy.csv, data/predictor_measures.csv,
#               data/teney_panel_weights.csv
# Writes:       data_private/EUframes_cy_rebuilt.csv
#               data_private/EUframes_cy_edu.csv
# Author:       Chris Moreh
# Last updated: 2026-09-08
# ########################################################################### #

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
})

respondents <- readRDS("data_private/eb_respondents.rds")
panel       <- read_csv("data/EUframes_cy.csv", show_col_types = FALSE)
measures    <- read_csv("data/predictor_measures.csv", show_col_types = FALSE)
weights     <- read_csv("data/teney_panel_weights.csv", show_col_types = FALSE)

# --- 1. which items form which scale --------------------------------------------
# The published assignment of the thirteen scored items to four scales, by item
# position. The euro item (8) is left out throughout, as in the paper. The two
# composites are the positive and the negative pair of scales.
items <- list(
  cosmo = paste0("it", c(1, 3, 5, 6, 7)),  # peace, democracy, travel, diversity, say
  util  = paste0("it", c(2, 4)),           # prosperity, social protection
  comm  = paste0("it", c(9, 12, 13, 14)),  # unemployment, identity, crime, frontiers
  lib   = paste0("it", c(10, 11))          # bureaucracy, waste of money
)
items$pos <- c(items$cosmo, items$util)
items$neg <- c(items$comm, items$lib)
items13   <- c(items$pos, items$neg)

# --- 2. three scoring rules ------------------------------------------------------
# A respondent's answers to a set of items can be turned into one number in
# more than one way, and the course menu offers three. The share is the
# published rule: the mentions from the set over the respondent's own total.
# The count is the number of items in the set that were mentioned. The
# per-item rate is the count over the number of items in the set. Which of
# the three you use is a decision about what the outcome is, and the
# specification menu treats it as one.
#
# A respondent who mentioned none of the thirteen items has no total to divide
# by. The published rule scores them zero on every scale. The other rules on
# the menu (drop, split, impute) are applied further down the pipeline and are
# explained in the missing-answers module.
scored <- respondents |>
  mutate(n_all = rowSums(across(all_of(items13))))

for (s in names(items)) {
  k <- length(items[[s]])
  scored <- scored |>
    mutate("c_{s}"  := rowSums(across(all_of(items[[s]]))),
           "s_{s}"  := if_else(n_all == 0, 0, .data[[paste0("c_", s)]] / n_all),
           "r_{s}"  := .data[[paste0("c_", s)]] / k)
}

# --- 3. from respondents to country-years -----------------------------------------
# The course panel averages every respondent in a country-year with the same
# weight ('as interviewed'). The design weight w1, which GESIS supplies to
# correct each national sample to its population, is the other choice, and the
# weights module compares the two. Both are computed here.
cells <- scored |>
  summarise(
    across(starts_with("s_"), mean, .names = "m{.col}"),
    across(starts_with("s_"), \(x) weighted.mean(x, w1), .names = "w{.col}"),
    across(starts_with("c_"), mean, .names = "m{.col}"),
    n_cy = n(),
    .by = c(cntry, year)
  ) |>
  rename_with(\(x) sub("^ms_", "m", x), starts_with("ms_")) |>
  rename_with(\(x) sub("^ws_", "w_m", x), starts_with("ws_")) |>
  rename_with(\(x) sub("^mc_", "c_m", x), starts_with("mc_"))

# --- 4. the macro series ----------------------------------------------------------
# The unemployment rate in the panel is the national estimate from the World
# Bank (SL.UEM.TOTL.NE.ZS), and the growth series is annual GDP growth from the
# same source (NY.GDP.MKTP.KD.ZG). Both are open data. Every series offered on
# the menu ships with the course in data/predictor_measures.csv, so the merge
# below works offline. The same two series can be fetched live with the WDI
# package:
#
#   WDI::WDI(country = unique(cells$cntry),
#            indicator = c(unemp = "SL.UEM.TOTL.NE.ZS", growth = "NY.GDP.MKTP.KD.ZG"),
#            start = 2004, end = 2013)
#
# and the Eurostat alternatives with eurostat::get_eurostat(). A live fetch may
# differ from the committed values in the last decimals, because the Bank
# revises past years. The committed file records the vintage that the course
# used.
macro <- measures |>
  filter(measure %in% c("wb_nat", "wb_growth")) |>
  select(cntry, year, predictor, value) |>
  pivot_wider(names_from = predictor, values_from = value)

# The bailout indicator is coded by rule from the record of EU and IMF
# assistance programmes; the codebook lists the programmes and years.
bailout <- panel |> select(cntry, year, bailout)

rebuilt <- cells |>
  left_join(macro,   by = c("cntry", "year")) |>
  left_join(bailout, by = c("cntry", "year")) |>
  arrange(cntry, year)

# --- 5. comparison with the course panel -------------------------------------------
compare <- panel |>
  inner_join(rebuilt, by = c("cntry", "year"), suffix = c("_course", "_rebuilt"))

scale_cols <- c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg")
differences <- tibble(
  column = c(scale_cols, "n_cy", "unemp", "growth"),
  max_abs_difference = map_dbl(column, \(v)
    max(abs(compare[[paste0(v, "_course")]] - compare[[paste0(v, "_rebuilt")]]),
        na.rm = TRUE))
)

weighted_check <- weights |>
  select(cntry, year, w_mcosmo, w_mutil, w_mcomm, w_mlib, w_mpos, w_mneg,
         c_mcosmo, c_mutil, c_mcomm, c_mlib, c_mpos, c_mneg) |>
  inner_join(rebuilt, by = c("cntry", "year"), suffix = c("_course", "_rebuilt"))
weighted_differences <- tibble(
  column = c(paste0("w_m", c("cosmo", "util", "comm", "lib", "pos", "neg")),
             paste0("c_m", c("cosmo", "util", "comm", "lib", "pos", "neg"))),
  max_abs_difference = map_dbl(column, \(v)
    max(abs(weighted_check[[paste0(v, "_course")]] -
            weighted_check[[paste0(v, "_rebuilt")]]), na.rm = TRUE))
)

cat("country-years compared:", nrow(compare), "of", nrow(panel), "\n\n")
cat("as-interviewed cell means, counts and macro series against data/EUframes_cy.csv:\n")
print(as.data.frame(differences), row.names = FALSE, right = FALSE)
cat("\ndesign-weighted and count-built cell means against data/teney_panel_weights.csv:\n")
print(as.data.frame(weighted_differences), row.names = FALSE, right = FALSE)

# --- 6. the panel by education band ---------------------------------------------------
# The individual-level extension asks whether unemployment narrows the gap
# between education bands. That question can be put to aggregates too: one row
# per country, year and band, with the same six scales and the number of
# respondents behind each mean, which is what a precision weight uses.
by_edu <- scored |>
  filter(!is.na(edu3)) |>
  summarise(across(starts_with("s_"), mean, .names = "m{.col}"),
            n = n(), .by = c(cntry, year, edu3)) |>
  rename_with(\(x) sub("^ms_", "m", x), starts_with("ms_")) |>
  left_join(macro,   by = c("cntry", "year")) |>
  left_join(bailout, by = c("cntry", "year")) |>
  arrange(cntry, year, edu3)

dir.create("data_private", showWarnings = FALSE)
write_csv(rebuilt |> select(cntry, year, all_of(scale_cols), n_cy, growth, unemp, bailout),
          "data_private/EUframes_cy_rebuilt.csv")
write_csv(by_edu, "data_private/EUframes_cy_edu.csv")
cat("\nwritten: data_private/EUframes_cy_rebuilt.csv (", nrow(rebuilt), " rows) and ",
    "data_private/EUframes_cy_edu.csv (", nrow(by_edu), " rows)\n", sep = "")
