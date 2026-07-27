# ########################################################################### #
# Title:        Forensics – see the two builds' disagreement for yourself
# Purpose:      An optional exhibit for the reproducibility story in Task 3.
#               Diffs the as-submitted panel against the corrected one year by
#               year, and counts respondents per year: the disagreement sits
#               entirely on 2009, which the as-submitted build copied from 2008.
# Reads:        data/rep_data.csv, data/EUframes_cy.csv
# Writes:       figures/forensics_year_diff.png,
#               figures/forensics_ncy_by_year.png
# Author:       Chris Moreh
# Last updated: 2026-07-19
# ########################################################################### #

# Not part of any task – run it only if you are curious to confirm the story in
# Task 3's "The other number in the record" callout. The as-submitted
# rep_data.csv gives t = -3.853; the corrected EUframes_cy.csv gives -3.804.
# Package-light on purpose: readr/dplyr/ggplot2 are all you need. The printed
# tables are the result; the two figures are saved for your notes.

library(readr)
library(dplyr)
library(ggplot2)

out_dir <- "figures"   # where the two figures are written

## ---- Read both builds and align them on country-year ----------------------

rep <- read_csv("data/rep_data.csv", show_col_types = FALSE) |>
  select(cntry, year, mcosmo_rep = mcosmopolitan, n_cy_rep = n_cy)

ten <- read_csv("data/EUframes_cy.csv", show_col_types = FALSE) |>
  select(cntry, year, mcosmo_ten = mcosmo, n_cy_ten = n_cy)

joined <- rep |>
  inner_join(ten, by = c("cntry", "year")) |>
  mutate(abs_diff = abs(mcosmo_rep - mcosmo_ten))

## ---- Clue 1: outcome disagreement by year ---------------------------------

# For each year, how far apart are the two builds on the cosmopolitan outcome?
by_year <- joined |>
  group_by(year) |>
  summarise(
    n_countries   = n(),
    mean_abs_diff = mean(abs_diff),
    max_abs_diff  = max(abs_diff),
    .groups = "drop"
  )

print(by_year, n = Inf)

## ---- Clue 2: respondents per year in the as-submitted panel ---------------

# Total respondents behind each year. Compare a year with the one before it.
ncy_by_year <- rep |>
  group_by(year) |>
  summarise(
    total_n = sum(n_cy_rep),
    mean_n  = mean(n_cy_rep),
    .groups = "drop"
  )

print(ncy_by_year, n = Inf)

# The 2008 vs 2009 step, stated plainly.
ratio_0809 <- ncy_by_year |>
  filter(year %in% c(2008, 2009)) |>
  summarise(ratio_2009_over_2008 = total_n[year == 2009] / total_n[year == 2008])

print(ratio_0809)

## ---- Figures --------------------------------------------------------------

# Mean absolute outcome difference by year.
p_diff <- ggplot(by_year, aes(x = factor(year), y = mean_abs_diff,
                              fill = year == 2009)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#B2182B"), guide = "none") +
  labs(
    title = "How far apart are the two builds, by year?",
    subtitle = "Mean absolute difference in cosmopolitan framing",
    x = NULL, y = "Mean |difference|"
  ) +
  theme_minimal(base_size = 12)

# Respondents per year in the as-submitted panel.
p_ncy <- ggplot(ncy_by_year, aes(x = factor(year), y = total_n,
                                 fill = year == 2009)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#B2182B"), guide = "none") +
  labs(
    title = "Respondents per year in the as-submitted panel",
    subtitle = "Total respondents (sum of n_cy)",
    x = NULL, y = "Respondents"
  ) +
  theme_minimal(base_size = 12)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_dir, "forensics_year_diff.png"),   p_diff, width = 7, height = 4, dpi = 150)
ggsave(file.path(out_dir, "forensics_ncy_by_year.png"), p_ncy,  width = 7, height = 4, dpi = 150)
