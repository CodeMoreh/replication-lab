# ============================================================================
# EXHIBIT ONLY – the CORRECTED workshop build that produced data/teney_panel.csv.
# Do NOT run it here: like the original prep script it needs the raw GESIS
# Eurobarometer scientific-use files (registration required, not redistributable).
# The paths below (rawdir, outdir_*) are the coordinator's own local machine
# paths and will not exist on yours – they are left in deliberately so you can
# see exactly how the corrected panel was built. The fix is at the 2009 wave:
# this build reads ZA4994 (EB 72.4) where the published script duplicated ZA4819.
# ============================================================================

# ============================================================================
# Build the workshop panel (teney_panel.csv) from raw Eurobarometer files
# - Corrected wave set (ZA4994 = EB 72.4 for 2009) -> data/teney_panel.csv
# - As-published wave set (ZA4819 duplicated)      -> _planning_data/ (comparison)
# - Validates non-2009 aggregates against the published rep_data.csv
# - Quantifies the impact of the ZA4819/ZA4994 wave swap on the Task-2 model
# Raw data: GESIS Eurobarometer scientific-use files (NOT redistributable).
# ============================================================================

suppressPackageStartupMessages({ library(haven); library(dplyr); library(tidyr); library(readr) })

rawdir <- "d:/GitHub/courses/2026_OR_NCL/_planning_data/raw"
outdir_public <- "d:/GitHub/courses/2026_OR_NCL/data"
outdir_priv   <- "d:/GitHub/courses/2026_OR_NCL/_planning_data"
dir.create(outdir_public, recursive = TRUE, showWarnings = FALSE)

# --- wave configuration ------------------------------------------------------
# items: the 14 'EU MEANING' variables in questionnaire order (verified labels):
# 1 PEACE | 2 ECONOMIC PROSPERITY | 3 DEMOCRACY | 4 SOCIAL PROTECTION
# 5 TRAVEL/STUDY/WORK ABROAD | 6 CULTURAL DIVERSITY | 7 STRONGER SAY IN THE WORLD
# 8 EURO (excluded) | 9 UNEMPLOYMENT | 10 BUREAUCRACY | 11 WASTE OF MONEY
# 12 LOSS OF CULTURAL IDENTITY | 13 MORE CRIME | 14 EXT FRONTIER CONTROL
waves <- list(
  list(file="ZA4056_v1-0-1.dta", eb="61.0", year=2004, cntry="isocntry", items=paste0("v", 63:76)),
  list(file="ZA4229_v1-1-0.dta", eb="62.0", year=2004, cntry="v7",       items=paste0("v", 105:118)),
  list(file="ZA4411_v1-1-0.dta", eb="63.4", year=2005, cntry="v7",       items=paste0("v", 89:102)),
  list(file="ZA4414_v1-1-0.dta", eb="64.2", year=2005, cntry="v7",       items=paste0("v", 118:131)),
  list(file="ZA4506_v1-0-1.dta", eb="65.2", year=2006, cntry="v7",       items=paste0("v", 98:111)),
  list(file="ZA4530_v2-1-0.dta", eb="67.2", year=2007, cntry="v7",       items=paste0("v", 130:143)),
  list(file="ZA4744_v5-0-0.dta", eb="69.2", year=2008, cntry="v7",       items=paste0("v", 218:231)),
  list(file="ZA4819_v3-0-2.dta", eb="70.1", year=2008, cntry="v7",       items=paste0("v", 234:247)),
  list(file="ZA4994_v3-0-0.dta", eb="72.4", year=2009, cntry="v7",       items=paste0("v", 221:234)),
  list(file="ZA5234_v2-0-1.dta", eb="73.4", year=2010, cntry="v7",       items=paste0("v", 277:290)),
  list(file="ZA5449_v2-2-0.dta", eb="74.2", year=2010, cntry="v7",       items=paste0("v", 306:319)),
  list(file="ZA5481_v2-0-1.dta", eb="75.3", year=2011, cntry="v7",       items=paste0("v", 315:328)),
  list(file="ZA5567_v2-0-1.dta", eb="76.3", year=2011, cntry="isocntry", items=paste0("qa12_", 1:14)),
  list(file="ZA5612_v2-0-0.dta", eb="77.3", year=2012, cntry="isocntry", items=paste0("qa15_", 1:14)),
  list(file="ZA5685_v2-0-0.dta", eb="78.1", year=2012, cntry="isocntry", items=paste0("qa13_", 1:14)),
  list(file="ZA5689_v2-0-0.dta", eb="79.3", year=2013, cntry="isocntry", items=paste0("qa14_", 1:14))
)

eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR",
          "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")

# dimension item positions (of the 14; Euro = 8 excluded everywhere)
dim_idx <- list(cosmo = c(1,3,5,6,7), util = c(2,4),
                comm  = c(9,12,13,14), lib  = c(10,11))

read_wave <- function(w) {
  d <- read_dta(file.path(rawdir, w$file), col_select = all_of(c(w$cntry, w$items)))
  names(d) <- c("cntry", paste0("it", 1:14))
  d <- d |> mutate(across(starts_with("it"), ~ as.numeric(.x)))
  vals <- unique(na.omit(unlist(d[paste0("it", 1:14)])))
  stopifnot("framing items not coded 0/1" = all(vals %in% c(0, 1)))
  d |>
    mutate(cntry = as.character(cntry),
           cntry = recode(cntry, "DE-E"="DE", "DE-W"="DE", "GB-GBN"="GB", "GB-NIR"="GB"),
           eb = w$eb, year = w$year) |>
    filter(cntry %in% eu27)
}

cat("Reading 16 waves...\n")
ind <- bind_rows(lapply(waves, read_wave))
cat("Individual-level rows (corrected set):", nrow(ind), "\n")

score <- function(d) {
  d |>
    mutate(
      s_cosmo = rowSums(across(all_of(paste0("it", dim_idx$cosmo)))),
      s_util  = rowSums(across(all_of(paste0("it", dim_idx$util)))),
      s_comm  = rowSums(across(all_of(paste0("it", dim_idx$comm)))),
      s_lib   = rowSums(across(all_of(paste0("it", dim_idx$lib)))),
      s_all   = rowSums(across(all_of(paste0("it", c(1:7, 9:14))))),  # 13 items, Euro excluded
      # relative-frequency scales (Teney 2016: 623); 0 when nothing mentioned,
      # matching the convention in the published replication script
      cosmo = ifelse(s_cosmo == 0, 0, s_cosmo / s_all),
      util  = ifelse(s_util  == 0, 0, s_util  / s_all),
      comm  = ifelse(s_comm  == 0, 0, s_comm  / s_all),
      lib   = ifelse(s_lib   == 0, 0, s_lib   / s_all),
      pos   = ifelse(s_cosmo + s_util == 0, 0, (s_cosmo + s_util) / s_all),
      neg   = ifelse(s_comm  + s_lib  == 0, 0, (s_comm  + s_lib)  / s_all)
    )
}

aggregate_panel <- function(d) {
  d |>
    score() |>
    group_by(cntry, year) |>
    summarise(mcosmo = mean(cosmo, na.rm = TRUE), mutil = mean(util, na.rm = TRUE),
              mcomm  = mean(comm,  na.rm = TRUE), mlib  = mean(lib,  na.rm = TRUE),
              mpos   = mean(pos,   na.rm = TRUE), mneg  = mean(neg,  na.rm = TRUE),
              n_cy = n(), .groups = "drop")
}

panel_corr <- aggregate_panel(ind)

# as-published variant: drop EB 72.4, duplicate EB 70.1 as 2009
ind_pub <- ind |>
  filter(eb != "72.4") |>
  bind_rows(ind |> filter(eb == "70.1") |> mutate(eb = "70.1-dup", year = 2009))
panel_pub <- aggregate_panel(ind_pub)

# --- macro variables ---------------------------------------------------------
repdata <- read_csv("d:/GitHub/courses/2026_OR_NCL/_planning/osf_source/moreh_rep/data/rep_data.csv",
                    show_col_types = FALSE)

# EU/IMF financial assistance programmes active per country-year
# (BoP facility: HU 2008-10, LV 2008-11, RO 2009-13; EFSF/EFSM/ESM & bank
#  recapitalisation: GR 2010-13, IE 2010-13, PT 2011-13, ES 2012-13, CY 2013)
bailout <- tribble(
  ~cntry, ~from, ~to,
  "HU", 2008, 2010,  "LV", 2008, 2011,  "RO", 2009, 2013,
  "GR", 2010, 2013,  "IE", 2010, 2013,  "PT", 2011, 2013,
  "ES", 2012, 2013,  "CY", 2013, 2013
)

add_macro <- function(panel) {
  panel |>
    left_join(repdata |> select(cntry, country, year, growth, unemp),
              by = c("cntry", "year")) |>
    rowwise() |>
    mutate(bailout = as.integer(any(bailout$cntry == cntry &
                                    year >= bailout$from & year <= bailout$to))) |>
    ungroup() |>
    relocate(country, cntry, year) |>
    arrange(cntry, year)
}

panel_corr <- add_macro(panel_corr)
panel_pub  <- add_macro(panel_pub)

write_csv(panel_corr, file.path(outdir_public, "teney_panel.csv"))
write_csv(panel_pub,  file.path(outdir_priv,  "teney_panel_aspublished.csv"))

# --- validation & impact -----------------------------------------------------
suppressPackageStartupMessages(library(plm))

cat("\n--- VALIDATION: as-published rebuild vs published rep_data.csv (mcosmopolitan) ---\n")
chk <- panel_pub |>
  inner_join(repdata |> select(cntry, year, mcosmopolitan, n_cy_pub = n_cy),
             by = c("cntry", "year"))
cat("max |mcosmo - mcosmopolitan| all years :", max(abs(chk$mcosmo - chk$mcosmopolitan)), "\n")
cat("max |n_cy - n_cy_pub|       all years :", max(abs(chk$n_cy - chk$n_cy_pub)), "\n")

cat("\n--- IMPACT of the wave swap on year-2009 values (corrected vs published) ---\n")
imp <- panel_corr |> filter(year == 2009) |>
  inner_join(repdata |> filter(year == 2009) |> select(cntry, mcosmopolitan),
             by = "cntry")
cat("mean |mcosmo_corrected - mcosmo_published| in 2009:",
    round(mean(abs(imp$mcosmo - imp$mcosmopolitan)), 4), "\n")

t2 <- function(d, dv) {
  d$y <- d[[dv]]
  d$unemp_c <- d$unemp - mean(d$unemp, na.rm = TRUE)
  m <- plm(y ~ unemp_c, data = d, index = c("cntry", "year"),
           effect = "twoways", model = "within")
  s <- summary(m)$coefficients
  sprintf("b = %.5f, se = %.5f, t = %.3f", s[1, 1], s[1, 2], s[1, 3])
}
cat("\n--- Task-2 model: mcosmopolitan ~ unemp_c, two-way FE ---\n")
cat("published rep_data.csv :", t2(repdata |> rename(y0 = mcosmopolitan) |> mutate(mcosmopolitan = y0), "mcosmopolitan"), "\n")
cat("as-published rebuild   :", t2(panel_pub,  "mcosmo"), "\n")
cat("corrected panel        :", t2(panel_corr, "mcosmo"), "\n")

cat("\nRows in teney_panel.csv:", nrow(panel_corr), "| countries:",
    length(unique(panel_corr$cntry)), "| years:", length(unique(panel_corr$year)), "\n")
cat("BUILD DONE\n")
