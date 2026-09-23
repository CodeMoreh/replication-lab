# ########################################################################### #
# Title:        01_build_respondents.R – the Eurobarometer respondent file
# Purpose:      Turn the sixteen GESIS Eurobarometer files you downloaded into
#               one respondent-level data frame carrying everything the course
#               can ask of the microdata: the fourteen EU-meaning items with the
#               "other" and "don't know" codes beside them, the design weight,
#               and the respondent characteristics that the published models
#               use (age, sex, education, occupation, urbanisation, left–right
#               placement). Nothing is scored here. The framing scales are
#               built in prep/02_scales_and_panel.R, which is where the course
#               teaches them. Every file is checked against the version and the
#               checksum that the course panel was built from, so a mismatch is
#               reported rather than silently absorbed, and the row counts per
#               country-year are checked against the committed panel.
# Reads:        the sixteen .dta files in the folder named by EB_RAW_DIR
#               data/EUframes_cy.csv and data/teney_panel_weights.csv (checks)
# Writes:       data_private/eb_respondents.rds – GESIS-derived microdata:
#               never committed, uploaded, shared or passed to an AI assistant
#               data_private/build_report.csv – the per-file check table
# Author:       Chris Moreh
# Last updated: 2026-09-08
# ########################################################################### #

# --- where the files are ------------------------------------------------------
# Put the sixteen .dta files (and the two .dta.zip archives, if you have not
# unzipped them) in ONE folder outside this workspace, and name that folder
# here or in the environment variable EB_RAW_DIR. On Windows a path can be
# written with forward slashes: "C:/Users/you/Eurobarometer".
raw_dir <- Sys.getenv("EB_RAW_DIR", unset = "../eurobarometer_raw")

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(purrr); library(readr); library(tibble)
})

# --- the sixteen studies, as the course holds them ----------------------------
# One row per wave: the file name GESIS serves, the study number, the fieldwork
# year the course assigns the wave to, the column holding the country, where
# the EU-meaning block starts (v-numbered files) or the question stem it hangs
# off (qa-named files), the design-weight column, and the MD5 checksum of the
# file the course panel was built from. ZA4506 and ZA4744 arrive from GESIS as
# .dta.zip; the script unzips them beside the archive.
inventory <- tribble(
  ~file,               ~study,   ~eb,    ~year, ~cntry_v,   ~items_from, ~items_style, ~w1_v, ~md5,
  "ZA4056_v1-0-1.dta", "ZA4056", "61.0", 2004, "isocntry",  63,  "v",    "v9", "0fc7763a1b0ae4bac6fb70bf87d07543",
  "ZA4229_v1-1-0.dta", "ZA4229", "62.0", 2004, "v7",        105, "v",    "v8", "76051b80553ee29be104d13777eeae31",
  "ZA4411_v1-1-0.dta", "ZA4411", "63.4", 2005, "v7",        89,  "v",    "v8", "d124e48c46b8744e14cb21533c3f25b3",
  "ZA4414_v1-1-0.dta", "ZA4414", "64.2", 2005, "v7",        118, "v",    "v8", "9456ddc9ddb878d4b98fadf88db40661",
  "ZA4506_v1-0-1.dta", "ZA4506", "65.2", 2006, "v7",        98,  "v",    "v8", "2c554ae535b409d937742c29cd05893a",
  "ZA4530_v2-1-0.dta", "ZA4530", "67.2", 2007, "v7",        130, "v",    "v8", "ec1115b3c551448ce9098e784d885d3d",
  "ZA4744_v5-0-0.dta", "ZA4744", "69.2", 2008, "v7",        218, "v",    "v8", "bc8661fd37984d2ad966e4065137db5a",
  "ZA4819_v3-0-2.dta", "ZA4819", "70.1", 2008, "v7",        234, "v",    "v8", "4947c7b4dc486efe7660deb2377292a7",
  "ZA4994_v3-0-0.dta", "ZA4994", "72.4", 2009, "v7",        221, "v",    "v8", "6c1e0b55d9a3a6c6114748d5939931a8",
  "ZA5234_v2-0-1.dta", "ZA5234", "73.4", 2010, "v7",        277, "v",    "v8", "0a1d8509648766913b76bc0942a608ec",
  "ZA5449_v2-2-0.dta", "ZA5449", "74.2", 2010, "v7",        306, "v",    "v8", "eb8f9b25b54f2c2246c269d2a87899a5",
  "ZA5481_v2-0-1.dta", "ZA5481", "75.3", 2011, "v7",        315, "v",    "v8", "38faabba08eb55646a2af3baf11bb0cd",
  "ZA5567_v2-0-1.dta", "ZA5567", "76.3", 2011, "isocntry",  0,   "qa12", "w1", "48399a7555224a194d32a92186fd9c89",
  "ZA5612_v2-0-0.dta", "ZA5612", "77.3", 2012, "isocntry",  0,   "qa15", "w1", "f25e4512690bf6445e11290160ca1976",
  "ZA5685_v2-0-0.dta", "ZA5685", "78.1", 2012, "isocntry",  0,   "qa13", "w1", "dbeae5925e6ce470ee26ac2024f105e1",
  "ZA5689_v2-0-0.dta", "ZA5689", "79.3", 2013, "isocntry",  0,   "qa14", "w1", "e46f479d0d82b4d0cc3fccd69b708397"
)

# The fourteen items of "What does the European Union mean to you personally?",
# in the order the questionnaire lists them, followed by the two closing codes.
# The euro item (8) is carried but never scored, following the published paper.
item_labels <- c(
  it1  = "Peace",
  it2  = "Economic prosperity",
  it3  = "Democracy",
  it4  = "Social protection",
  it5  = "Freedom to travel, study and work anywhere in the EU",
  it6  = "Cultural diversity",
  it7  = "Stronger say in the world",
  it8  = "Euro",
  it9  = "Unemployment",
  it10 = "Bureaucracy",
  it11 = "Waste of money",
  it12 = "Loss of our cultural identity",
  it13 = "More crime",
  it14 = "Not enough control at external frontiers",
  oth  = "Other (spontaneous)",
  dk   = "Don't know"
)

eu27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB",
          "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
          "SE", "SI", "SK")

# --- 1. are the files there, and are they the files the course used? ----------
# A file that is missing stops the script with the study number to fetch. A
# file whose checksum differs is read anyway, because GESIS re-releases files
# under the same name with corrections; the report says which, and the cell
# count check at the end says whether it matters.
if (!dir.exists(raw_dir))
  stop("The folder '", raw_dir, "' does not exist. Set raw_dir at the top of ",
       "this script, or the EB_RAW_DIR environment variable, to the folder ",
       "holding the sixteen .dta files.", call. = FALSE)

unzip_if_needed <- function(file) {
  target <- file.path(raw_dir, file)
  archive <- paste0(target, ".zip")
  if (!file.exists(target) && file.exists(archive)) {
    utils::unzip(archive, exdir = raw_dir)
    if (!file.exists(target))
      stop(basename(archive), " unzipped, but ", file, " was not inside it.",
           call. = FALSE)
  }
  target
}

paths <- map_chr(inventory$file, unzip_if_needed)
missing <- inventory$file[!file.exists(paths)]
if (length(missing) > 0)
  stop("Not found in '", raw_dir, "': ", paste(missing, collapse = ", "),
       ". Download the Stata (.dta) file of each study from search.gesis.org ",
       "into that folder; the study numbers are in the inventory above.",
       call. = FALSE)

checksums <- tools::md5sum(paths) |> unname()

# --- 2. read each wave ---------------------------------------------------------
# The EU-meaning block is sixteen adjacent columns in every file: fourteen
# items, then "other", then "don't know". The respondent characteristics are
# found by their variable labels rather than by position, because the position
# of the occupation item in particular moves from wave to wave.
covariate_labels <- c(
  age    = "AGE EXACT$",
  gender = "GENDER$|D10 SEX$",
  edu    = "AGE EDUCATION$",
  occ    = "OCCUPATION OF RESPONDENT$",
  urban  = "TYPE OF COMMUNITY$",
  rile   = "LEFT-RIGHT PLACEMENT$"
)

label_of <- function(d) {
  map_chr(d, \(x) { l <- attr(x, "label"); if (is.null(l)) "" else as.character(l) })
}

# One column name per characteristic, or NA when a wave does not carry it. Two
# matches is an error rather than a guess: it means the pattern has stopped
# being specific to one variable.
resolve <- function(nm, lab, pattern, file, what) {
  i <- grep(pattern, lab, ignore.case = TRUE)
  if (length(i) == 0) return(NA_character_)
  if (length(i) > 1)
    stop(file, ": '", what, "' matched ", length(i), " variables (",
         paste(nm[i], collapse = ", "), ")", call. = FALSE)
  nm[i]
}

read_wave <- function(w, path) {
  items <- if (w$items_style == "v") paste0("v", w$items_from:(w$items_from + 15))
           else paste0(w$items_style, "_", 1:16)

  head <- read_dta(path, n_max = 0)
  nm <- names(head); lab <- label_of(head)
  cov <- imap_chr(covariate_labels, \(p, k) resolve(nm, lab, p, w$file, k))

  cols <- c(w$cntry_v, items, w$w1_v, na.omit(unname(cov)))
  d <- read_dta(path, col_select = all_of(cols))
  d <- d[, cols]
  names(d) <- c("cntry", names(item_labels), "w1", names(cov)[!is.na(cov)])
  for (miss in names(cov)[is.na(cov)]) d[[miss]] <- NA_real_

  d <- d |> mutate(across(all_of(names(item_labels)), \(x) as.numeric(x)))
  vals <- unique(na.omit(unlist(d[names(item_labels)])))
  stopifnot("EU-meaning items are not coded 0/1" = all(vals %in% c(0, 1)))

  # Germany and the United Kingdom are sampled as two regions each; the
  # published analysis treats each as one country.
  d |>
    mutate(cntry = as.character(cntry),
           cntry = recode(cntry, "DE-E" = "DE", "DE-W" = "DE",
                          "GB-GBN" = "GB", "GB-NIR" = "GB"),
           study = w$study, eb = w$eb, year = w$year,
           across(c(age, gender, edu, occ, urban, rile, w1), \(x) as.numeric(x))) |>
    filter(cntry %in% eu27) |>
    select(study, eb, year, cntry, w1, all_of(names(item_labels)),
           age, gender, edu, occ, urban, rile)
}

waves <- map2(seq_len(nrow(inventory)), paths,
              \(i, p) read_wave(inventory[i, ], p))
respondents <- bind_rows(waves)

# --- 3. the respondent characteristics, coded as the published models code them
# Education is the age at which full-time schooling finished. GESIS codes
# "still studying" as 98 and "no full-time education" as 97; the published
# preparation replaces 98 with the respondent's current age and sets 97 to
# missing, then cuts three bands at 16 and 20. The four-band version keeps the
# students as a band of their own and folds "none" into the lowest band, which
# is the coding the GESIS recode offers; both are kept so the choice between
# them is yours to make on the menu.
#
# Socio-economic position collapses the eighteen-category occupation item into
# the eight groups of the published models, with the unemployed as their own
# group. Left–right placement is 1 to 10 and is asked in twelve of the sixteen
# waves; the other four are missing by design.
respondents <- respondents |>
  mutate(
    female  = as.integer(gender == 2),
    edu_raw = edu,
    edu4    = case_when(edu_raw == 98            ~ "studying",
                        edu_raw %in% c(97, 0:15) ~ "le15",
                        edu_raw %in% 16:19       ~ "e16_19",
                        edu_raw %in% 20:96       ~ "e20plus",
                        TRUE                     ~ NA_character_),
    edu     = if_else(edu %in% c(0, 97, 99), NA_real_, edu),
    edu     = if_else(edu == 98, age, edu),
    edu3    = case_when(edu < 16              ~ "low",
                        edu >= 16 & edu <= 19 ~ "mid",
                        edu > 19              ~ "high",
                        TRUE                  ~ NA_character_),
    ses     = case_when(occ %in% 5:9   ~ "self_employed",
                        occ %in% 10:12 ~ "manager",
                        occ %in% 13:14 ~ "white_collar",
                        occ %in% 15:18 ~ "manual",
                        occ == 1       ~ "house_person",
                        occ == 3       ~ "unemployed",
                        occ == 4       ~ "retired",
                        occ == 2       ~ "student",
                        TRUE           ~ NA_character_),
    unemployed = as.integer(ses == "unemployed"),
    urban   = case_when(urban == 1 ~ "rural", urban == 2 ~ "small_town",
                        urban == 3 ~ "large_town", TRUE ~ NA_character_),
    rile    = if_else(rile %in% 1:10, rile, NA_real_)
  ) |>
  select(-gender, -occ)

# --- 4. the checks ---------------------------------------------------------------
# Three things are compared with the files the course ships. The checksum says
# whether each file is byte for byte the one the panel was built from. The cell
# count says whether the same respondents are in the same country-years. The
# no-opinion count says whether the closing codes were read from the right
# columns. All three should agree exactly; the report says where they do not.
panel   <- read_csv("data/EUframes_cy.csv", show_col_types = FALSE)
weights <- read_csv("data/teney_panel_weights.csv", show_col_types = FALSE)

items13 <- names(item_labels)[c(1:7, 9:14)]
cells <- respondents |>
  mutate(n_all = rowSums(across(all_of(items13))),
         noop_type = case_when(n_all > 0 ~ "answered", oth == 1 ~ "other",
                               dk == 1 ~ "dk", TRUE ~ "blank")) |>
  summarise(n_cy_built = n(),
            n_noop_built  = sum(n_all == 0),
            n_dk_built    = sum(noop_type == "dk"),
            n_other_built = sum(noop_type == "other"),
            n_blank_built = sum(noop_type == "blank"),
            .by = c(cntry, year))

cell_check <- panel |>
  select(cntry, year, n_cy) |>
  left_join(weights |> select(cntry, year, n_noop, n_dk, n_other, n_blank),
            by = c("cntry", "year")) |>
  left_join(cells, by = c("cntry", "year"))

file_report <- inventory |>
  select(study, eb, year, file, md5_expected = md5) |>
  mutate(md5_found = checksums,
         identical_to_course_file = md5_found == md5_expected,
         rows_eu27 = map_int(waves, nrow))

report <- tibble(
  check = c("files found", "files identical to the course build",
            "country-years in the committed panel", "country-years rebuilt",
            "cells with an identical respondent count",
            "cells with identical no-opinion counts (none, DK, other, blank)"),
  value = c(nrow(inventory), sum(file_report$identical_to_course_file),
            nrow(panel), nrow(cells),
            sum(cell_check$n_cy == cell_check$n_cy_built, na.rm = TRUE),
            sum(cell_check$n_noop == cell_check$n_noop_built &
                cell_check$n_dk == cell_check$n_dk_built &
                cell_check$n_other == cell_check$n_other_built &
                cell_check$n_blank == cell_check$n_blank_built, na.rm = TRUE))
)

# --- 5. write --------------------------------------------------------------------
# data_private/ is ignored by git in this workspace. Keep it that way: the file
# below is derived from licensed microdata and stays on your machine.
dir.create("data_private", showWarnings = FALSE)
saveRDS(respondents, "data_private/eb_respondents.rds")
write_csv(file_report, "data_private/build_report.csv")

print(as.data.frame(file_report |> select(study, eb, year, rows_eu27,
                                          identical_to_course_file)),
      row.names = FALSE, right = FALSE)
cat("\n")
print(as.data.frame(report), row.names = FALSE, right = FALSE)
cat("\nrespondents:", nrow(respondents), "rows,", ncol(respondents),
    "columns, written to data_private/eb_respondents.rds\n")
if (all(file_report$identical_to_course_file) &&
    report$value[5] == nrow(panel) && report$value[6] == nrow(panel)) {
  cat("Every check passed: your file is the one the course was built from.\n")
} else {
  cat("At least one check differs. A checksum difference means GESIS has ",
      "re-released that file; a cell difference means the respondents read ",
      "are not the course's. Compare data_private/build_report.csv with the ",
      "inventory above, and bring the report to the session.\n", sep = "")
}
