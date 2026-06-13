# ============================================================================
# EXHIBIT ONLY – the ORIGINAL published prep script (OSF node 8rtwe). Do NOT
# run it: it needs the raw GESIS Eurobarometer microdata (registration required,
# not redistributable), and its source repo CGMoreh/data_raw now returns 404.
# It ships as a read-along record of the published pipeline. Watch steps #8 and
# #9 below: EB 70.1 (ZA4819) is read a SECOND time as the 2009 wave in place of
# EB 72.4 (ZA4994) – the wave-duplication discussed in data/teney_panel_codebook.md.
# ============================================================================

# ############################################################################ #
# Create replication dataset for Teney (2016)
# ############################################################################ #


## ------------- Load package libraries ------------------------------------

library(dplyr)

## ---- Get raw EB data (from GitHub repo) -------------------------------------

temp <- tempfile()
download.file("https://github.com/CGMoreh/data_raw/raw/main/Eurobarometer.zip",temp)

# unzip(temp, list = TRUE)  # Print a list of files in the folder


## ---- Create combined dataset ------------------------------------------------

# Not included in analysis:
  # eb571 <- haven::read_dta(unz(temp, "ZA3639_v1-0-1.dta")) 
  # eb591 <- haven::read_dta(unz(temp, "ZA3904_v1-0-1.dta")) 
  # eb713 <- haven::read_dta(unz(temp, "ZA4973_v3-0-0.dta")) 

euframes <- paste("fr", seq(1:14), sep = "")                    # Create standard names for the 14 'EU framing' items

eb <- bind_rows(                                                # Combine cases from 16 EB rounds
  #1# ---- eb61 ---- #
  (haven::read_dta(unz(temp, "ZA4056_v1-0-1.dta")) |>           # read in original Eurobarometer Gesis dataset 
    mutate(year = 2004) |>                                      # year of the survey data collection
    rename(cntry = isocntry) |>                                 # standardise country variable name
    rename_at(vars(v63:v76), ~ euframes) |>                     # standardise 'EU framing' variable names
    select(cntry, year, fr1:fr14, -fr8)),                       # keep vars of interest, exclude the "Euro" item
  #2# ---- eb62 ---- #
  (haven::read_dta(unz(temp, "ZA4229_v1-1-0.dta")) |>
    mutate(year = 2004) |> 
    rename(cntry = v7) |> 
    rename_at(vars(v105:v118), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)), 
  #3# ---- eb634 ---- #
  (haven::read_dta(unz(temp, "ZA4411_v1-1-0.dta"))  |> 
    mutate(year = 2005) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v89:v102), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #4# ---- eb642 ---- #
  (haven::read_dta(unz(temp, "ZA4414_v1-1-0.dta"))  |> 
    mutate(year = 2005) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v118:v131), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)), 
  #5# ---- eb652 ---- #
  (haven::read_dta(unz(temp, "ZA4506_v1-0-1.dta")) |> 
    mutate(year = 2006) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v98:v111), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #6# ---- eb672 ---- #
  (haven::read_dta(unz(temp, "ZA4530_v2-1-0.dta")) |> 
    mutate(year = 2007) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v130:v143), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #7# ---- eb692 ---- #
  (haven::read_dta(unz(temp, "ZA4744_v5-0-0.dta"))  |> 
    mutate(year = 2008) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v218:v231), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)), 
  #8# ---- eb701 ---- #
  (haven::read_dta(unz(temp, "ZA4819_v3-0-2.dta")) |> 
    mutate(year = 2008) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v234:v247), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #9# ---- eb724 ---- #
  # WAVE-DUPLICATION (the bug): this reads ZA4819 (EB 70.1) again for 2009.
  # EB 72.4 is ZA4994 – the corrected build (prep/workshop_panel_build.R) uses
  # ZA4994 here. See data/teney_panel_codebook.md for the consequence (t shifts
  # from -3.804 on the corrected panel to -3.853 on this as-published file).
  (haven::read_dta(unz(temp, "ZA4819_v3-0-2.dta"))  |>
    mutate(year = 2009) |>
    rename(cntry = v7)  |> 
    rename_at(vars(v234:v247), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #10# ---- eb734 ---- #
  (haven::read_dta(unz(temp, "ZA5234_v2-0-1.dta")) |> 
    mutate(year = 2010) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v277:v290), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #11# ---- eb742 ---- #
  (haven::read_dta(unz(temp, "ZA5449_v2-2-0.dta")) |> 
    mutate(year = 2010) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v306:v319), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #12# ---- eb753 ---- #
  (haven::read_dta(unz(temp, "ZA5481_v2-0-1.dta")) |> 
    mutate(year = 2011) |> 
    rename(cntry = v7)  |> 
    rename_at(vars(v315:v328), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #13# ---- eb763 ---- #
  (haven::read_dta(unz(temp, "ZA5567_v2-0-1.dta"))  |> 
    mutate(year = 2011) |> 
    rename(cntry = isocntry)  |> 
    rename_at(vars(qa12_1:qa12_14), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)), 
  #14# ---- eb773 ---- #
  (haven::read_dta(unz(temp, "ZA5612_v2-0-0.dta")) |> 
    mutate(year = 2012) |> 
    rename(cntry = isocntry)  |> 
    rename_at(vars(qa15_1:qa15_14), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #15# ---- eb781 ---- #
  (haven::read_dta(unz(temp, "ZA5685_v2-0-0.dta"))  |> 
    mutate(year = 2012) |> 
    rename(cntry = isocntry)  |> 
    rename_at(vars(qa13_1:qa13_14), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)),
  #16# ---- eb793 ---- #
  (haven::read_dta(unz(temp, "ZA5689_v2-0-0.dta")) |> 
    mutate(year = 2013) |> 
    rename(cntry = isocntry)  |> 
    rename_at(vars(qa14_1:qa14_14), ~ euframes) |> 
    select(cntry, year, fr1:fr14, -fr8)), 
  ) |>                                                             # Close `bind_rows` 
  mutate(cntry = recode(cntry, "DE-E"="DE", "DE-W"="DE", "GB-GBN"="GB", "GB-NIR"="GB")) |> # combine region levels for Germany and UK
  filter(cntry %in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR",   
                      "GB", "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", 
                      "PT", "RO", "SE", "SI", "SK"))  |>                                   # keep only 27 EU countries (as of 2013)
  mutate(id = paste(cntry, as.character(year), sep="-"),                                   # country-year ID
         positive = rowSums(across(c(fr1:fr7))),                                           # scale of positive EU framing
         cosmopolitan = rowSums(across(c(fr1, fr3, fr5:fr7))),                             # scale of only non-materialist positive framing
         all = rowSums(across(c(fr1:fr14))),                                               # scale of all EU framings
         positive_prop = ifelse(!positive, 0, positive/all),                              # positive framing as proportion of all
         cosmopolitan_prop = ifelse(!cosmopolitan, 0, cosmopolitan/all))                  # cosmopolitan as proportion


unlink(temp) # Unlink temp folder

# Intermediary dataset
saveRDS(eb, "data/eb_ind.rds", compress = "xz")

#ebm <- readRDS("data/eb.rds")

## ---- Create country/year level dataset --------------------------------------

eb_cy <- eb |>
  mutate(N = nrow(eb)) |>                                                 # N: total no. of cases in EB data
  group_by(cntry) |>
    mutate(n_cntry = n()) |>                                              # n_cntry: no. of cases by country
    ungroup() |>
  group_by(cntry, year, id) |>                                            # group by 'country' and 'year'
    summarise(across(c(mposframe = positive_prop, 
                       mcosmopolitan = cosmopolitan_prop, 
                       N, 
                       n_cntry), 
                     ~ mean(.x, na.rm = TRUE)),                           # country-year average framings
              n_cy = n()) |>                                              # n_cy: no. of cases by country&year
  ungroup()


## ---- Merge with WB data -----------------------------------------------------

wb <- read.csv("https://media.githubusercontent.com/media/CGMoreh/data_raw/main/WB_Data.csv")  |> 
  filter(year != 2003)

rep_data <- full_join(eb_cy, wb, by = c("id", "cntry", "year")) |> 
  relocate(id, country, cntry, year) |>
  relocate(c(N, n_cntry, n_cy), .after = last_col())

## ---- Save rep_data ----------------------------------------------------------

# dir.create("./data")
# saveRDS(rep_data, file = "data/rep_data.rds")
readr::write_csv(rep_data, "data/rep_data.csv")
