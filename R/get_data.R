# ============================================================================
# get_data.R – fetch the published rep_data.csv the way a researcher would
# ----------------------------------------------------------------------------
# NOTE: rep_data.csv already ships inside this template's data/ folder, so the
# report renders offline without ever running this script. This file is the
# OSF-access EXERCISE, not a hard dependency – run it to practise pulling a
# real published artefact from the Open Science Framework. If every remote
# step fails (no network, OSF down), the committed copy is used and the report
# still works.
#
# The fetch chain, most "proper" first, each step announced with a message():
#   1. file already present  -> nothing to do
#   2. osfr: node 6zqct -> moreh_rep/data -> download rep_data.csv   (the analyst's fork)
#   3. direct OSF download URL (fork copy)  https://osf.io/download/6a2c1567411ddb2ea1b9945d/
#   4. direct OSF download URL (original)   https://osf.io/download/t27r6/
#   5. workshop site mirror     https://codemoreh.github.io/applied-replication/data/rep_data.csv
#
# Note on OSF nodes: the analyst's fork is osf.io/6zqct ("Multi100 | Teney_EurSocioRev_2016 |
# Analyst_C6HJR"), maintained for educational use. The official Multi100 archival record
# remains osf.io/8rtwe – both point to byte-identical rep_data.csv files.
#
# Usage:
#   source("R/get_data.R")   # writes/ensures data/rep_data.csv
# ============================================================================

get_data <- function(dest = "data/rep_data.csv") {

  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)

  # --- Step 1: already present? ---------------------------------------------
  if (file.exists(dest)) {
    message("[1/4] '", dest, "' already present – nothing to do.")
    return(invisible(dest))
  }

  # --- Step 2: the proper way – the osfr package ----------------------------
  # osfr talks to the OSF API. Node 6zqct is the analyst's fork (maintained for
  # educational use); the file lives under the 'moreh_rep/data' path inside it.
  # Anonymous access works for public nodes, so no OSF login is needed.
  message("[2/5] Trying osfr (OSF fork 6zqct -> moreh_rep/data/rep_data.csv) …")
  ok <- tryCatch({
    if (!requireNamespace("osfr", quietly = TRUE)) {
      stop("osfr not installed")
    }
    node  <- osfr::osf_retrieve_node("6zqct")
    files <- osfr::osf_ls_files(node, path = "moreh_rep/data")
    hit   <- files[files$name == "rep_data.csv", ]
    if (nrow(hit) == 0) stop("rep_data.csv not found under moreh_rep/data")
    osfr::osf_download(hit, path = dirname(dest), conflicts = "overwrite")
    file.exists(dest)
  }, error = function(e) {
    message("      osfr route failed: ", conditionMessage(e))
    FALSE
  })
  if (isTRUE(ok)) {
    message("      success via osfr.")
    return(invisible(dest))
  }

  # --- Step 3: direct OSF download URL – fork copy --------------------------
  # The fork's rep_data.csv has a stable per-file download URL.
  message("[3/5] Trying the direct OSF download URL (fork copy) …")
  ok <- tryCatch({
    utils::download.file(
      "https://osf.io/download/6a2c1567411ddb2ea1b9945d/", dest,
      mode = "wb", quiet = TRUE)
    file.exists(dest) && file.size(dest) > 0
  }, error = function(e) {
    message("      fork direct URL failed: ", conditionMessage(e))
    FALSE
  })
  if (isTRUE(ok)) {
    message("      success via the fork direct URL.")
    return(invisible(dest))
  }

  # --- Step 4: direct OSF download URL – original component (t27r6) ---------
  # Every OSF file has a stable download link of the form osf.io/download/<id>/.
  # t27r6 is the file ID on the original official component 8rtwe (archival record).
  message("[4/5] Trying the direct OSF download URL (original osf.io/download/t27r6) …")
  ok <- tryCatch({
    utils::download.file("https://osf.io/download/t27r6/", dest,
                         mode = "wb", quiet = TRUE)
    file.exists(dest) && file.size(dest) > 0
  }, error = function(e) {
    message("      direct OSF URL failed: ", conditionMessage(e))
    FALSE
  })
  if (isTRUE(ok)) {
    message("      success via the direct OSF URL.")
    return(invisible(dest))
  }

  # --- Step 5: the workshop site mirror -------------------------------------
  # Last resort that does not touch OSF at all – a plain-HTTPS copy hosted on
  # the workshop's own GitHub Pages site.
  message("[5/5] Trying the workshop site mirror …")
  ok <- tryCatch({
    utils::download.file(
      "https://codemoreh.github.io/applied-replication/data/rep_data.csv",
      dest, mode = "wb", quiet = TRUE)
    file.exists(dest) && file.size(dest) > 0
  }, error = function(e) {
    message("      site mirror failed: ", conditionMessage(e))
    FALSE
  })
  if (isTRUE(ok)) {
    message("      success via the site mirror.")
    return(invisible(dest))
  }

  # --- All remote routes exhausted ------------------------------------------
  stop("get_data(): could not obtain rep_data.csv from any source. ",
       "If you are offline, the committed copy in data/ should already exist – ",
       "check that you are running from the repository root.")
}

# Run on source().
get_data()
