##############################################################################
# Title:        get_data.R – fetch the EU-frames panel the way a researcher would
# Purpose:      EUframes_cy.csv already ships inside the data/ folder of this
#               workspace, so the report renders offline without this script.
#               It is an optional exercise in OSF access: sourcing it pulls a
#               real published artefact from the Open Science Framework, and
#               if every remote step fails (no network, OSF down) the
#               committed copy is used and the report still works. The steps
#               are tried in order, the most 'proper' first, and each is
#               announced with a message(): (1) the file is already present,
#               so nothing is done; (2) osfr fetches EUframes_cy.csv from the
#               main folder of OSF node 6zqct, the fork of the analyst; (3) the
#               mirror on the course site, https://codemoreh.github.io/
#               applied-replication/cases/eu-frames/data/EUframes_cy.csv.
#               The fork osf.io/6zqct ("Multi100 | Teney_EurSocioRev_2016 |
#               Analyst_C6HJR") is maintained for educational use; the
#               official Multi100 archival record is osf.io/8rtwe.
# Author:       Chris Moreh
# Last updated: 2026-10-26
# Sourced by:   index.qmd (Task 2, optional), which writes or confirms
#               data/EUframes_cy.csv
##############################################################################

get_data <- function(dest = "data/EUframes_cy.csv") {

  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)

  # --- Step 1: already present? ---------------------------------------------
  if (file.exists(dest)) {
    message("[1/3] '", dest, "' already present – nothing to do.")
    return(invisible(dest))
  }

  # --- Step 2: the proper way – the osfr package ----------------------------
  # osfr is an R client for the OSF API. Node 6zqct is the analyst's fork
  # (maintained for educational use), and EUframes_cy.csv is in its main
  # folder. Anonymous access works for public nodes, so no OSF login is needed.
  message("[2/3] Trying osfr (OSF fork 6zqct main folder -> EUframes_cy.csv) …")
  ok <- tryCatch({
    if (!requireNamespace("osfr", quietly = TRUE)) {
      stop("osfr not installed")
    }
    node  <- osfr::osf_retrieve_node("6zqct")
    files <- osfr::osf_ls_files(node)
    hit   <- files[files$name == "EUframes_cy.csv", ]
    if (nrow(hit) == 0) stop("EUframes_cy.csv not found in the main folder")
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

  # --- Step 3: the course site mirror ---------------------------------------
  # The last resort does not touch OSF at all. It downloads a plain-HTTPS copy
  # from the GitHub Pages site of the course, which serves the data folder of
  # the case verbatim.
  message("[3/3] Trying the course site mirror …")
  ok <- tryCatch({
    utils::download.file(
      "https://codemoreh.github.io/applied-replication/cases/eu-frames/data/EUframes_cy.csv",
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
  stop("get_data(): could not obtain EUframes_cy.csv from any source. ",
       "If you are offline, the committed copy in data/ should already exist – ",
       "check that you are running from the workspace root folder.")
}

# Run on source().
get_data()
