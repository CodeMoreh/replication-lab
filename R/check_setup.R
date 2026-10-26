##############################################################################
# Title:        check_setup.R – does this machine have what the course needs?
# Purpose:      Report R version, Quarto, the required packages, and whether the
#               workspace data files are present and readable. Prints an
#               [OK] / [FAIL] / [SKIP] line per check and a verdict. It only
#               checks, and changes nothing on your system. Base R throughout,
#               because it has to run before any package is installed.
# Author:       Chris Moreh
# Last updated: 2026-10-26
# Sourced by:   the participant, before the session (source("R/check_setup.R"))
##############################################################################

# A FAIL is not the end of the world: the browser lab on the course site runs
# the same exercise inside a web browser. The message at the bottom says what
# to do.

# --- tiny reporting helpers -------------------------------------------------
.results <- list2env(list(pass = 0L, fail = 0L, skip = 0L))
.tally <- function(kind, label, msg) {
  assign(kind, get(kind, envir = .results) + 1L, envir = .results)
  writeLines(paste0(label, msg))
}
ok   <- function(msg) .tally("pass", "[OK]   ", msg)
fail <- function(msg) .tally("fail", "[FAIL] ", msg)
skip <- function(msg) .tally("skip", "[SKIP] ", msg)

# Run a system command quietly and return its trimmed stdout, or "" on failure.
sys_out <- function(cmd, args) {
  out <- tryCatch(
    suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  if (length(out) == 0) "" else trimws(paste(out, collapse = " "))
}

writeLines("\n=== Replication lab – setup check ===\n")

# --- 1. R version -----------------------------------------------------------
if (getRversion() >= "4.4.0") {
  ok(paste0("R ", getRversion(), " (>= 4.4 required)"))
} else {
  fail(paste0("R ", getRversion(), " is below 4.4 – please update R"))
}

# --- 2. Quarto on PATH ------------------------------------------------------
qv <- sys_out("quarto", "--version")
if (nzchar(qv) && !grepl("not found|error", qv, ignore.case = TRUE)) {
  ok(paste0("Quarto on PATH (version ", qv, ")"))
} else {
  fail("Quarto not found on PATH – install from https://quarto.org and reopen the terminal")
}

# --- 3. packages ------------------------------------------------------------
# Every specification on the curve is fitted with fixest and tested with the
# CR2 errors of clubSandwich; effectsize converts its t to a partial r, and
# parameters prints the reproduced model. The data work uses datawizard,
# insight, dplyr, purrr, tibble and readr, the figures tinyplot, and the causal
# graph dagitty and ggdag. Quarto runs the R chunks of the report through
# rmarkdown, so a fresh R installation without it renders nothing.
needed <- c("readr", "datawizard", "insight", "dplyr", "purrr", "tibble", "fixest", "clubSandwich",
            "effectsize", "parameters", "tinyplot", "dagitty", "ggdag", "modelsummary",
            "osfr", "rmarkdown")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)] # r-style: keep – purrr may not be installed yet
if (length(missing) == 0) {
  ok(paste0("all ", length(needed), " required packages installed"))
} else {
  fail(paste0("missing package(s): ", paste(missing, collapse = ", ")))
  writeLines(c("        install the missing ones with:",
               paste0('          install.packages("pak"); pak::pak(c("', # r-style: keep – the instruction printed for a machine without pak
                      paste(missing, collapse = '", "'), '"))')))
}

# --- 4. data files present and readable -------------------------------------
# The report renders offline because these ship inside the workspace. The loop
# confirms that they are here and that each can be read. EUframes_cy.csv is the
# panel on which every task is fitted. The next four are what R/spec_tools.R
# reads: the measurement variants of the panel, the published predictor series,
# the curve of 864 specifications and the reasons for the options that are not
# on it. rep_data.csv is the as-submitted Multi100 artefact, kept as provenance
# and used by no task.
data_files <- c("data/EUframes_cy.csv", "data/teney_panel_measurement.csv",
                "data/predictor_measures.csv", "data/spec_curve.csv",
                "data/menu_options.csv", "data/rep_data.csv")
for (f in data_files) { # r-style: keep – each pass prints one check line
  if (!file.exists(f)) {
    fail(paste0("data file missing: ", f, " – run from the workspace root folder"))
    next
  }
  n_rows <- tryCatch(nrow(utils::read.csv(f)), error = function(e) NA_integer_) # r-style: keep – readr may not be installed yet
  if (is.na(n_rows)) {
    fail(paste0("could not read ", f))
  } else {
    ok(paste0(f, " present and readable (", n_rows, " rows)"))
  }
}

# The codebook that the tasks refer to.
if (file.exists("data/EUframes_cy_codebook.md")) {
  ok("data/EUframes_cy_codebook.md present")
} else {
  skip("data/EUframes_cy_codebook.md not found (optional reference)")
}

# --- 5. the report file -----------------------------------------------------
# index.qmd is what you open and work in. Its presence is the last check.
if (file.exists("index.qmd")) {
  ok("index.qmd present – open it to start")
} else {
  fail("index.qmd not found – run this from the workspace root folder")
}

# --- verdict ----------------------------------------------------------------
writeLines(c("\n=== Verdict ===",
             sprintf("  %d OK   %d FAIL   %d SKIP", .results$pass, .results$fail, .results$skip)))

if (.results$fail == 0) {
  writeLines("\nYou are ready. Open index.qmd and run `quarto preview` in the terminal.\n")
} else {
  writeLines(c("\nSome checks failed. Two options:",
               " 1. Fix the FAILs above (each line says how), then re-run this script.",
               "    The help page on the course site lists the common failures and fixes.",
               " 2. Come anyway – the browser lab runs the same exercise inside a web",
               "    browser, and you can work through the rest on a partner's laptop.",
               ""))
}
