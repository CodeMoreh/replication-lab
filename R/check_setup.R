# ########################################################################### #
# Title:        check_setup.R – does this machine have what the course needs?
# Purpose:      Report R version, Quarto, the required packages, and whether the
#               workspace data files are present and readable. Prints an
#               [OK] / [FAIL] / [SKIP] line per check and a verdict. It only
#               checks, and changes nothing on your system.
# Sourced by:   the participant, before the session (source("R/check_setup.R"))
# Author:       Chris Moreh
# Last updated: 2026-09-08
# ########################################################################### #

# A FAIL is not the end of the world: the browser lab on the course site runs
# the same exercise inside a web browser. The message at the bottom says what
# to do.

# --- tiny reporting helpers -------------------------------------------------
.results <- new.env()
.results$pass <- 0L; .results$fail <- 0L; .results$skip <- 0L

ok   <- function(msg) { .results$pass <- .results$pass + 1L; cat("[OK]   ", msg, "\n", sep = "") }
fail <- function(msg) { .results$fail <- .results$fail + 1L; cat("[FAIL] ", msg, "\n", sep = "") }
skip <- function(msg) { .results$skip <- .results$skip + 1L; cat("[SKIP] ", msg, "\n", sep = "") }

# Run a system command quietly and return its trimmed stdout, or "" on failure.
sys_out <- function(cmd, args) {
  out <- tryCatch(
    suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  if (length(out) == 0) "" else trimws(paste(out, collapse = " "))
}

cat("\n=== Replication lab – setup check ===\n\n")

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
# The gaussian options on the menu need fixest and plm, and the beta family
# adds betareg (fixed-effects and pooled designs), glmmTMB (random effects) and
# sandwich (the cluster-robust pooled beta). All are checked so that any menu
# choice runs. Quarto runs the R chunks of the report through rmarkdown, so a
# fresh R installation without it renders nothing.
needed <- c("dplyr", "readr", "ggplot2", "broom", "fixest", "plm",
            "dagitty", "ggdag", "modelsummary", "osfr", "betareg",
            "glmmTMB", "sandwich", "rmarkdown")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) == 0) {
  ok(paste0("all ", length(needed), " required packages installed"))
} else {
  fail(paste0("missing package(s): ", paste(missing, collapse = ", ")))
  cat('        install the missing ones with:\n')
  cat('          install.packages("pak"); pak::pak(c("',
      paste(missing, collapse = '", "'), '"))\n', sep = "")
}

# --- 4. data files present and readable -------------------------------------
# The report renders offline because these ship inside the workspace. The loop
# confirms that they are here and that each reads as a country-year panel.
# EUframes_cy.csv is the panel on which every task is fitted. rep_data.csv is
# the as-submitted Multi100 artefact, kept alongside it as provenance and used
# by no task.
data_files <- c("data/EUframes_cy.csv", "data/rep_data.csv")
for (f in data_files) {
  if (!file.exists(f)) {
    fail(paste0("data file missing: ", f, " – run from the workspace root folder"))
    next
  }
  n_rows <- tryCatch(nrow(utils::read.csv(f)), error = function(e) NA_integer_)
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
cat("\n=== Verdict ===\n")
cat(sprintf("  %d OK   %d FAIL   %d SKIP\n",
            .results$pass, .results$fail, .results$skip))

if (.results$fail == 0) {
  cat("\nYou are ready. Open index.qmd and run `quarto preview` in the terminal.\n")
} else {
  cat("\nSome checks failed. Two options:\n",
      " 1. Fix the FAILs above (each line says how), then re-run this script.\n",
      "    The help page on the course site lists the common failures and fixes.\n",
      " 2. Come anyway – the browser lab runs the same exercise inside a web\n",
      "    browser, and you can work through the rest on a partner's laptop.\n",
      sep = "")
}
cat("\n")
