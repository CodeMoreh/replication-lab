# ============================================================================
# check_setup.R – does this machine have what the workshop needs?
# ----------------------------------------------------------------------------
# Run this BEFORE the day (or in the setup sprint) to find problems while there
# is still time to fix them. It prints an [OK] / [FAIL] / [SKIP] line per check
# and a verdict at the end. Nothing here changes your system – it only looks.
#
# Run it from a terminal:   Rscript R/check_setup.R
# or from the R console:     source("R/check_setup.R")
#
# A FAIL is not the end of the world: the workshop has a browser track that
# needs nothing but a web browser. The message at the bottom says what to do.
# ============================================================================

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

# --- 3. git on PATH + identity configured -----------------------------------
gv <- sys_out("git", "--version")
if (nzchar(gv) && grepl("git version", gv)) {
  ok(paste0("git on PATH (", gv, ")"))
  user_name  <- sys_out("git", c("config", "--global", "user.name"))
  user_email <- sys_out("git", c("config", "--global", "user.email"))
  if (nzchar(user_name))  ok(paste0("git user.name set ('",  user_name,  "')"))
  else fail("git user.name not set – run: git config --global user.name \"Your Name\"")
  if (nzchar(user_email)) ok(paste0("git user.email set ('", user_email, "')"))
  else fail("git user.email not set – run: git config --global user.email \"you@example.com\"")
} else {
  fail("git not found on PATH – install from https://git-scm.com and reopen the terminal")
}

# --- 4. packages ------------------------------------------------------------
needed <- c("dplyr", "readr", "ggplot2", "broom", "fixest", "plm",
            "dagitty", "ggdag", "modelsummary", "osfr")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) == 0) {
  ok(paste0("all ", length(needed), " required packages installed"))
} else {
  fail(paste0("missing package(s): ", paste(missing, collapse = ", ")))
  cat('        install with: install.packages(c("',
      paste(missing, collapse = '", "'), '"))\n', sep = "")
}

# --- 5. write access to the working directory -------------------------------
probe <- file.path(getwd(), ".setup_write_probe")
writable <- tryCatch({
  writeLines("probe", probe); unlink(probe); TRUE
}, error = function(e) FALSE)
if (isTRUE(writable)) {
  ok(paste0("write access to working directory (", getwd(), ")"))
} else {
  fail(paste0("cannot write to working directory (", getwd(),
              ") – move the project somewhere you own"))
}

# --- 6. optional network probe to OSF ---------------------------------------
# This is the only check that needs the internet, so a failure is a [SKIP],
# not a [FAIL] – the template renders offline regardless.
net_ok <- tryCatch({
  con <- url("https://osf.io", open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = 1L)
  TRUE
}, error = function(e) FALSE)
if (isTRUE(net_ok)) {
  ok("network reaches https://osf.io (OSF fetch will work)")
} else {
  skip("could not reach https://osf.io – offline is fine, data ships with the template")
}

# --- verdict ----------------------------------------------------------------
cat("\n=== Verdict ===\n")
cat(sprintf("  %d OK   %d FAIL   %d SKIP\n",
            .results$pass, .results$fail, .results$skip))

if (.results$fail == 0) {
  cat("\nYou are ready for the full pipeline track. See you there.\n")
} else {
  cat("\nSome checks failed. Two options:\n",
      " 1. Fix the FAILs above (each line says how), then re-run this script.\n",
      " 2. Come anyway – the workshop has a browser track that needs nothing\n",
      "    but a web browser, and your pair partner's laptop can drive the rest.\n",
      sep = "")
}
cat("\n")
