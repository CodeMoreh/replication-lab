# Replication lab

## **What is this?**

A ready-to-run replication exercise for the workshop "Applied Replication for Data Skills". You reproduce one of the reanalyses from the Multi100 project, whose results were recently published in *Nature* (Aczel et al. 2026), and test the robustness of a published claim from the target article – using the same data, tools, and workflow that the project used. You are analyst no. 6, and the code you work with is the coordinator's own submitted analysis.

The report you build is `index.qmd`. It renders out of the box: the data ship inside `data/`, so your very first `quarto preview` works with no edits and no network.

------------------------------------------------------------------------

## In the room: five steps

This workspace is a plain folder – no accounts, no version control, nothing to sign in to. You download it, unzip it, and work inside it.

1.  **Download the workspace.** On the GitHub page of the template, click the green **Code** button, then **Download ZIP** – or use the direct link `https://github.com/CodeMoreh/replication-lab/archive/refs/heads/main.zip`. Save it somewhere you own (Desktop or Documents).

2.  **Unzip it.** Extract the ZIP to a real folder (on Windows, "Extract All" – do not work inside the compressed preview). You will get a folder called `replication-lab-main` containing `index.qmd`, `data/`, `R/`, and this file.

3.  **Open the folder in your IDE.** In Positron or VS Code, **File → Open Folder…** and pick the unzipped folder; in RStudio, open `replication-lab.Rproj`, which sits inside it. (Positron is the one that the walkthrough uses on the shared screen; any of the three works for every step.)

4.  **Restore the packages.** This workspace does not pin versions with a lockfile – instead, `R/check_setup.R` tells you exactly what you need. In the R console:

    ``` r
    source("R/check_setup.R")
    ```

    It prints an `[OK]` / `[FAIL]` line per check (R version, Quarto, the required packages, the data files). If any packages are missing, it prints the one line that installs them. Run that line, then re-run the check.

5.  **Open the report and preview it.** Open `index.qmd`, then in a terminal inside your IDE:

    ``` sh
    quarto preview
    ```

    The skeleton report opens in a browser. That is your starting point – now work through the tasks inside `index.qmd`.

------------------------------------------------------------------------

## The Eurobarometer files, for the individual-level work

Everything above runs on the country-year panel in `data/`. The respondent-level data behind it are sixteen Eurobarometer surveys that GESIS distributes under terms that do not allow this workspace to carry them, so they are downloaded under your own free GESIS account, into a folder outside the workspace. The course site lists the sixteen studies and walks through the download at https://codemoreh.github.io/applied-replication/companion/data-sources.html#sec-gesis. Two scripts then do the rest:

- `prep/01_build_respondents.R` – set the folder path at the top, source it once. It checks every file against the version and checksum the course was built from, reads the sixteen, harmonises the respondent characteristics, and writes `data_private/eb_respondents.rds`. That folder is ignored by git and must stay so: the file is licensed microdata, never to be shared or opened in a session an AI assistant can read.
- `prep/02_scales_and_panel.R` – read and run section by section. It builds the framing scales under the three scoring rules, averages respondents into country-years, merges the macro series, reproduces `data/EUframes_cy.csv` to machine precision, and builds the panel by education band.

The two scripts under `prep/` that begin "EXHIBIT ONLY" are the historical builds, kept for reading.

------------------------------------------------------------------------

## A note on version control

This folder is deliberately git-free for the session. There is nothing to commit or push, and no account to set up – so the whole room can get to the analysis in minutes, whatever their setup.

The workspace *came from* a GitHub repository (`CodeMoreh/replication-lab`), and putting your own work under version control – and publishing the rendered report to a live URL – is the natural take-home step. The workshop site has a self-study companion module that walks it click by click: **Git and GitHub self-study** at <https://codemoreh.github.io/applied-replication/companion/git-github.html>. The `.github/` and `.devcontainer/` folders already in this workspace are what make that path work once the folder is a repository again. You can ignore them for the session.

------------------------------------------------------------------------

## What you will do today

The tasks in `index.qmd` map to the day's learning objectives:

- **The three Rs.** Distinguish reproducibility, robustness, and replicability – and say why the distinction matters. *(Task 1)*
- **Meet the data.** Read the country-year panel and look at the outcome over time. *(Task 2)*
- **State the estimand and encode it in a DAG.** Write the target quantity, then derive an adjustment set with `adjustmentSets()`. *(Task 2, continued)*
- **Reproduce the recorded result.** Fit the two-way fixed-effects model on the panel and confirm **t = −3.804**, the value on record for this analyst. *(Task 3)*
- **Work the specification menu.** Fit alternative specifications with `try_spec()`, compare them, and send the one you would defend to the class chart as an exploratory result. *(Task 4)*
- **Preregister, then build and run.** Complete the preregistration block, then translate it into a real fit and read the claim-aligned result. *(Task 5)*
- **Locate yourself in the multiverse.** Submit your result with `report_result()` and watch your dot land among the others. *(Task 5, continued)*
- **Go further.** Sweep a whole axis, the mirror-image expectation, GDP growth as the predictor, or the full SCORE dossier. *(Going further)*

------------------------------------------------------------------------

## Links

- **Workshop site:** https://codemoreh.github.io/applied-replication/
- **Submit your result:** run `report_result()` – it prints a one-click link that drops your result onto the live multiverse at https://codemoreh.github.io/applied-replication/results.html
- **Take-home git/GitHub path:** https://codemoreh.github.io/applied-replication/companion/git-github.html
- **The OSF analyst fork:** https://osf.io/6zqct/ – the analyst's maintained fork, carrying the working materials and the panel that this workspace uses. The official Multi100 archival record is at https://osf.io/8rtwe/.

------------------------------------------------------------------------

## Notes for the repository maintainer

This workspace is published as the "Use this template" repository `CodeMoreh/replication-lab`. The two folders that serve the take-home / publish path:

- `.devcontainer/devcontainer.json` – a Codespaces / dev-container image (R + Quarto + the package list) so the repo can be opened and rendered in the cloud with no local install.
- `.github/workflows/publish.yml` – renders the report and publishes it to GitHub Pages on every push, once **Settings → Pages → Source** is set to **"GitHub Actions"**. It installs R and re-runs the chunks, so a participant's edits show up.

Neither is needed for the in-room zip route. Keep `data/` in sync with the `data/` folder of the course repository: `EUframes_cy.csv` and its codebook are what the report reads directly; `teney_panel_measurement.csv`, `teney_panel_weights.csv` and `predictor_measures.csv` are the measurement variants that `R/spec_tools.R` fits from when a Level 2 choice departs from the baseline; `spec_grid_mi.csv` holds the pooled multiple-imputation results, which cannot be fitted here because they need the respondent-level file.

------------------------------------------------------------------------

*Code is MIT-licensed; prose, figures, and data are CC BY 4.0 – see `LICENSE`.*
