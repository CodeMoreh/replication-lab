# Replication lab

## **What is this?**

A ready-to-run replication exercise for the course "Applied Replication for Data Skills". You reproduce one of the reanalyses from the Multi100 project, whose results were recently published in *Nature* (Aczel et al. 2026), and test the robustness of a published claim from the target article – using the same data, tools, and workflow that the project used. You are analyst no. 6, and the code you work with is the coordinator's own submitted analysis.

The report you build is `index.qmd`. It renders out of the box: the data ship inside `data/`, so your very first `quarto preview` works with no edits and no network.

------------------------------------------------------------------------

## Setting up the workspace

This workspace is a plain folder, with no version control and no account to sign in to. You download it, unzip it, and work inside it.

1.  **Download the workspace.** On the GitHub page of the template, click the green **Code** button, then **Download ZIP** – or use the direct link `https://github.com/CodeMoreh/replication-lab/archive/refs/heads/main.zip`. Save it somewhere you own (Desktop or Documents).

2.  **Unzip it.** Extract the ZIP to a real folder (on Windows, "Extract All" – do not work inside the compressed preview). You will get a folder called `replication-lab-main` containing `index.qmd`, `data/`, `R/`, and this file.

3.  **Open the folder in your IDE.** In Positron or VS Code, **File → Open Folder…** and pick the unzipped folder; in RStudio, open `replication-lab.Rproj`, which sits inside it. (Positron is the one that the walkthrough uses on the shared screen; any of the three works for every step.)

4.  **Restore the packages.** This workspace has no lockfile that pins package versions. Instead, `R/check_setup.R` checks what is installed and lists what is missing. In the R console:

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

Everything above runs on the country-year panel in `data/`. The respondent-level data behind it are sixteen Eurobarometer surveys that GESIS distributes under terms that do not allow this workspace to include them, so they are downloaded under your own free GESIS account, into a folder outside the workspace. The course site lists the sixteen studies and walks through the download at https://codemoreh.github.io/applied-replication/companion/data-sources.html#sec-gesis. Two scripts then do the rest:

- `prep/01_build_respondents.R` – set the folder path at the top, source it once. It checks every file against the version and checksum of the copy from which the course was built, reads the sixteen, harmonises the respondent characteristics, and writes `data_private/eb_respondents.rds`. That folder is ignored by git and must stay so: the file is licensed microdata, never to be shared or opened in a session that an AI assistant can read.
- `prep/02_scales_and_panel.R` – read and run section by section. It builds the framing scales under the three scoring rules, averages respondents into country-years, merges the macro series, reproduces `data/EUframes_cy.csv` to machine precision, and builds the panel by education band.

The two scripts under `prep/` that begin "EXHIBIT ONLY" are the historical builds, kept for reading.

------------------------------------------------------------------------

## A note on version control

This folder is kept free of git for the session. There is nothing to commit or push and no account to set up, so everyone in the class can get to the analysis in minutes, whatever their machine.

The workspace *came from* a GitHub repository (`CodeMoreh/replication-lab`), and putting your own work under version control – and publishing the rendered report to a live URL – is the natural take-home step. The course site has a self-study companion module that goes through it click by click: **Git and GitHub self-study** at <https://codemoreh.github.io/applied-replication/companion/git-github.html>. The `.github/` and `.devcontainer/` folders already in this workspace set up that route once the folder is a repository again. You can ignore them for the session.

------------------------------------------------------------------------

## What you will do today

The tasks in `index.qmd` map to the day's learning objectives:

- **The three Rs.** Distinguish reproducibility, robustness, and replicability – and say why the distinction matters. *(Task 1)*
- **Meet the data.** Read the country-year panel and look at the outcome over time. *(Task 2)*
- **State the estimand and encode it in a DAG.** Write the target quantity, then derive an adjustment set with `adjustmentSets()`. *(Task 2, continued)*
- **Reproduce the recorded result.** Fit the two-way fixed-effects model on the panel and confirm **t = −3.804**, the value recorded for analyst C6HJR in the Multi100 dataset. *(Task 3)*
- **Work the specification menu.** Fit alternative specifications with `try_spec()`, compare them, and submit the one you would defend to the class chart with `submit_exploration()`, where it is labelled exploratory. *(Task 4)*
- **Preregister, then build and run.** Complete the preregistration block, then translate it into a real fit and read the claim-aligned result. *(Task 5)*
- **Locate yourself in the multiverse.** Submit your result with `report_result()` and find your circles among the others on the class chart by the tag that both submission functions print. *(Task 5, continued)*
- **Go further.** Sweep a whole axis, test the mirror-image expectation, use GDP growth as the predictor, or read the full SCORE dossier. *(Going further)*

------------------------------------------------------------------------

## Links

- **Course site:** https://codemoreh.github.io/applied-replication/
- **Submit your results:** `submit_exploration()` in Task 4 and `report_result()` in Task 5 each print a link to a pre-filled Google Form; open it and press **Send**, and the result appears on the live multiverse chart at https://codemoreh.github.io/applied-replication/cases/eu-frames/multiverse.html
- **Take-home git/GitHub path:** https://codemoreh.github.io/applied-replication/companion/git-github.html
- **The OSF analyst fork:** https://osf.io/6zqct/ – the analyst's maintained fork, which holds the working materials and the panel that this workspace uses. The official Multi100 archival record is at https://osf.io/8rtwe/.

------------------------------------------------------------------------

## Notes for the repository maintainer

This workspace is published as the "Use this template" repository `CodeMoreh/replication-lab`. Two folders serve the take-home and publishing route:

- `.devcontainer/devcontainer.json` – a Codespaces / dev-container image (R + Quarto + the package list) so the repo can be opened and rendered in the cloud with no local install.
- `.github/workflows/publish.yml` – renders the report and publishes it to GitHub Pages on every push, once **Settings → Pages → Source** is set to **"GitHub Actions"**. It installs R and re-runs the chunks, so a participant's edits show up.

Neither is needed for the zip route used in class. Keep `data/` in sync with the `data/` folder of the course repository: `EUframes_cy.csv` and its codebook are what the report reads directly. `teney_panel_measurement.csv` and `predictor_measures.csv` are the measurement variants of the panel and the published predictor series from which `R/spec_tools.R` fits every specification. `spec_curve.csv` (with `spec_curve_codebook.md`) is the curve of 864 specifications, from which the helpers read the placement of each fit and against which they check it, and `menu_options.csv` gives the reason for every option that is not on the curve. Each live fit computes its CR2 errors clustered by country, with Satterthwaite degrees of freedom, through the `clubSandwich` package, the rule that classes the curve, so it returns the stored p-value exactly. `teney_panel_weights.csv` is read by the two scripts in `prep/`.

------------------------------------------------------------------------

*Code is MIT-licensed; prose, figures, and data are CC BY 4.0 – see `LICENSE`.*
