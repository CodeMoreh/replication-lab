# Replication lab

## **What is this?**

A ready-to-run replication exercise for the workshop "Applied Replication for Data Skills". You reproduce one of the reanalyses from the Multi100 project, whose results were recently published in *Nature* (Aczel et al. 2026), and test the robustness of a published claim from the target article – using the same data, tools, and workflow the project used. You are analyst no. 6, and the code you work with is the coordinator's own submitted analysis.

The report you build is `index.qmd`. It renders out of the box: the data ship inside `data/`, so your very first `quarto preview` works with no edits and no network.

------------------------------------------------------------------------

## In the room: five steps

This workspace is a plain folder – no accounts, no version control, nothing to sign in to. You download it, unzip it, and work inside it.

1.  **Download the workspace.** On the template's GitHub page, click the green **Code** button, then **Download ZIP** – or use the direct link `https://github.com/CodeMoreh/replication-lab/archive/refs/heads/main.zip`. Save it somewhere you own (Desktop or Documents).

2.  **Unzip it.** Extract the ZIP to a real folder (on Windows, "Extract All" – do not work inside the compressed preview). You will get a folder called `replication-lab-main` containing `index.qmd`, `data/`, `R/`, and this file.

3.  **Open the folder in Positron.** **File → Open Folder…** and pick the unzipped folder. (Any IDE set up for R works, but Positron is the one the walkthrough uses.)

4.  **Restore the packages.** This workspace does not pin versions with a lockfile – instead, `R/check_setup.R` tells you exactly what you need. In the R console:

    ``` r
    source("R/check_setup.R")
    ```

    It prints an `[OK]` / `[FAIL]` line per check (R version, Quarto, the required packages, the data files). If any packages are missing, it prints the one line that installs them. Run that line, then re-run the check.

5.  **Open the report and preview it.** Open `index.qmd`, then in the Positron terminal:

    ``` sh
    quarto preview
    ```

    The skeleton report opens in a browser. That is your starting point – now work through the tasks inside `index.qmd`.

------------------------------------------------------------------------

## A note on version control

This folder is deliberately git-free for the session. There is nothing to commit, nothing to push, and no account to set up – so the whole room can get to the analysis in minutes, whatever their setup.

The workspace *came from* a GitHub repository (`CodeMoreh/replication-lab`), and putting your own work under version control – and publishing the rendered report to a live URL – is the natural take-home step. The workshop site has a self-study companion module that walks it click by click: **Git and GitHub self-study** at <https://codemoreh.github.io/applied-replication/companion/git-github.html>. The `.github/` and `.devcontainer/` folders already in this workspace are what make that path work once the folder is a repository again. You can ignore them for the session.

------------------------------------------------------------------------

## What you will do today

The tasks in `index.qmd` map to the day's learning objectives:

- **The three Rs.** Distinguish reproducibility, robustness, and replicability – and say why the distinction matters. *(Task 1)*
- **Meet the data.** Read the corrected country-year panel and look at the outcome over time. *(Task 2)*
- **Reproduce the recorded result.** Fit the two-way fixed-effects model on the corrected panel and confirm **t = −3.804**, the value on record for this analyst. A collapsed callout tells the story of the *other* number in the trail, −3.853, and how the analyst caught and corrected it. *(Task 3)*
- **State the estimand and encode it in a DAG.** Write the target quantity, then derive an adjustment set with `adjustmentSets()`. *(Task 4)*
- **Preregister, then build and run.** Complete the preregistration block, then translate it into a real fit and read the claim-aligned result. *(Tasks 5–6)*
- **Locate yourself in the multiverse.** Submit your result with `report_result()` and watch your dot land among the others. *(Task 7)*
- **Go further.** A second, exploratory specification, the mirror-image expectation, GDP growth as the predictor, or the full SCORE dossier. *(Going further)*

------------------------------------------------------------------------

## Links

- **Workshop site:** https://codemoreh.github.io/applied-replication/
- **Submit your result:** run `report_result()` – it prints a one-click link that drops your result onto the live multiverse at https://codemoreh.github.io/applied-replication/results.html
- **Take-home git/GitHub path:** https://codemoreh.github.io/applied-replication/companion/git-github.html
- **The OSF analyst fork:** https://osf.io/6zqct/ – the analyst's maintained fork, which adds the corrected April 2025 analysis. The official Multi100 archival record is at https://osf.io/8rtwe/.

------------------------------------------------------------------------

## Notes for the repository maintainer

This workspace is published as the "Use this template" repository `CodeMoreh/replication-lab`. The two folders that serve the take-home / publish path:

- `.devcontainer/devcontainer.json` – a Codespaces / dev-container image (R + Quarto + the package list) so the repo can be opened and rendered in the cloud with no local install.
- `.github/workflows/publish.yml` – renders the report and publishes it to GitHub Pages on every push, once **Settings → Pages → Source** is set to **"GitHub Actions"**. It installs R and re-runs the chunks, so a participant's edits show up.

Neither is needed for the in-room zip route. Keep `data/` and `data/EUframes_cy_codebook.md` in sync with the workshop repository's `data/`.

------------------------------------------------------------------------

*Code is MIT-licensed; prose, figures, and data are CC BY 4.0 – see `LICENSE`.*
