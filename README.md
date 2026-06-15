# Replication lab

## **What is this?** 

A ready-to-run replication exercise for the workshop "Applied replication for data skills" (Open Research Conference, Newcastle University, 16 June 2026). You reproduce one of the reanalyses that were part of the Multi100 project, whose results were recently published in *Nature* (Aczel et al. 2026), and test the robustness of a published claim from Teney (2016) – using the same data, tools, and workflow the Multi100 project used. The report you build (`index.qmd`) renders to a website you publish yourself; every task is a small, git-committable step that doubles as a git lesson.

> The template renders out of the box: the data ship inside `data/`, so your very first `quarto preview` works with no edits and no network.

------------------------------------------------------------------------

## In the room: three steps

1.  **Make it yours.** On this template's GitHub page, click the green **Use this template → Create a new repository**. Name it `replication-lab`, set it to **Public** (public repos get free Pages and free Actions minutes – and you keep a portfolio piece), and create it under *your own* account.

2.  **Clone it in Positron (or another IDE, but then make sure it's all optimised for R work).** In Positron: **File → New Folder from Git…**, paste your new repo's URL, and pick a local folder. Sign in to GitHub through the **Accounts** menu (browser OAuth – no tokens needed). If you have never set your git identity, run once in the terminal:

    ``` sh
    git config --global user.name  "Your Name"
    git config --global user.email "you@example.com"
    ```

3.  **Preview the report.** In the Positron terminal:

    ``` sh
    quarto preview
    ```

    The skeleton report opens in a browser. That is your starting point – now work through Tasks A–F inside `index.qmd`.

> **Publishing (one-time):** in your repo's **Settings → Pages → Source**, choose **"GitHub Actions"**. From then on, every push rebuilds and publishes your report to `https://<your-username>.github.io/replication-lab/`, free.

------------------------------------------------------------------------

## Skills passport

Tick a box when you have done the thing – and tick it *with a commit* (edit this file, commit, push), so even the checklist helps you practice git. The ten boxes map to the workshop's learning objectives.

- [ ] **1. The three Rs.** I can distinguish reproducibility, robustness, and replicability – and say why the distinction matters. *(Task A)*
- [ ] **2. Repo from a template.** I created this repository from the template and cloned it in Positron. *(Steps 1–2 above)*
- [ ] **3. Data like a researcher.** I retrieved the published data programmatically from OSF (`R/get_data.R`). *(Task B)*
- [ ] **4. Small, frequent commits.** I have made at least five commits with meaningful messages. *(throughout)*
- [ ] **5. State the estimand.** I wrote the claim's estimand: a unit-specific quantity plus a target population. *(Task C)*
- [ ] **6. Encode assumptions in a DAG.** I completed the DAG and derived an adjustment set with `adjustmentSets()`. *(Task C)*
- [ ] **7. Preregister before running.** I committed my analytical choice *before* running it, with a `prereg:` commit message. *(Task D2)*
- [ ] **8. Fit and report a model.** I fitted a panel model and reported it correctly (coefficient table + standardised result row). *(Tasks D1–D2)*
- [ ] **9. Publish a reproducible report.** I rendered the Quarto report and published it via GitHub Pages. *(Task E)*
- [ ] **10. Locate yourself in the multiverse.** I submitted my result via the `report_result()` link and can see my dot on the live multiverse among the others. *(Task E + debrief)*

------------------------------------------------------------------------

## Links

- **Workshop site:** https://codemoreh.github.io/applied-replication/
- **Submit your result:** run `report_result()` – it prints a one-click link that drops your result onto the live multiverse at https://codemoreh.github.io/applied-replication/results.html
- **Discussion (help & repo-URL gallery):** https://github.com/CodeMoreh/applied-replication/discussions – post your repository URL here, and ask for help if you get stuck.
- **The OSF analyst fork (Task B fetches from here):** https://osf.io/6zqct/ – the analyst's maintained fork, which adds the corrected April 2025 analysis. The official Multi100 archival record is at https://osf.io/8rtwe/.

------------------------------------------------------------------------

*Code is MIT-licensed; prose, figures, and data are CC BY 4.0 – see `LICENSE`.*