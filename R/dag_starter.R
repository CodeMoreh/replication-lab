# ============================================================================
# dag_starter.R – a half-built causal DAG for the Teney claim
# ----------------------------------------------------------------------------
# This is the object you complete in Task C. It encodes the identification
# assumptions behind the claim "national unemployment lowers cosmopolitan EU
# framing". The causal path of interest and the two assumed confounders are
# already drawn; the more arguable arrows are present but COMMENTED OUT, each
# tagged "uncomment if you believe this:". Your job is to decide which beliefs
# you hold, uncomment the matching edges, re-source this file, and then run
# dagitty::adjustmentSets() to read off what you must adjust for.
#
# A one-line vocabulary, because the whole exercise turns on these three:
#   confounder – a common cause of BOTH exposure and outcome; ADJUST for it
#                (leaving it out biases the estimate).
#   mediator   – sits ON the path from exposure to outcome; do NOT adjust when
#                you want the TOTAL effect (adjusting removes part of the effect).
#   collider   – a common EFFECT of two variables; adjusting OPENS a spurious
#                path, so leave colliders alone unless you know what you are doing.
#
# Node names match the adjustmentSets() call in the report:
#   exposure = "unemployment", outcome = "framing".
# ============================================================================

suppressPackageStartupMessages({
  library(dagitty)
  library(ggdag)
  library(ggplot2)
})

# --- The DAG ----------------------------------------------------------------
# dagitty's text syntax: one edge per line, "cause -> effect". dagitty does
# NOT accept comments inside the dag{...} string, so the explanation lives in
# R comments out here and the arrows you may want to add are listed below the
# string as ready-to-paste lines.
#
# Node status in brackets ([exposure]/[outcome]) lets ggdag colour them.
#
# Edges already INCLUDED:
#   unemployment -> framing      the causal path of interest.
#   country -> unemployment      country-constant traits (history, size, EU
#   country -> framing           tenure …) drive both local unemployment and
#                                how people frame the EU – a confounder pair.
#   year -> unemployment         EU-wide shocks in a given year (the crisis,
#   year -> framing              common events) move both – the second pair.
# Together these two confounder pairs are exactly what country + year fixed
# effects soak up. Running adjustmentSets() on this graph returns { country,
# year } – the fixed effects ARE the adjustment set.
dag <- dagitty::dagitty('dag {
  unemployment   [exposure]
  framing        [outcome]
  country
  year
  growth
  bailout
  politicisation

  unemployment -> framing
  country -> unemployment
  country -> framing
  year -> unemployment
  year -> framing
}')

# --- Arrows you may or may not believe (TASK C) -----------------------------
# Each is a substantive claim about the world. If you hold one, add it to the
# graph and re-run adjustmentSets() to watch your adjustment set change. The
# tidy way to add an edge without rewriting the whole string:
#
#   dag <- dagitty::dagitty(paste0(sub("\\}\\s*$", "", dag),
#                                  "growth -> unemployment\n}"))
#
# or simply paste the line(s) you believe straight into the dag{...} string
# above and re-source this file.
#
# GROWTH as a TIME-VARYING confounder – a slump raises unemployment AND may
# sour EU sentiment in the same year (a back-door that fixed effects, which
# only remove country-constant and year-common variation, cannot close):
#     growth -> unemployment
#     growth -> framing
#
# BAILOUT as a MEDIATOR – unemployment triggers a financial-assistance
# programme, which then shapes framing; adjusting for a mediator removes part
# of the very effect you are after, so leave it out for the TOTAL effect:
#     unemployment -> bailout
#     bailout -> framing
#
# POLITICISATION of the EU by parties/elites as another cause of framing –
# Multi100 instructed analysts to disregard it; is that defensible here?
#     politicisation -> framing
#
# The time trend or the country driving growth itself:
#     year -> growth
#     country -> growth

# --- Layout coordinates for a clean ggdag plot ------------------------------
# Exposure left, outcome right, confounders above, the arguable nodes below –
# so uncommented edges read tidily rather than crossing the figure.
coords <- list(
  x = c(unemployment = 0, framing = 3,
        country = 1, year = 2,
        growth = 1.5, bailout = 1.5, politicisation = 3),
  y = c(unemployment = 0, framing = 0,
        country = 1.4, year = 1.4,
        growth = 0.8, bailout = -1.2, politicisation = -0.9)
)
dagitty::coordinates(dag) <- coords

# --- A small helper so the report can draw the DAG in one call --------------
# tidy_dagitty() + ggdag styling; colour by status (exposure/outcome) and
# keep labels readable. Returns a ggplot object.
plot_dag <- function(d = dag) {
  ggdag::ggdag_status(d, text = FALSE, use_labels = "name") +
    ggdag::theme_dag() +
    ggplot2::scale_colour_manual(
      values = c(exposure = "#1b6ca8", outcome = "#d1495b"),
      na.value = "grey70",
      breaks = c("exposure", "outcome"),
      name = NULL
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(legend.position = "bottom")
}
