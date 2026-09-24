# ########################################################################### #
# Title:        dag_starter.R – the half-built DAG for the claim under test
# Purpose:      The object you complete in Task 2. It encodes the identification
#               assumptions behind the claim "national unemployment lowers
#               cosmopolitan EU framing": the causal path of interest and the
#               two assumed confounder pairs are drawn, and the more arguable
#               arrows are written into the same call but commented out.
#               Uncomment the ones you believe, re-source, and read the
#               adjustment set off the graph with dagitty::adjustmentSets().
# Sourced by:   index.qmd (Task 2)
# Author:       Chris Moreh
# Last updated: 2026-08-25
# ########################################################################### #

# A short vocabulary, because the whole exercise depends on these three terms:
#   confounder – a common cause of BOTH exposure and outcome; ADJUST for it,
#                because leaving it out biases the estimate.
#   mediator   – sits ON the path from exposure to outcome; do NOT adjust when
#                you want the TOTAL effect, since adjusting removes part of it.
#   collider   – a common EFFECT of two variables; adjusting OPENS a spurious
#                path, so leave colliders alone unless you know what you are
#                doing.

suppressPackageStartupMessages({
  library(dagitty)
  library(ggdag)
  library(ggplot2)
})

# --- Where the nodes sit ----------------------------------------------------
# The exposure is on the left, the outcome on the right, the two assumed
# confounders above and the arguable nodes below, so that an uncommented edge
# is drawn without crossing the rest of the figure. Coordinates for nodes that
# no arrow mentions are ignored until you bring the node into the graph.
coords <- list(
  x = c(unemployment = 0, framing = 3,
        country = 1, year = 2,
        growth = 1.5, bailout = 1.5, politicisation = 3),
  y = c(unemployment = 0, framing = 0,
        country = 1.4, year = 1.4,
        growth = 0.8, bailout = -1.2, politicisation = -0.9)
)

# --- The graph --------------------------------------------------------------
# Each arrow is an ordinary R formula, written EFFECT ~ CAUSE – the same
# direction you read a regression in, and the reverse of how an arrow is drawn.
# So `framing ~ unemployment` is the arrow unemployment -> framing. With one
# arrow per line, a leading # switches an arrow off and takes its comma with
# it. In Task 2 you switch arrows on and off in this way.
#
# A node exists only if some arrow mentions it, so the candidates below stay off
# the plot until you commit to one.
dag <- dagify(
  # The causal path of interest.
  framing ~ unemployment,

  # Country-constant traits (history, size, EU tenure) drive both the local
  # labour market and how people there frame the EU. These two arrows are the
  # first confounder pair.
  unemployment ~ country,
  framing ~ country,

  # EU-wide shocks in a given year, such as the crisis and other common events,
  # move both, and these two arrows are the second confounder pair. Country and
  # year fixed effects absorb both pairs, which is why adjustmentSets() returns
  # { country, year } and why the two-way fixed-effects model in Task 3 IS that
  # adjustment set.
  unemployment ~ year,
  framing ~ year,

  # ----- Arrows you may or may not believe (TASK 2) -----
  # Each one is a substantive claim about the world, not a modelling detail. Take
  # the # off the arrows you hold, re-source this file, and check how the
  # adjustment set changes.

  # GROWTH as a TIME-VARYING confounder: a slump raises unemployment and may sour
  # EU sentiment in the same year, opening a back-door path that fixed effects
  # cannot close, because they remove only country-constant and year-common
  # variation.
  # unemployment ~ growth,
  # framing ~ growth,

  # BAILOUT as a MEDIATOR: unemployment triggers a financial-assistance
  # programme, which then shapes framing. Adjusting for a mediator strips out
  # part of the very effect you are after, so leave it unadjusted if your
  # estimand is the total effect.
  # bailout ~ unemployment,
  # framing ~ bailout,

  # POLITICISATION of the EU by parties and elites as a cause of framing alone,
  # with no route into unemployment. Multi100 instructed analysts to disregard
  # it; is that defensible here?
  # framing ~ politicisation,

  # The year and the country driving growth itself. This only changes anything
  # once the two growth arrows above are also uncommented, and then it makes
  # growth a descendant of the things that the fixed effects already absorb.
  # growth ~ year,
  # growth ~ country,

  exposure = "unemployment",
  outcome  = "framing",
  coords   = coords
)

# --- Draw it ----------------------------------------------------------------
# One call for the report. Node names are too long to fit inside circles, so
# each node is drawn as a labelled box. Colour marks the exposure and the
# outcome, and every other node is grey. Returns a ggplot object.
plot_dag <- function(d = dag) {
  ggdag::tidy_dagitty(d) |>
    ggdag::node_status() |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggdag::geom_dag_edges(
      edge_colour = "grey45",
      arrow_directed = grid::arrow(length = grid::unit(7, "pt"),
                                   type = "closed")) +
    # Labelled boxes, because a name like "unemployment" does not fit inside
    # any circle small enough to leave the arrows visible.
    ggplot2::geom_label(
      ggplot2::aes(label = name, fill = status),
      colour = "white", fontface = "bold", size = 3.4, label.size = 0,
      label.r = grid::unit(4, "pt"), label.padding = grid::unit(5, "pt")) +
    ggplot2::scale_fill_manual(
      values = c(exposure = "#1b6ca8", outcome = "#d1495b"),
      na.value = "grey55", breaks = c("exposure", "outcome"),
      labels = c("exposure", "outcome"), name = NULL) +
    # Without the override, the legend keys show the placeholder letter "a"
    # that geom_label draws.
    ggplot2::guides(fill = ggplot2::guide_legend(
      override.aes = list(label = ""))) +
    ggdag::theme_dag() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.25)) +
    ggplot2::theme(legend.position = "bottom")
}
