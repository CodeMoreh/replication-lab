##############################################################################
# Title:        dag_starter.R – the half-built DAG for the claim under test
# Purpose:      The object you complete in Task 2. It encodes the identification
#               assumptions behind the claim "national unemployment lowers
#               cosmopolitan EU framing": the causal path of interest and the
#               two assumed confounder pairs are drawn, and the more arguable
#               arrows are written into the same call but commented out.
#               Uncomment the ones you believe, re-source, and read the
#               adjustment set off the graph with dagitty::adjustmentSets().
# Author:       Chris Moreh
# Last updated: 2026-10-26
# Sourced by:   index.qmd (Task 2)
##############################################################################

# A short vocabulary, because the whole exercise depends on these three terms:
#   confounder – a common cause of BOTH exposure and outcome; ADJUST for it,
#                because leaving it out biases the estimate.
#   mediator   – sits ON the path from exposure to outcome; do NOT adjust when
#                you want the TOTAL effect, since adjusting removes part of it.
#   collider   – a common EFFECT of two variables; adjusting OPENS a spurious
#                path, so leave colliders alone unless you know what you are
#                doing.

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
dag <- ggdag::dagify(
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
# outcome, and every other node is grey. The canvas is a tinyplot (McDermott,
# Arel-Bundock and Zeileis, tinyplot, CRAN, doi:10.32614/CRAN.package.tinyplot),
# and the arrows and boxes are drawn on it with the base graphics under it.
plot_dag <- function(d = dag) {
  xy <- dagitty::coordinates(d)
  # only the nodes that some arrow mentions are in the graph; dplyr::mutate(),
  # because the columns are built from the arguments of this function
  nodes <- tibble::tibble(name = names(d)) |>
    dplyr::mutate(
      x = xy$x[name], y = xy$y[name],
      fill = dplyr::case_when(name %in% dagitty::exposures(d) ~ "#1b6ca8",
                              name %in% dagitty::outcomes(d) ~ "#d1495b",
                              .default = "grey55"))
  e <- dagitty::edges(d)
  from <- match(e$v, nodes$name)
  to <- match(e$w, nodes$name)
  tinyplot::tinyplot(y ~ x, data = nodes, type = "n", axes = FALSE, frame.plot = FALSE, grid = FALSE,
                     xlab = "", ylab = "", xlim = range(nodes$x) + c(-0.5, 0.5),
                     ylim = range(nodes$y) + c(-0.6, 0.3))
  # Labelled boxes, because a name like "unemployment" does not fit inside any
  # circle small enough to leave the arrows visible; each arrow stops where it
  # meets the box of the node that it points to, and starts where it leaves the
  # box of the node that it comes from.
  w <- graphics::strwidth(nodes$name, font = 2) / 2 + graphics::strwidth("m")
  h <- graphics::strheight(nodes$name, font = 2) / 2 + graphics::strheight("m") / 2
  dx <- nodes$x[to] - nodes$x[from]
  dy <- nodes$y[to] - nodes$y[from]
  cut_to <- pmin(w[to] / abs(dx), h[to] / abs(dy)) + 0.02
  cut_from <- pmin(w[from] / abs(dx), h[from] / abs(dy))
  graphics::arrows(nodes$x[from] + cut_from * dx, nodes$y[from] + cut_from * dy,
                   nodes$x[to] - cut_to * dx, nodes$y[to] - cut_to * dy,
                   length = 0.1, angle = 20, col = "grey45", lwd = 1.6)
  graphics::rect(nodes$x - w, nodes$y - h, nodes$x + w, nodes$y + h, col = nodes$fill, border = NA)
  graphics::text(nodes$x, nodes$y, nodes$name, col = "white", font = 2)
  graphics::legend("bottom", legend = c("exposure", "outcome"), fill = c("#1b6ca8", "#d1495b"),
                   border = NA, bty = "n", horiz = TRUE)
}
