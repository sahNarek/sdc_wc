#' Pitch markings only (no coordinate system — caller adds scale/coord)
draw_pitch_markings <- function(colour = "white", linewidth = 0.6) {
  list(
    annotate(
      "rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80,
      fill = NA, colour = colour, linewidth = linewidth
    ),
    annotate(
      "rect", xmin = 0, xmax = 60, ymin = 0, ymax = 80,
      fill = NA, colour = colour, linewidth = linewidth
    ),
    annotate(
      "rect", xmin = 0, xmax = 18, ymin = 18, ymax = 62,
      fill = NA, colour = colour, linewidth = linewidth
    ),
    annotate(
      "rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62,
      fill = NA, colour = colour, linewidth = linewidth
    ),
    annotate(
      "rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50,
      fill = NA, colour = colour, linewidth = linewidth
    ),
    annotate(
      "rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50,
      fill = NA, colour = colour, linewidth = linewidth
    ),
    annotate(
      "segment", x = 60, xend = 60, y = 0, yend = 80,
      colour = colour, linewidth = linewidth
    ),
    annotate("point", x = 12, y = 40, colour = colour, size = 1.05),
    annotate("point", x = 108, y = 40, colour = colour, size = 1.05),
    annotate("point", x = 60, y = 40, colour = colour, size = 1.05),
    annotate(
      "path",
      colour = colour, linewidth = linewidth,
      x = 60 + 10 * cos(seq(0, 2 * pi, length.out = 200)),
      y = 40 + 10 * sin(seq(0, 2 * pi, length.out = 200))
    ),
    annotate(
      "path",
      x = 12 + 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
      y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
      colour = colour, linewidth = linewidth
    ),
    annotate(
      "path",
      x = 108 - 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
      y = 40 - 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)),
      colour = colour, linewidth = linewidth
    )
  )
}

#' Draw StatsBomb pitch using ggplot annotate (Working-with-R.pdf style)
draw_pitch_sb <- function(
    colour = "black",
    fill = NA,
    linewidth = 0.6
) {
  c(
    draw_pitch_markings(colour = colour, linewidth = linewidth),
    list(
      theme(rect = element_blank(), line = element_blank()),
      scale_y_reverse(),
      coord_fixed(ratio = 105 / 100)
    )
  )
}

draw_pitch_half_attacking <- function(colour = "black", linewidth = 0.6) {
  c(
    draw_pitch_sb(colour = colour, linewidth = linewidth),
    list(coord_flip(xlim = c(85, 125)))
  )
}
