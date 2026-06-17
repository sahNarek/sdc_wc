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

# StatsBomb goal mouth: y 36–44 (8 yd), goal line x = 120
GOAL_POST_Y_MIN <- 36
GOAL_POST_Y_MAX <- 44
GOAL_LINE_X <- 120
GOAL_NET_MIN_X <- 117

# Real-world goal dimensions (metres) for front-on net panel
GOAL_WIDTH_M <- 7.32
GOAL_HEIGHT_M <- 2.44

#' Tight goal-panel limits and aspect ratio for a wide rectangle (not square)
#'
#' StatsBomb coordinates map 1:1 to metres. \code{aspect.ratio} keeps patchwork
#' from stretching the panel when stacked in a portrait figure.
goal_panel_layout <- function(goal_width_m = GOAL_WIDTH_M,
                              goal_height_m = GOAL_HEIGHT_M,
                              x_pad = 0.15,
                              display_tallness = 2.35) {
  x_span <- goal_width_m + 2 * x_pad
  base_aspect <- goal_height_m / (x_span * (goal_width_m / goal_height_m))
  list(
    xlim = c(-x_pad, goal_width_m + x_pad),
    ylim = c(0, goal_height_m),
    coord_ratio = goal_width_m / goal_height_m,
    aspect_ratio = base_aspect * display_tallness
  )
}

#' Center the goal-mouth panel (SofaScore-style: ~55% figure width, not edge-to-edge)
wrap_centered_goal_panel <- function(goal_plot, width_frac = 0.58) {
  width_frac <- min(max(width_frac, 0.45), 0.75)
  side <- (1 - width_frac) / 2
  patchwork::wrap_plots(
    patchwork::plot_spacer(),
    goal_plot,
    patchwork::plot_spacer(),
    ncol = 3,
    widths = c(side, width_frac, side)
  )
}

#' Optional SVG background for goal-mouth panel (viewBox should match 7.32 x 2.44 m)
goal_net_bg_layers <- function(svg_path,
                                width_m = GOAL_WIDTH_M,
                                height_m = GOAL_HEIGHT_M) {
  if (is.null(svg_path) || !nzchar(svg_path) || !file.exists(svg_path)) {
    return(list())
  }

  if (requireNamespace("grImport", quietly = TRUE)) {
    pic <- tryCatch(
      grImport::readPicture(svg_path),
      error = function(e) NULL
    )
    if (!is.null(pic)) {
      return(list(
        ggplot2::annotation_custom(
          grob = pic,
          xmin = 0,
          xmax = width_m,
          ymin = 0,
          ymax = height_m
        )
      ))
    }
  }

  if (requireNamespace("rsvg", quietly = TRUE)) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      rsvg::rsvg_png(svg_path, tmp, width = 732, height = 244)
      TRUE
    }, error = function(e) FALSE)
    if (ok && requireNamespace("png", quietly = TRUE)) {
      img <- png::readPNG(tmp)
      return(list(
        ggplot2::annotation_raster(
          raster = img,
          xmin = 0,
          xmax = width_m,
          ymin = 0,
          ymax = height_m,
          interpolate = TRUE
        )
      ))
    }
  }

  warning(
    "goal_net_bg_svg could not be rendered; install grImport or rsvg.",
    call. = FALSE
  )
  list()
}

#' Front-on goal frame layers for UC8 net panel (width 7.32 m, height 2.44 m)
#'
#' Expects goal panel coordinates in metres. Optional \code{bg_svg} path overlays
#' a user-supplied background (viewBox 7.32 x 2.44).
draw_goal_net <- function(colour = "#222222",
                          net_colour = "#E8EEF2",
                          net_line_colour = "#B8C4CE",
                          linewidth = 0.55,
                          width_m = GOAL_WIDTH_M,
                          height_m = GOAL_HEIGHT_M,
                          bg_svg = NULL) {
  v_lines <- seq(0, width_m, by = width_m / 12)
  h_lines <- seq(0, height_m, by = height_m / 8)

  c(
    goal_net_bg_layers(bg_svg, width_m = width_m, height_m = height_m),
    list(
      annotate(
        "rect", xmin = 0, xmax = width_m, ymin = 0, ymax = height_m,
        fill = net_colour, colour = NA
      ),
      annotate(
        "segment",
        x = v_lines, xend = v_lines,
        y = 0, yend = height_m,
        colour = net_line_colour, linewidth = linewidth * 0.3, alpha = 0.45
      ),
      annotate(
        "segment",
        x = 0, xend = width_m,
        y = h_lines, yend = h_lines,
        colour = net_line_colour, linewidth = linewidth * 0.3, alpha = 0.45
      ),
      annotate(
        "segment", x = 0, xend = width_m, y = height_m, yend = height_m,
        colour = colour, linewidth = linewidth * 2
      ),
      annotate(
        "segment", x = 0, xend = 0, y = 0, yend = height_m,
        colour = colour, linewidth = linewidth * 2
      ),
      annotate(
        "segment", x = width_m, xend = width_m, y = 0, yend = height_m,
        colour = colour, linewidth = linewidth * 2
      ),
      annotate(
        "segment", x = 0, xend = width_m, y = 0, yend = 0,
        colour = colour, linewidth = linewidth * 1.2
      )
    )
  )
}
