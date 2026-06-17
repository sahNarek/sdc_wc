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

# Depth of the top-down goal net behind the goal line (StatsBomb / SBPitch box = 2 yd)
GOAL_NET_DEPTH_SB <- 2

#' Top-down goal net behind the attacking goal line (StatsBomb / SBPitch box style)
#'
#' Draws a shallow net box from \code{GOAL_LINE_X} to \code{GOAL_LINE_X + net_depth}
#' between the posts (\code{GOAL_POST_Y_MIN}–\code{GOAL_POST_Y_MAX}). Mesh only — no
#' pitch-colour fill or perspective ground plane.
draw_pitch_goal_net_layers <- function(colour = "#222222",
                                       net_colour = "#E8EEF2",
                                       net_line_colour = "#B8C4CE",
                                       linewidth = 0.5,
                                       goal_line_x = GOAL_LINE_X,
                                       goal_post_y_min = GOAL_POST_Y_MIN,
                                       goal_post_y_max = GOAL_POST_Y_MAX,
                                       net_depth = GOAL_NET_DEPTH_SB) {
  x0 <- goal_line_x
  x1 <- goal_line_x + net_depth
  y0 <- goal_post_y_min
  y1 <- goal_post_y_max

  h_mesh <- seq(x0, x1, length.out = 4)
  v_mesh <- seq(y0, y1, length.out = 5)

  list(
    ggplot2::annotate(
      "rect",
      xmin = x0,
      xmax = x1,
      ymin = y0,
      ymax = y1,
      fill = net_colour,
      colour = NA
    ),
    ggplot2::annotate(
      "segment",
      x = h_mesh,
      xend = h_mesh,
      y = y0,
      yend = y1,
      colour = net_line_colour,
      linewidth = linewidth * 0.35,
      alpha = 0.55
    ),
    ggplot2::annotate(
      "segment",
      x = x0,
      xend = x1,
      y = v_mesh,
      yend = v_mesh,
      colour = net_line_colour,
      linewidth = linewidth * 0.35,
      alpha = 0.55
    ),
    ggplot2::annotate(
      "segment",
      x = x1,
      xend = x1,
      y = y0,
      yend = y1,
      colour = colour,
      linewidth = linewidth * 1.4
    ),
    ggplot2::annotate(
      "segment",
      x = x0,
      xend = x1,
      y = y0,
      yend = y0,
      colour = colour,
      linewidth = linewidth * 1.05
    ),
    ggplot2::annotate(
      "segment",
      x = x0,
      xend = x1,
      y = y1,
      yend = y1,
      colour = colour,
      linewidth = linewidth * 1.05
    )
  )
}

#' Attacking-third pitch (goal at top) for shot maps
#'
#' Uses \code{coord_flip} without \code{scale_y_reverse} so StatsBomb \code{y}
#' increases left-to-right (36 = left post, 44 = right post) and shot
#' trajectories point to the correct side of the goal mouth.
draw_pitch_half_attacking <- function(colour = "black",
                                      linewidth = 0.6,
                                      show_goal_net = TRUE,
                                      goal_net_depth = GOAL_NET_DEPTH_SB,
                                      x_min = 85,
                                      x_max = 125) {
  net_layers <- if (isTRUE(show_goal_net)) {
    draw_pitch_goal_net_layers(
      colour = colour,
      linewidth = linewidth,
      net_depth = goal_net_depth
    )
  } else {
    list()
  }

  c(
    draw_pitch_markings(colour = colour, linewidth = linewidth),
    net_layers,
    list(
      theme(rect = element_blank(), line = element_blank()),
      coord_fixed(ratio = 105 / 100),
      coord_flip(xlim = c(x_min, x_max))
    )
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
GOAL_GROUND_DEPTH_M <- 0.45
GOAL_GROUND_BOTTOM_FLARE_FRAC <- 0.09
GOAL_GROUND_SIDE_EXT_M <- 0.12

#' Tight goal-panel limits and aspect ratio for a wide rectangle (not square)
#'
#' StatsBomb coordinates map 1:1 to metres. \code{aspect.ratio} keeps patchwork
#' from stretching the panel when stacked in a portrait figure.
goal_panel_layout <- function(goal_width_m = GOAL_WIDTH_M,
                              goal_height_m = GOAL_HEIGHT_M,
                              ground_depth_m = GOAL_GROUND_DEPTH_M,
                              bottom_flare_frac = GOAL_GROUND_BOTTOM_FLARE_FRAC,
                              side_ext_m = GOAL_GROUND_SIDE_EXT_M,
                              x_pad = 0.15,
                              icon_size = 0.22,
                              display_tallness = 2.35) {
  bottom_flare_m <- goal_width_m * bottom_flare_frac
  x_outer <- side_ext_m + x_pad
  top_pad_m <- goal_height_m * icon_size * 0.45
  off_frame_pad_m <- goal_width_m * 0.045
  x_span <- goal_width_m + 2 * (bottom_flare_m + x_outer + off_frame_pad_m)
  y_span <- goal_height_m + ground_depth_m + top_pad_m
  base_aspect <- y_span / (x_span * (goal_width_m / goal_height_m))
  list(
    xlim = c(
      -bottom_flare_m - x_outer - off_frame_pad_m,
      goal_width_m + bottom_flare_m + x_outer + off_frame_pad_m
    ),
    ylim = c(-ground_depth_m, goal_height_m + top_pad_m),
    coord_ratio = goal_width_m / y_span,
    aspect_ratio = base_aspect * display_tallness,
    ground_depth_m = ground_depth_m,
    bottom_flare_m = bottom_flare_m,
    side_ext_m = side_ext_m,
    x_outer = x_outer,
    top_pad_m = top_pad_m
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

#' Stack goal-mouth panel with an optional centred legend row beneath it
wrap_goal_panel_block <- function(goal_plot,
                                  legend_plot = NULL,
                                  width_frac = 0.58,
                                  legend_height = 0.09) {
  panel <- if (is.null(legend_plot)) {
    goal_plot
  } else {
    patchwork::wrap_plots(
      goal_plot,
      legend_plot,
      ncol = 1,
      heights = c(1, legend_height)
    )
  }

  wrap_centered_goal_panel(panel, width_frac = width_frac)
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

#' Perspective ground trapezoid beneath the goal mouth (outline only, no fill)
#'
#' SofaScore-style: bottom edge wider than the posts, diagonals from the post
#' bases to the bottom corners, horizontal extensions only on the goal line.
goal_ground_border_layers <- function(colour = "#222222",
                                      linewidth = 0.55,
                                      width_m = GOAL_WIDTH_M,
                                      ground_depth_m = GOAL_GROUND_DEPTH_M,
                                      bottom_flare_frac = GOAL_GROUND_BOTTOM_FLARE_FRAC,
                                      side_ext_m = GOAL_GROUND_SIDE_EXT_M,
                                      x_pad = 0.15) {
  bottom_flare <- width_m * bottom_flare_frac
  x_bot_lo <- -bottom_flare
  x_bot_hi <- width_m + bottom_flare
  x_top_ext_lo <- -side_ext_m - x_pad
  x_top_ext_hi <- width_m + side_ext_m + x_pad
  y_top <- 0
  y_bot <- -ground_depth_m
  sill_lw <- linewidth * 1.05
  goal_lw <- linewidth * 2.1

  list(
    ggplot2::annotate(
      "segment",
      x = x_bot_lo,
      xend = x_bot_hi,
      y = y_bot,
      yend = y_bot,
      colour = colour,
      linewidth = sill_lw
    ),
    ggplot2::annotate(
      "segment",
      x = x_top_ext_lo,
      xend = 0,
      y = y_top,
      yend = y_top,
      colour = colour,
      linewidth = sill_lw
    ),
    ggplot2::annotate(
      "segment",
      x = width_m,
      xend = x_top_ext_hi,
      y = y_top,
      yend = y_top,
      colour = colour,
      linewidth = sill_lw
    ),
    ggplot2::annotate(
      "segment",
      x = 0,
      xend = width_m,
      y = y_top,
      yend = y_top,
      colour = colour,
      linewidth = goal_lw
    ),
    ggplot2::annotate(
      "segment",
      x = 0,
      xend = x_bot_lo,
      y = y_top,
      yend = y_bot,
      colour = colour,
      linewidth = sill_lw
    ),
    ggplot2::annotate(
      "segment",
      x = width_m,
      xend = x_bot_hi,
      y = y_top,
      yend = y_bot,
      colour = colour,
      linewidth = sill_lw
    )
  )
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
                          ground_depth_m = GOAL_GROUND_DEPTH_M,
                          show_ground_border = TRUE,
                          bg_svg = NULL) {
  v_lines <- seq(0, width_m, by = width_m / 12)
  h_lines <- seq(0, height_m, by = height_m / 8)

  frame_layers <- list(
    goal_net_bg_layers(bg_svg, width_m = width_m, height_m = height_m),
    ggplot2::annotate(
      "rect", xmin = 0, xmax = width_m, ymin = 0, ymax = height_m,
      fill = net_colour, colour = NA
    ),
    ggplot2::annotate(
      "segment",
      x = v_lines, xend = v_lines,
      y = 0, yend = height_m,
      colour = net_line_colour, linewidth = linewidth * 0.3, alpha = 0.45
    ),
    ggplot2::annotate(
      "segment",
      x = 0, xend = width_m,
      y = h_lines, yend = h_lines,
      colour = net_line_colour, linewidth = linewidth * 0.3, alpha = 0.45
    ),
    ggplot2::annotate(
      "segment", x = 0, xend = width_m, y = height_m, yend = height_m,
      colour = colour, linewidth = linewidth * 2
    ),
    ggplot2::annotate(
      "segment", x = 0, xend = 0, y = 0, yend = height_m,
      colour = colour, linewidth = linewidth * 2
    ),
    ggplot2::annotate(
      "segment", x = width_m, xend = width_m, y = 0, yend = height_m,
      colour = colour, linewidth = linewidth * 2
    )
  )

  if (isTRUE(show_ground_border)) {
    c(
      frame_layers,
      goal_ground_border_layers(
        colour = colour,
        linewidth = linewidth,
        width_m = width_m,
        ground_depth_m = ground_depth_m
      )
    )
  } else {
    c(
      frame_layers,
      ggplot2::annotate(
        "segment", x = 0, xend = width_m, y = 0, yend = 0,
        colour = colour, linewidth = linewidth * 1.2
      )
    )
  }
}
