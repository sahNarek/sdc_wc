#' Body-part icon paths, xG colouring, and legend helpers for shot maps
SHOT_ICON_SETS <- list(
  standard = list(
    files = list(
      "Head"       = "head",
      "Left Foot"  = "left_foot",
      "Right Foot" = "right_foot",
      "Other"      = "other"
    ),
    dims = list(
      head       = c(width = 120, height = 140),
      left_foot  = c(width = 80, height = 200),
      right_foot = c(width = 80, height = 200),
      other      = c(width = 100, height = 100)
    )
  ),
  footprint = list(
    files = list(
      "Head"       = "head",
      "Left Foot"  = "left_footprint",
      "Right Foot" = "right_footprint",
      "Other"      = "other"
    ),
    dims = list(
      head            = c(width = 120, height = 140),
      left_footprint  = c(width = 100, height = 100),
      right_footprint = c(width = 100, height = 100),
      other           = c(width = 100, height = 100)
    )
  )
)

ICON_COLOR_CACHE <- new.env(parent = emptyenv())

get_icons_dir <- function(root = get_project_root()) {
  file.path(root, "assets", "icons")
}

icon_set_config <- function(icon_set = c("standard", "footprint")) {
  icon_set <- match.arg(icon_set)
  SHOT_ICON_SETS[[icon_set]]
}

#' Rasterise SVG icons to PNG when PNGs are not already provided
ensure_shot_icons <- function(root = get_project_root(),
                              icon_set = c("standard", "footprint")) {
  icon_set <- match.arg(icon_set)
  cfg <- icon_set_config(icon_set)
  icons_dir <- get_icons_dir(root)

  for (stem in unique(unlist(cfg$files))) {
    png_path <- file.path(icons_dir, paste0(stem, ".png"))
    if (file.exists(png_path)) {
      next
    }

    svg_path <- file.path(icons_dir, paste0(stem, ".svg"))
    if (!file.exists(svg_path)) {
      next
    }

    if (!requireNamespace("rsvg", quietly = TRUE)) {
      install.packages("rsvg", repos = "https://cloud.r-project.org")
    }

    dims <- cfg$dims[[stem]] %||% c(width = 100, height = 100)
    rsvg::rsvg_png(
      svg_path,
      png_path,
      width = dims[["width"]],
      height = dims[["height"]]
    )
  }

  invisible(icons_dir)
}

#' Resolve PNG icon path for a StatsBomb body-part label
body_part_icon_path <- function(body_part,
                                root = get_project_root(),
                                icon_set = c("standard", "footprint")) {
  ensure_shot_icons(root, icon_set = icon_set)
  cfg <- icon_set_config(icon_set)
  icons_dir <- get_icons_dir(root)
  stem <- cfg$files[[body_part]] %||% cfg$files[["Other"]]
  file.path(icons_dir, paste0(stem, ".png"))
}

#' Extract an opacity mask from a body-part icon
icon_opacity_mask <- function(icon_path) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    install.packages("magick", repos = "https://cloud.r-project.org")
  }

  img <- magick::image_read(icon_path)
  info <- magick::image_info(img)

  if (isTRUE(info$matte)) {
    return(magick::image_separate(img, channel = "alpha"))
  }

  gray <- magick::image_convert(img, colorspace = "gray")
  magick::image_threshold(gray, threshold = "55%", type = "white")
}

#' Fill the icon shape with a solid colour (no black overlay)
colorize_body_part_icon <- function(icon_path, hex_color) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    install.packages("magick", repos = "https://cloud.r-project.org")
  }

  mask <- icon_opacity_mask(icon_path)
  info <- magick::image_info(mask)
  fill <- magick::image_blank(info$width, info$height, color = hex_color)
  magick::image_composite(fill, mask, operator = "CopyOpacity")
}

#' Map xG values to hex colours along the shot gradient
xg_to_hex <- function(xg,
                      colors = palette_single_gradient(),
                      limits = c(0, 0.8)) {
  xg <- replace_na(xg, limits[1])
  xg_scaled <- pmin(pmax(xg, limits[1]), limits[2])
  if (limits[2] == limits[1]) {
    return(rep(colors[length(colors)], length(xg)))
  }

  idx <- round(
    1 + (xg_scaled - limits[1]) / (limits[2] - limits[1]) * (length(colors) - 1)
  )
  idx <- pmax(1L, pmin(length(colors), idx))
  colors[idx]
}

#' Cached path to a body-part icon tinted with a specific colour
colored_body_part_icon_path <- function(body_part,
                                        hex_color,
                                        root = get_project_root(),
                                        icon_set = "footprint") {
  key <- paste(icon_set, body_part, hex_color, sep = "|")
  if (exists(key, envir = ICON_COLOR_CACHE, inherits = FALSE)) {
    return(get(key, envir = ICON_COLOR_CACHE))
  }

  base_icon <- body_part_icon_path(body_part, root = root, icon_set = icon_set)
  colored <- colorize_body_part_icon(base_icon, hex_color)
  cache_dir <- file.path(tempdir(), "sdc_shot_icons")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(
    cache_dir,
    paste0(gsub("[^A-Za-z0-9]+", "_", key), ".png")
  )
  magick::image_write(colored, out)
  assign(key, out, envir = ICON_COLOR_CACHE)
  out
}

invalidate_ball_icon_cache <- function() {
  keys <- ls(envir = ICON_COLOR_CACHE)
  ball_keys <- grep("^ball\\|", keys, value = TRUE)
  if (length(ball_keys) > 0) {
    rm(list = ball_keys, envir = ICON_COLOR_CACHE)
  }
  invisible(NULL)
}

#' Ensure ball.svg is rasterised to PNG (refreshes when SVG is newer)
ensure_ball_icon <- function(root = get_project_root()) {
  icons_dir <- get_icons_dir(root)
  png_path <- file.path(icons_dir, "ball.png")
  svg_path <- file.path(icons_dir, "ball.svg")
  if (!file.exists(svg_path)) {
    stop("Ball icon not found: ", svg_path, call. = FALSE)
  }

  needs_refresh <- !file.exists(png_path) ||
    file.info(svg_path)$mtime > file.info(png_path)$mtime

  if (!isTRUE(needs_refresh)) {
    return(invisible(png_path))
  }

  if (!requireNamespace("rsvg", quietly = TRUE)) {
    install.packages("rsvg", repos = "https://cloud.r-project.org")
  }

  invalidate_ball_icon_cache()
  rsvg::rsvg_png(svg_path, png_path, width = 200, height = 200)
  invisible(png_path)
}

#' Path to the rasterised ball icon
ball_icon_path <- function(root = get_project_root()) {
  ensure_ball_icon(root)
  file.path(get_icons_dir(root), "ball.png")
}

#' Cached ball icon tinted with a solid colour
colored_ball_icon_path <- function(hex_color, root = get_project_root()) {
  key <- paste("ball", hex_color, sep = "|")
  if (exists(key, envir = ICON_COLOR_CACHE, inherits = FALSE)) {
    return(get(key, envir = ICON_COLOR_CACHE))
  }

  colored <- colorize_body_part_icon(ball_icon_path(root), hex_color)
  cache_dir <- file.path(tempdir(), "sdc_shot_icons")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(
    cache_dir,
    paste0(gsub("[^A-Za-z0-9]+", "_", key), ".png")
  )
  magick::image_write(colored, out)
  assign(key, out, envir = ICON_COLOR_CACHE)
  out
}

#' Goal-net ball colour from shot outcome (green = goal, red = otherwise)
goal_net_ball_color <- function(outcome) {
  if (identical(outcome, "Goal")) {
    SDC_PALETTE[["green"]]
  } else {
    SDC_PALETTE[["red"]]
  }
}

#' Add green/red ball icon paths for the goal-mouth panel
add_goal_net_ball_icons <- function(net_data) {
  net_data %>%
    dplyr::mutate(
      marker_fill = purrr::map_chr(
        .data$`shot.outcome.name`,
        goal_net_ball_color
      ),
      colored_icon = purrr::map_chr(.data$marker_fill, colored_ball_icon_path)
    )
}

#' Legend for goal-mouth ball markers (icons above labels, centred)
plot_goal_mouth_ball_legend <- function() {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  ensure_ball_icon()

  legend_df <- tibble::tibble(
    label = c("Goal", "No goal"),
    color = c(SDC_PALETTE[["green"]], SDC_PALETTE[["red"]]),
    x = c(1, 2.15),
    y_icon = 1,
    y_label = 0.52
  ) %>%
    dplyr::mutate(
      icon = purrr::map_chr(.data$color, colored_ball_icon_path)
    )

  ggplot2::ggplot(legend_df) +
    ggimage::geom_image(
      ggplot2::aes(x = .data$x, y = .data$y_icon, image = .data$icon),
      size = 0.38
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$x, y = .data$y_label, label = .data$label),
      family = SDC_FONTS$body,
      size = 4.2,
      colour = "#333333"
    ) +
    ggplot2::coord_cartesian(xlim = c(0.35, 2.8), ylim = c(0.35, 1.12), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 0, 4, 0))
}

#' Add per-shot coloured icon paths to shot data
add_colored_shot_icons <- function(shots_df,
                                   shot_color = SDC_PALETTE[["blue"]],
                                   lightest_color = NULL,
                                   gradient_colors = NULL,
                                   team_colors = NULL,
                                   limits = c(0, 0.8),
                                   icon_set = "footprint") {
  if (!is.null(team_colors) && "team.name" %in% names(shots_df)) {
    return(shots_df %>%
      dplyr::mutate(
        colored_icon = purrr::pmap_chr(
          list(.data$`shot.body_part.name`, .data$`shot.statsbomb_xg`, .data$`team.name`),
          function(body_part, xg, team) {
            base_color <- team_colors[[team]]
            if (is.null(base_color) || is.na(base_color)) {
              base_color <- shot_color
            }
            colors <- resolve_single_hue_gradient(
              color = base_color,
              lightest_color = lightest_color,
              gradient_colors = gradient_colors,
              n = 11,
              variant = "shot_map"
            )
            hex <- xg_to_hex(xg, colors = colors, limits = limits)
            colored_body_part_icon_path(body_part, hex, icon_set = icon_set)
          }
        )
      ))
  }

  colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 11,
    variant = "shot_map"
  )

  shots_df %>%
    mutate(
      xg_hex = xg_to_hex(shot.statsbomb_xg, colors = colors, limits = limits),
      colored_icon = purrr::pmap_chr(
        list(shot.body_part.name, xg_hex),
        function(body_part, hex) {
          colored_body_part_icon_path(body_part, hex, icon_set = icon_set)
        }
      )
    )
}

#' Human-readable body-part label for legends
body_part_label <- function(body_part) {
  dplyr::case_when(
    body_part == "Head" ~ "Header",
    body_part == "Left Foot" ~ "Left foot",
    body_part == "Right Foot" ~ "Right foot",
    TRUE ~ "Other"
  )
}

#' Small legend plot with body-part icons (neutral tint for shape reference)
plot_body_part_legend <- function(icon_color = SDC_PALETTE[["blue"]],
                                  icon_set = "footprint") {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  legend_df <- tibble::tibble(
    body_part = c("Head", "Left Foot", "Right Foot"),
    x = c(1, 2.35, 3.7),
    y = 1
  ) %>%
    dplyr::mutate(
      label = vapply(body_part, body_part_label, character(1)),
      icon = purrr::map2_chr(
        body_part,
        rep(icon_color, length(body_part)),
        function(bp, col) colored_body_part_icon_path(bp, col, icon_set = icon_set)
      )
    )

  ggplot2::ggplot(legend_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggimage::geom_image(ggplot2::aes(image = .data$icon), size = 0.44) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      y = 0.6,
      family = SDC_FONTS$body,
      size = 4.6,
      colour = "#333333"
    ) +
    ggplot2::coord_cartesian(xlim = c(0.35, 4.35), ylim = c(0.35, 1.25), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 0, 4, 0))
}

#' Legend for outcome-coloured shot trajectories (single row)
plot_shot_outcome_legend <- function() {
  cols <- shot_trajectory_outcome_colors()
  legend_df <- tibble::tibble(
    label = c("Goal", "Saved", "Other"),
    line_colour = unname(cols),
    x = c(1, 2, 3)
  )

  ggplot2::ggplot(legend_df) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x - 0.28,
        y = 1,
        xend = .data$x - 0.02,
        yend = 1,
        colour = I(.data$line_colour)
      ),
      linetype = "dashed",
      linewidth = 0.5,
      alpha = 0.9
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$x + 0.02, y = 1, label = .data$label),
      hjust = 0,
      family = SDC_FONTS$body,
      size = 3.5,
      colour = "#333333"
    ) +
    ggplot2::coord_cartesian(xlim = c(0.45, 3.75), ylim = c(0.72, 1.28), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 2, 6, 2))
}

#' Combined bottom-row legend: xG bar, goal balls, and trajectory dashes
plot_shot_map_bottom_legend <- function(shot_colors,
                                      limits = c(0, 0.8),
                                      show_xg = TRUE,
                                      show_ball = FALSE,
                                      show_trajectory = FALSE) {
  if (isTRUE(show_ball) && !requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(0.1, 1.05), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 8, 6, 8))

  if (isTRUE(show_xg)) {
    bar_n <- 80
    bar_x <- seq(0.45, 2.55, length.out = bar_n)
    bar_df <- tibble::tibble(
      x = bar_x,
      y = 0.5,
      xg = seq(limits[1], limits[2], length.out = bar_n)
    )
    bar_w <- diff(bar_x)[1]

    p <- p +
      ggplot2::annotate(
        "text",
        x = 1.5,
        y = 0.9,
        label = "Expected goals (xG)",
        family = SDC_FONTS$body,
        size = 4,
        colour = "#333333"
      ) +
      ggplot2::geom_tile(
        data = bar_df,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          width = bar_w,
          height = 0.2,
          fill = .data$xg
        )
      ) +
      ggplot2::scale_fill_gradientn(
        colours = shot_colors,
        limits = limits,
        oob = scales::squish,
        guide = "none"
      ) +
      ggplot2::annotate(
        "text",
        x = 0.45,
        y = 0.26,
        label = format(limits[1], nsmall = 1),
        family = SDC_FONTS$body,
        size = 3.6,
        colour = "#333333"
      ) +
      ggplot2::annotate(
        "text",
        x = 2.55,
        y = 0.26,
        label = format(limits[2], nsmall = 1),
        family = SDC_FONTS$body,
        size = 3.6,
        colour = "#333333"
      )
  }

  if (isTRUE(show_ball)) {
    ball_df <- tibble::tibble(
      label = c("Goal", "No goal"),
      color = c(SDC_PALETTE[["green"]], SDC_PALETTE[["red"]]),
      x = c(3.15, 4.55),
      y = 0.5
    ) %>%
      dplyr::mutate(
        icon = purrr::map_chr(.data$color, colored_ball_icon_path)
      )

    p <- p +
      ggimage::geom_image(
        data = ball_df,
        ggplot2::aes(x = .data$x - 0.2, y = .data$y, image = .data$icon),
        size = 0.11
      ) +
      ggplot2::geom_text(
        data = ball_df,
        ggplot2::aes(x = .data$x + 0.07, y = .data$y, label = .data$label),
        hjust = 0,
        family = SDC_FONTS$body,
        size = 4,
        colour = "#333333"
      )
  }

  if (isTRUE(show_trajectory)) {
    cols <- shot_trajectory_outcome_colors()
    traj_df <- tibble::tibble(
      label = c("Goal", "Saved", "Other"),
      line_colour = unname(cols),
      x = c(6.35, 7.75, 9.15)
    )

    p <- p +
      ggplot2::geom_segment(
        data = traj_df,
        ggplot2::aes(
          x = .data$x - 0.34,
          y = 0.5,
          xend = .data$x - 0.04,
          yend = 0.5,
          colour = I(.data$line_colour)
        ),
        linetype = "dashed",
        linewidth = 1,
        alpha = 0.95
      ) +
      ggplot2::geom_text(
        data = traj_df,
        ggplot2::aes(x = .data$x + 0.03, y = 0.5, label = .data$label),
        hjust = 0,
        family = SDC_FONTS$body,
        size = 4,
        colour = "#333333"
      )
  }

  p
}

#' Combine shot map with body-part legend (top) and xG legend (bottom)
assemble_shot_map <- function(shot_plot,
                              icon_set = "footprint",
                              icon_color = SDC_PALETTE[["blue"]],
                              shot_colors = NULL,
                              xg_limits = c(0, 0.8),
                              show_xg_legend = TRUE,
                              show_goal_net_ball_legend = FALSE,
                              show_trajectory_legend = FALSE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(shot_colors)) {
    shot_colors <- resolve_single_hue_gradient(
      color = icon_color,
      variant = "shot_map",
      n = 11
    )
  }

  body_legend <- plot_body_part_legend(icon_set = icon_set, icon_color = icon_color)

  body_row <- patchwork::wrap_plots(
    patchwork::plot_spacer(),
    body_legend,
    patchwork::plot_spacer(),
    ncol = 3,
    widths = c(0.65, 1.7, 0.65)
  )

  show_bottom <- isTRUE(show_xg_legend) ||
    isTRUE(show_goal_net_ball_legend) ||
    isTRUE(show_trajectory_legend)

  legend_rows <- list(body_row)
  legend_heights <- c(0.125)

  if (isTRUE(show_bottom)) {
    bottom_row <- plot_shot_map_bottom_legend(
      shot_colors = shot_colors,
      limits = xg_limits,
      show_xg = isTRUE(show_xg_legend),
      show_ball = isTRUE(show_goal_net_ball_legend),
      show_trajectory = isTRUE(show_trajectory_legend)
    )
    legend_rows <- c(legend_rows, list(bottom_row))
    legend_heights <- c(legend_heights, 0.14)
  }

  legend_block <- patchwork::wrap_plots(
    legend_rows,
    ncol = 1,
    heights = legend_heights
  )

  panels <- list(shot_plot, legend_block)
  heights <- c(1, sum(legend_heights) + 0.02)

  patchwork::wrap_plots(panels, ncol = 1, heights = heights)
}
