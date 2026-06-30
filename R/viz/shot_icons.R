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

#' Clear cached tinted gloves icons (e.g. after SVG update)
invalidate_gloves_icon_cache <- function() {
  keys <- ls(envir = ICON_COLOR_CACHE, all.names = TRUE)
  gloves_keys <- grep("^gloves\\|", keys, value = TRUE)
  if (length(gloves_keys) > 0) {
    rm(list = gloves_keys, envir = ICON_COLOR_CACHE)
  }
  invisible(NULL)
}

invalidate_block_icon_cache <- function() {
  keys <- ls(envir = ICON_COLOR_CACHE, all.names = TRUE)
  block_keys <- grep("^block\\|", keys, value = TRUE)
  if (length(block_keys) > 0) {
    rm(list = block_keys, envir = ICON_COLOR_CACHE)
  }
  invisible(NULL)
}

#' Ensure block_sign.svg is rasterised to PNG (refreshes when SVG is newer)
ensure_block_icon <- function(root = get_project_root()) {
  icons_dir <- get_icons_dir(root)
  png_path <- file.path(icons_dir, "block_sign.png")
  svg_path <- file.path(icons_dir, "block_sign.svg")
  if (!file.exists(svg_path)) {
    stop("Block icon not found: ", svg_path, call. = FALSE)
  }

  needs_refresh <- !file.exists(png_path) ||
    file.info(svg_path)$mtime > file.info(png_path)$mtime

  if (!isTRUE(needs_refresh)) {
    return(invisible(png_path))
  }

  if (!requireNamespace("rsvg", quietly = TRUE)) {
    install.packages("rsvg", repos = "https://cloud.r-project.org")
  }

  invalidate_block_icon_cache()
  rsvg::rsvg_png(svg_path, png_path, width = 200, height = 200)
  invisible(png_path)
}

#' Path to the rasterised block icon
block_icon_path <- function(root = get_project_root()) {
  ensure_block_icon(root)
  file.path(get_icons_dir(root), "block_sign.png")
}

#' Cached block icon tinted with a solid colour
colored_block_icon_path <- function(hex_color, root = get_project_root()) {
  key <- paste("block", hex_color, sep = "|")
  if (exists(key, envir = ICON_COLOR_CACHE, inherits = FALSE)) {
    return(get(key, envir = ICON_COLOR_CACHE))
  }

  colored <- colorize_body_part_icon(block_icon_path(root), hex_color)
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

#' Ensure gloves.svg is rasterised to PNG (refreshes when SVG is newer)
ensure_gloves_icon <- function(root = get_project_root()) {
  icons_dir <- get_icons_dir(root)
  png_path <- file.path(icons_dir, "gloves.png")
  svg_path <- file.path(icons_dir, "gloves.svg")
  if (!file.exists(svg_path)) {
    stop("Gloves icon not found: ", svg_path, call. = FALSE)
  }

  needs_refresh <- !file.exists(png_path) ||
    file.info(svg_path)$mtime > file.info(png_path)$mtime

  if (!isTRUE(needs_refresh)) {
    return(invisible(png_path))
  }

  if (!requireNamespace("rsvg", quietly = TRUE)) {
    install.packages("rsvg", repos = "https://cloud.r-project.org")
  }

  invalidate_gloves_icon_cache()
  rsvg::rsvg_png(svg_path, png_path, width = 200, height = 200)
  invisible(png_path)
}

#' Path to the rasterised gloves icon
gloves_icon_path <- function(root = get_project_root()) {
  ensure_gloves_icon(root)
  file.path(get_icons_dir(root), "gloves.png")
}

#' Cached gloves icon tinted with a solid colour
colored_gloves_icon_path <- function(hex_color, root = get_project_root()) {
  key <- paste("gloves", hex_color, sep = "|")
  if (exists(key, envir = ICON_COLOR_CACHE, inherits = FALSE)) {
    return(get(key, envir = ICON_COLOR_CACHE))
  }

  colored <- colorize_body_part_icon(gloves_icon_path(root), hex_color)
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

#' Path to the star SVG asset (central player marker in passing networks)
star_icon_svg_path <- function(root = get_project_root()) {
  file.path(root, "assets", "star.svg")
}

#' Ensure star.svg is rasterised to PNG (refreshes when SVG is newer)
ensure_star_icon <- function(root = get_project_root()) {
  icons_dir <- get_icons_dir(root)
  png_path <- file.path(icons_dir, "star_outline.png")
  svg_path <- star_icon_svg_path(root)
  if (!file.exists(svg_path)) {
    stop("Star icon not found: ", svg_path, call. = FALSE)
  }

  needs_refresh <- !file.exists(png_path) ||
    file.info(svg_path)$mtime > file.info(png_path)$mtime

  if (!isTRUE(needs_refresh)) {
    return(invisible(png_path))
  }

  if (!requireNamespace("rsvg", quietly = TRUE)) {
    install.packages("rsvg", repos = "https://cloud.r-project.org")
  }

  rsvg::rsvg_png(svg_path, png_path, width = 240, height = 240)
  invisible(png_path)
}

#' Path to the rasterised star icon
star_icon_path <- function(root = get_project_root()) {
  ensure_star_icon(root)
  file.path(get_icons_dir(root), "star_outline.png")
}

#' Cached star icon tinted with a solid colour
colored_star_icon_path <- function(hex_color, root = get_project_root()) {
  key <- paste("star", hex_color, sep = "|")
  if (exists(key, envir = ICON_COLOR_CACHE, inherits = FALSE)) {
    return(get(key, envir = ICON_COLOR_CACHE))
  }

  colored <- colorize_body_part_icon(star_icon_path(root), hex_color)
  cache_dir <- file.path(tempdir(), "sdc_shot_icons")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(
    cache_dir,
    paste0(gsub("[^A-Za-z0-9]+", "_", key), ".png")
  )
  magick::image_write(magick::image_convert(colored, "png32"), out)
  assign(key, out, envir = ICON_COLOR_CACHE)
  out
}

#' Five-point star polygon coordinates (centre, outer/inner radius)
star_polygon_coords <- function(cx,
                                cy,
                                r_outer,
                                r_inner,
                                n_points = 5L) {
  angles <- seq(-pi / 2, 3 * pi / 2, length.out = 2L * n_points + 1L)[-(2L * n_points + 1L)]
  radii <- rep(c(r_outer, r_inner), each = n_points)
  cbind(
    x = cx + radii * cos(angles),
    y = cy + radii * sin(angles)
  )
}

#' Star marker for the most-involved player: team-coloured outline on transparent PNG
central_player_star_icon_path <- function(hex_color, root = get_project_root()) {
  key <- paste("star_central", basename(star_icon_svg_path(root)), "v4", hex_color, sep = "|")
  if (exists(key, envir = ICON_COLOR_CACHE, inherits = FALSE)) {
    return(get(key, envir = ICON_COLOR_CACHE))
  }

  if (!requireNamespace("magick", quietly = TRUE)) {
    install.packages("magick", repos = "https://cloud.r-project.org")
  }

  ensure_star_icon(root)
  colored_outline <- colorize_body_part_icon(star_icon_path(root), hex_color)
  result <- magick::image_convert(colored_outline, "png32")

  cache_dir <- file.path(tempdir(), "sdc_shot_icons")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(
    cache_dir,
    paste0(gsub("[^A-Za-z0-9]+", "_", key), ".png")
  )
  magick::image_write(result, out)
  assign(key, out, envir = ICON_COLOR_CACHE)
  out
}

#' Goal-net marker colour from shot outcome (green = goal, orange = saved, purple = blocked)
goal_net_marker_color <- function(outcome) {
  if (identical(outcome, "Goal")) {
    SDC_PALETTE[["green"]]
  } else if (identical(outcome, "Saved")) {
    SDC_PALETTE[["orange"]]
  } else if (identical(outcome, "Blocked")) {
    SDC_PALETTE[["purple"]]
  } else {
    SDC_PALETTE[["red"]]
  }
}

#' Goal-net ball colour from shot outcome (green = goal, red = otherwise)
goal_net_ball_color <- goal_net_marker_color

#' Add goal-mouth icon paths (ball for goals / misses, gloves for saves)
add_goal_net_ball_icons <- function(net_data) {
  ensure_ball_icon()
  ensure_gloves_icon()
  ensure_block_icon()

  net_data %>%
    dplyr::mutate(
      marker_fill = purrr::map_chr(
        .data$`shot.outcome.name`,
        goal_net_marker_color
      ),
      colored_icon = purrr::map2_chr(
        .data$`shot.outcome.name`,
        .data$marker_fill,
        function(outcome, fill) {
          if (identical(outcome, "Saved")) {
            colored_gloves_icon_path(fill)
          } else if (identical(outcome, "Blocked")) {
            colored_block_icon_path(fill)
          } else {
            colored_ball_icon_path(fill)
          }
        }
      )
    )
}

#' Data frame for goal-mouth icon legend (goal / saved / blocked / missed)
shot_outcome_legend_df <- function(x_centers = c(1.05, 3.35, 5.75, 8.15),
                                   y_icon = 0.68,
                                   y_label = 0.50) {
  ensure_ball_icon()
  ensure_gloves_icon()
  ensure_block_icon()

  tibble::tibble(
    label = c("Goal", "Saved", "Blocked", "Missed"),
    outcome = c("Goal", "Saved", "Blocked", "Off T"),
    color = c(
      SDC_PALETTE[["green"]],
      SDC_PALETTE[["orange"]],
      SDC_PALETTE[["purple"]],
      SDC_PALETTE[["red"]]
    ),
    x = x_centers,
    y_icon = y_icon,
    y_label = y_label
  ) %>%
    dplyr::mutate(
      icon = purrr::map2_chr(
        .data$outcome,
        .data$color,
        function(outcome, fill) {
          if (identical(outcome, "Saved")) {
            colored_gloves_icon_path(fill)
          } else if (identical(outcome, "Blocked")) {
            colored_block_icon_path(fill)
          } else {
            colored_ball_icon_path(fill)
          }
        }
      )
    )
}

#' Legend for goal-mouth ball markers (icons above labels, centred)
plot_goal_mouth_ball_legend <- function() {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  title_style <- legend_title_ggpar()
  label_style <- legend_label_ggpar()
  legend_df <- shot_outcome_legend_df()

  ggplot2::ggplot(legend_df) +
    ggplot2::annotate(
      "text",
      x = 4.6,
      y = 0.95,
      label = "Shot outcome",
      family = title_style$family,
      size = title_style$size,
      colour = title_style$colour,
      fontface = title_style$fontface
    ) +
    ggimage::geom_image(
      ggplot2::aes(x = .data$x, y = .data$y_icon, image = .data$icon),
      size = 0.28
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$x, y = .data$y_label, label = .data$label),
      family = label_style$family,
      size = 4.1,
      colour = label_style$colour,
      fontface = label_style$fontface
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 9.2), ylim = c(0.32, 1.05), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 4, 2, 4))
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
          list(
            .data$`shot.body_part.name`,
            .data$`shot.statsbomb_xg`,
            .data$`team.name`
          ),
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

#' Shared legend title styling (Open Sans Regular — style guide legends/captions)
legend_title_ggpar <- function() {
  list(
    family = SDC_FONTS$body,
    size = 5,
    colour = "#111111",
    fontface = "plain"
  )
}

#' Shared legend item label styling (Open Sans Regular)
legend_label_ggpar <- function() {
  list(
    family = SDC_FONTS$body,
    size = 4.6,
    colour = "#111111",
    fontface = "plain"
  )
}

#' Single-row shot-map legend with aligned title and label baselines
plot_shot_map_legend_row <- function(shot_colors,
                                    limits = c(0, 0.8),
                                    icon_color = SDC_PALETTE[["blue"]],
                                    icon_set = "footprint",
                                    show_body_part = TRUE,
                                    show_xg = TRUE,
                                    show_trajectory = FALSE,
                                    show_ball = FALSE) {
  needs_images <- isTRUE(show_body_part) || isTRUE(show_ball)
  if (needs_images && !requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  title_style <- legend_title_ggpar()
  label_style <- legend_label_ggpar()
  title_y <- 0.88
  marker_y <- 0.55
  label_y <- 0.28

  section_keys <- c(
    if (isTRUE(show_body_part)) "body",
    if (isTRUE(show_xg)) "xg",
    if (isTRUE(show_trajectory)) "outcome",
    if (isTRUE(show_ball)) "ball"
  )
  section_centers <- if (length(section_keys) == 1) {
    5
  } else if (length(section_keys) == 2) {
    c(2.8, 7.2)
  } else if (length(section_keys) == 3) {
    c(1.75, 5, 8.25)
  } else {
    seq(1.5, 8.5, length.out = length(section_keys))
  }
  names(section_centers) <- section_keys

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(0.12, 1.02), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 8, 4, 8))

  if (isTRUE(show_body_part)) {
    cx <- section_centers[["body"]]
    body_spread <- 1.12
    body_df <- tibble::tibble(
      body_part = c("Head", "Left Foot", "Right Foot"),
      x = cx + c(-body_spread, 0, body_spread),
      y = marker_y
    ) %>%
      dplyr::mutate(
        label = vapply(body_part, body_part_label, character(1)),
        icon = purrr::map2_chr(
          body_part,
          rep(icon_color, 3L),
          function(bp, col) colored_body_part_icon_path(bp, col, icon_set = icon_set)
        )
      )

    p <- p +
      ggplot2::annotate(
        "text",
        x = cx,
        y = title_y,
        label = "Body part used",
        family = title_style$family,
        size = title_style$size,
        colour = title_style$colour,
        fontface = title_style$fontface
      ) +
      ggimage::geom_image(
        data = body_df,
        ggplot2::aes(x = .data$x, y = .data$y, image = .data$icon),
        size = 0.32
      ) +
      ggplot2::geom_text(
        data = body_df,
        ggplot2::aes(x = .data$x, y = label_y, label = .data$label),
        family = label_style$family,
        size = 4.2,
        colour = label_style$colour,
        fontface = label_style$fontface
      )
  }

  if (isTRUE(show_xg)) {
    cx <- section_centers[["xg"]]
    bar_half <- 1.05
    bar_x <- seq(cx - bar_half, cx + bar_half, length.out = 80)
    bar_w <- diff(bar_x)[1]
    bar_df <- tibble::tibble(
      x = bar_x,
      y = marker_y,
      xg = seq(limits[1], limits[2], length.out = length(bar_x))
    )

    p <- p +
      ggplot2::annotate(
        "text",
        x = cx,
        y = title_y,
        label = "Expected goals (xG)",
        family = title_style$family,
        size = title_style$size,
        colour = title_style$colour,
        fontface = title_style$fontface
      ) +
      ggplot2::geom_tile(
        data = bar_df,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          width = bar_w,
          height = 0.18,
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
        x = cx - bar_half,
        y = label_y,
        label = format(limits[1], nsmall = 1),
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
      ) +
      ggplot2::annotate(
        "text",
        x = cx + bar_half,
        y = label_y,
        label = format(limits[2], nsmall = 1),
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
      )
  }

  if (isTRUE(show_trajectory)) {
    cx <- section_centers[["outcome"]]
    cols <- shot_trajectory_outcome_colors()
    traj_df <- tibble::tibble(
      label = c("Goal", "Saved", "Blocked", "Missed"),
      line_colour = unname(cols),
      x = cx + c(-1.65, -0.55, 0.55, 1.65)
    )

    p <- p +
      ggplot2::annotate(
        "text",
        x = cx,
        y = title_y,
        label = "Shot outcome",
        family = title_style$family,
        size = title_style$size,
        colour = title_style$colour,
        fontface = title_style$fontface
      ) +
      ggplot2::geom_segment(
        data = traj_df,
        ggplot2::aes(
          x = .data$x - 0.28,
          y = marker_y,
          xend = .data$x + 0.28,
          yend = marker_y,
          colour = I(.data$line_colour)
        ),
        linetype = "dashed",
        linewidth = 0.95,
        alpha = 0.95
      ) +
      ggplot2::geom_text(
        data = traj_df,
        ggplot2::aes(x = .data$x, y = label_y, label = .data$label),
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
      )
  }

  if (isTRUE(show_ball)) {
    cx <- section_centers[["ball"]]
    ball_df <- tibble::tibble(
      label = c("Goal", "No goal"),
      color = c(SDC_PALETTE[["green"]], SDC_PALETTE[["red"]]),
      x = cx + c(-0.55, 0.55),
      y = marker_y
    ) %>%
      dplyr::mutate(
        icon = purrr::map_chr(.data$color, colored_ball_icon_path)
      )

    p <- p +
      ggimage::geom_image(
        data = ball_df,
        ggplot2::aes(x = .data$x, y = .data$y, image = .data$icon),
        size = 0.1
      ) +
      ggplot2::geom_text(
        data = ball_df,
        ggplot2::aes(x = .data$x, y = label_y, label = .data$label),
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
      )
  }

  p
}

#' Small legend plot with body-part icons (neutral tint for shape reference)
plot_body_part_legend <- function(icon_color = SDC_PALETTE[["blue"]],
                                  icon_set = "footprint") {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  title_style <- legend_title_ggpar()
  label_style <- legend_label_ggpar()

  legend_df <- tibble::tibble(
    body_part = c("Head", "Left Foot", "Right Foot"),
    x = c(1, 2.35, 3.7),
    y = 0.88
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
    ggplot2::annotate(
      "text",
      x = 2.35,
      y = 1.42,
      label = "Body part used",
      family = title_style$family,
      size = title_style$size,
      colour = title_style$colour,
      fontface = title_style$fontface
    ) +
    ggimage::geom_image(ggplot2::aes(image = .data$icon), size = 0.44) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      y = 0.48,
      family = label_style$family,
      size = label_style$size,
      colour = label_style$colour,
      fontface = label_style$fontface
    ) +
    ggplot2::coord_cartesian(xlim = c(0.35, 4.35), ylim = c(0.35, 1.5), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 0, 4, 0))
}

#' Legend for outcome-coloured shot trajectories (single row)
plot_shot_outcome_legend <- function() {
  cols <- shot_trajectory_outcome_colors()
  legend_df <- tibble::tibble(
    label = c("Goal", "Saved", "Missed"),
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

#' Combined bottom-row legend: xG bar and trajectory outcome dashes
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
    title_style <- legend_title_ggpar()
    label_style <- legend_label_ggpar()
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
        family = title_style$family,
        size = title_style$size,
        colour = title_style$colour,
        fontface = title_style$fontface
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
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
      ) +
      ggplot2::annotate(
        "text",
        x = 2.55,
        y = 0.26,
        label = format(limits[2], nsmall = 1),
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
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
    title_style <- legend_title_ggpar()
    label_style <- legend_label_ggpar()
    cols <- shot_trajectory_outcome_colors()
    traj_df <- tibble::tibble(
      label = c("Goal", "Saved", "Missed"),
      line_colour = unname(cols),
      x = c(6.35, 7.75, 9.15)
    )

    p <- p +
      ggplot2::annotate(
        "text",
        x = 7.75,
        y = 0.9,
        label = "Shot outcome",
        family = title_style$family,
        size = title_style$size,
        colour = title_style$colour,
        fontface = title_style$fontface
      ) +
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
        family = label_style$family,
        size = label_style$size,
        colour = label_style$colour,
        fontface = label_style$fontface
      )
  }

  p
}

#' Draw a gradient bar segment (no title or tick labels)
heatmap_share_legend_bar <- function(p,
                                     cx,
                                     heat_colors,
                                     marker_y,
                                     bar_half = 1.5,
                                     bar_height = 0.09) {
  n_seg <- 80
  fill_cols <- grDevices::colorRampPalette(heat_colors)(n_seg)
  seg_w <- (2 * bar_half) / n_seg
  bar_df <- tibble::tibble(
    x = seq(cx - bar_half + seg_w / 2, cx + bar_half - seg_w / 2, length.out = n_seg),
    y = marker_y,
    fill_colour = fill_cols
  )

  p +
    ggplot2::geom_rect(
      data = bar_df,
      ggplot2::aes(
        xmin = .data$x - seg_w / 2,
        xmax = .data$x + seg_w / 2,
        ymin = .data$y - bar_height,
        ymax = .data$y + bar_height
      ),
      fill = bar_df$fill_colour,
      colour = NA
    )
}

#' Add one heatmap share legend section centred at \code{cx}
heatmap_share_legend_section <- function(p,
                                         cx,
                                         heat_colors,
                                         title,
                                         limits = c(0, 0.2),
                                         breaks = NULL,
                                         bar_half = 1.2,
                                         label_size = NULL,
                                         title_y = NULL,
                                         marker_y = NULL,
                                         label_y = NULL) {
  if (is.null(breaks)) {
    max_pct <- limits[2] * 100
    step <- if (max_pct <= 12) 4 else if (max_pct <= 20) 5 else 10
    breaks <- seq(0, limits[2], by = step / 100)
    breaks <- breaks[breaks <= limits[2] + 1e-9]
  }

  title_style <- legend_title_ggpar()
  label_style <- legend_label_ggpar()
  if (!is.null(label_size)) {
    label_style$size <- label_size
  }
  title_y <- title_y %||% 0.88
  marker_y <- marker_y %||% 0.55
  label_y <- label_y %||% 0.28
  tick_df <- tibble::tibble(
    x = cx - bar_half + (breaks - limits[1]) / diff(limits) * (2 * bar_half),
    label = scales::percent(breaks, accuracy = 1)
  )

  p <- heatmap_share_legend_bar(
    p,
    cx = cx,
    heat_colors = heat_colors,
    marker_y = marker_y,
    bar_half = bar_half
  )

  p +
    ggplot2::annotate(
      "text",
      x = cx,
      y = title_y,
      label = title,
      family = title_style$family,
      size = title_style$size,
      colour = title_style$colour,
      fontface = title_style$fontface
    ) +
    ggplot2::geom_text(
      data = tick_df,
      ggplot2::aes(x = .data$x, y = label_y, label = .data$label),
      family = label_style$family,
      size = label_style$size,
      colour = label_style$colour,
      fontface = label_style$fontface
    )
}

#' Single-row heatmap share legend with title above the gradient bar
plot_heatmap_share_legend_row <- function(heat_colors,
                                          title = "Share of attacking actions",
                                          limits = c(0, 0.2),
                                          breaks = NULL,
                                          compact = FALSE) {
  bar_half <- if (isTRUE(compact)) 2.4 else 1.5
  label_size <- if (isTRUE(compact)) 3.1 else NULL
  if (isTRUE(compact) && is.null(breaks)) {
    max_pct <- limits[2] * 100
    if (max_pct <= 15) {
      breaks <- c(0, limits[2])
    } else {
      breaks <- c(0, limits[2] / 2, limits[2])
    }
    breaks <- unique(breaks)
  }

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(
      xlim = if (isTRUE(compact)) c(0, 10) else c(0, 10),
      ylim = if (isTRUE(compact)) c(0.08, 0.98) else c(0.12, 1.02),
      clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        if (isTRUE(compact)) 6 else 4,
        if (isTRUE(compact)) 10 else 6,
        if (isTRUE(compact)) 4 else 4,
        if (isTRUE(compact)) 10 else 6
      )
    )

  heatmap_share_legend_section(
    p,
    cx = 5,
    heat_colors = heat_colors,
    title = title,
    limits = limits,
    breaks = breaks,
    bar_half = bar_half,
    label_size = label_size
  )
}

#' Stacked dual-team heatmap legend with one title and shared tick labels
plot_heatmap_share_legend_stacked <- function(top_colors,
                                              bottom_colors,
                                              title = "Share of attacking actions",
                                              limits = c(0, 0.2),
                                              breaks = NULL) {
  if (is.null(breaks)) {
    max_pct <- limits[2] * 100
    step <- if (max_pct <= 12) 2 else if (max_pct <= 20) 5 else 10
    breaks <- seq(0, limits[2], by = step / 100)
    breaks <- breaks[breaks <= limits[2] + 1e-9]
  }

  title_style <- legend_title_ggpar()
  label_style <- legend_label_ggpar()
  cx <- 5
  bar_half <- 1.5
  title_y <- 0.94
  top_bar_y <- 0.66
  bottom_bar_y <- 0.46
  label_y <- 0.22
  tick_df <- tibble::tibble(
    x = cx - bar_half + (breaks - limits[1]) / diff(limits) * (2 * bar_half),
    label = scales::percent(breaks, accuracy = 1)
  )

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(0.08, 1.02), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))

  p <- heatmap_share_legend_bar(
    p,
    cx = cx,
    heat_colors = top_colors,
    marker_y = top_bar_y,
    bar_half = bar_half
  )
  p <- heatmap_share_legend_bar(
    p,
    cx = cx,
    heat_colors = bottom_colors,
    marker_y = bottom_bar_y,
    bar_half = bar_half
  )

  p +
    ggplot2::annotate(
      "text",
      x = cx,
      y = title_y,
      label = title,
      family = title_style$family,
      size = title_style$size,
      colour = title_style$colour,
      fontface = title_style$fontface
    ) +
    ggplot2::geom_text(
      data = tick_df,
      ggplot2::aes(x = .data$x, y = label_y, label = .data$label),
      family = label_style$family,
      size = label_style$size,
      colour = label_style$colour,
      fontface = label_style$fontface
    )
}

#' Two team heatmap legends side by side
plot_heatmap_share_legend_pair <- function(left_colors,
                                           right_colors,
                                           left_title,
                                           right_title,
                                           left_limits = c(0, 0.2),
                                           right_limits = c(0, 0.2)) {
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(0.12, 1.02), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))

  p <- heatmap_share_legend_section(
    p,
    cx = 2.5,
    heat_colors = left_colors,
    title = left_title,
    limits = left_limits,
    bar_half = 1.2
  )
  heatmap_share_legend_section(
    p,
    cx = 7.5,
    heat_colors = right_colors,
    title = right_title,
    limits = right_limits,
    bar_half = 1.2
  )
}

#' Combine binned heatmap with a single aligned legend row beneath the pitch
assemble_player_heatmap <- function(heatmap_plot,
                                    heat_colors,
                                    legend_title = "Share of attacking actions",
                                    legend_limits = NULL,
                                    legend_height_frac = 0.14,
                                    compact_legend = FALSE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(legend_limits)) {
    legend_limits <- c(0, 0.2)
  }

  legend_block <- plot_heatmap_share_legend_row(
    heat_colors = heat_colors,
    title = legend_title,
    limits = legend_limits,
    compact = isTRUE(compact_legend)
  )

  patchwork::wrap_plots(
    list(heatmap_plot, legend_block),
    ncol = 1,
    heights = c(1, if (isTRUE(compact_legend)) 0.10 else legend_height_frac)
  )
}

#' Combine shot map with a single aligned legend row beneath the pitch
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

  legend_block <- plot_shot_map_legend_row(
    shot_colors = shot_colors,
    limits = xg_limits,
    icon_color = icon_color,
    icon_set = icon_set,
    show_body_part = TRUE,
    show_xg = isTRUE(show_xg_legend),
    show_trajectory = isTRUE(show_trajectory_legend),
    show_ball = isTRUE(show_goal_net_ball_legend)
  )

  legend_height <- 0.20

  patchwork::wrap_plots(
    list(shot_plot, legend_block),
    ncol = 1,
    heights = c(1, legend_height)
  )
}
