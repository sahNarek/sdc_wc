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

#' Add per-shot coloured icon paths to shot data
add_colored_shot_icons <- function(shots_df,
                                   shot_color = SDC_PALETTE[["blue"]],
                                   limits = c(0, 0.8),
                                   icon_set = "footprint") {
  colors <- palette_single_gradient(color = shot_color, n = 11, lightest = "#EAF3FA")

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
    x = seq_along(body_part),
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

  ggplot2::ggplot(legend_df, ggplot2::aes(x = x, y = y)) +
    ggimage::geom_image(ggplot2::aes(image = icon), size = 0.16) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      y = 0.7,
      family = SDC_FONTS$body,
      size = 3.2,
      colour = "#333333"
    ) +
    ggplot2::xlim(0.4, 3.6) +
    ggplot2::ylim(0.5, 1.15) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}

#' Combine shot map with icon legend
assemble_shot_map <- function(shot_plot, icon_set = "footprint") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  legend <- plot_body_part_legend(icon_set = icon_set)
  patchwork::wrap_plots(shot_plot, legend, ncol = 1, heights = c(1, 0.1))
}
