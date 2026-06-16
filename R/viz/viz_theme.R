#' SDC 6-color graphics palette (El Mundial de los Datos)
SDC_PALETTE <- c(
  blue   = "#1F77B4",
  orange = "#FF7F0E",
  green  = "#2CA02C",
  red    = "#D62728",
  purple = "#9467BD",
  cyan   = "#17BECF"
)

SDC_PALETTE_VEC <- unname(unlist(SDC_PALETTE))

#' Typography — Style Guide for Articles and Social Media
SDC_FONTS <- list(
  title = "barlow",
  body  = "opensans"
)

FIGURE_SPECS <- list(
  `16_9` = list(width_px = 1280, height_px = 750, dpi = 96),
  `4_5`  = list(width_px = 864,  height_px = 1080, dpi = 96),
  `1_1`  = list(width_px = 800,  height_px = 800, dpi = 96)
)

#' Single-hue gradient from light tint to full palette colour
palette_single_gradient <- function(color = SDC_PALETTE[["blue"]],
                                  n = 11,
                                  lightest = "#F5FAFD") {
  grDevices::colorRampPalette(c(lightest, color))(n)
}

#' Article-grade ggplot theme
theme_sdc <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = SDC_FONTS$body) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(
        family = SDC_FONTS$body,
        size = base_size,
        colour = "#333333"
      ),
      axis.text = element_text(
        family = SDC_FONTS$body,
        size = base_size - 1,
        colour = "#333333"
      ),
      legend.position = "bottom",
      legend.title = element_text(
        family = SDC_FONTS$body,
        size = base_size,
        colour = "#333333"
      ),
      legend.text = element_text(
        family = SDC_FONTS$body,
        size = base_size - 1,
        colour = "#333333"
      ),
      plot.title = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = base_size + 10,
        colour = "#111111",
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        family = SDC_FONTS$body,
        size = base_size + 1,
        colour = "#444444",
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        family = SDC_FONTS$body,
        size = base_size - 2,
        colour = "#555555",
        hjust = 0,
        margin = margin(t = 8)
      ),
      strip.text = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = base_size + 1,
        colour = "#111111"
      )
    )
}

scale_fill_sdc <- function(...) {
  scale_fill_manual(values = SDC_PALETTE_VEC, ...)
}

scale_color_sdc <- function(...) {
  scale_color_manual(values = SDC_PALETTE_VEC, ...)
}

#' Human-readable file name for exported charts
figure_slug <- function(..., sep = "_") {
  parts <- list(...)
  parts <- vapply(parts, function(x) {
    x <- as.character(x)
    x <- tryCatch(iconv(x, from = "", to = "ASCII//TRANSLIT"), error = function(e) x)
    x <- gsub("[^A-Za-z0-9]+", "_", x)
    gsub("^_+|_+$", "", x)
  }, character(1))
  paste(parts[nzchar(parts)], collapse = sep)
}

#' Save ggplot with required pixel dimensions and size cap
save_figure <- function(plot,
                        path,
                        format = c("16_9", "4_5", "1_1"),
                        max_kb = 500,
                        dpi = NULL) {
  format <- match.arg(format)
  spec <- FIGURE_SPECS[[format]]
  dpi <- dpi %||% spec$dpi

  width_in <- spec$width_px / dpi
  height_in <- spec$height_px / dpi

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width_in,
    height = height_in,
    units = "in",
    dpi = dpi,
    bg = "white"
  )

  size_kb <- file.info(path)$size / 1024
  if (size_kb > max_kb) {
    for (try_dpi in c(90, 84, 78, 72)) {
      ggplot2::ggsave(
        filename = path,
        plot = plot,
        width = spec$width_px / try_dpi,
        height = spec$height_px / try_dpi,
        units = "in",
        dpi = try_dpi,
        bg = "white"
      )
      size_kb <- file.info(path)$size / 1024
      if (size_kb <= max_kb) {
        break
      }
    }
  }

  invisible(path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

team_colors_default <- function(home_team, away_team) {
  setNames(
    c(SDC_PALETTE[["blue"]], SDC_PALETTE[["orange"]]),
    c(home_team, away_team)
  )
}

#' Map StatsBomb team names to report display labels (e.g. Germany -> Alemania)
team_display_map <- function(home_team, away_team, display_home, display_away) {
  stats <- c(home_team, away_team)
  labels <- c(display_home, display_away)
  setNames(labels, stats)
}

#' Replace internal team names with display labels in a data frame column
apply_team_display_labels <- function(df, team_col = "Team", name_map = NULL) {
  if (is.null(name_map) || !team_col %in% names(df)) {
    return(df)
  }

  df[[team_col]] <- dplyr::recode(df[[team_col]], !!!name_map, .default = df[[team_col]])
  df
}
