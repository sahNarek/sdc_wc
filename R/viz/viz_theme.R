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

#' Very light tint of a base colour for single-hue chart gradients
gradient_lightest <- function(color, mix = 0.94) {
  rgb <- grDevices::col2rgb(color) / 255
  blended <- mix * c(1, 1, 1) + (1 - mix) * as.numeric(rgb)
  grDevices::rgb(blended[[1]], blended[[2]], blended[[3]])
}

#' Darken a palette colour toward black (for high-xG shot icons)
gradient_darkest <- function(color, amount = 0.22) {
  rgb <- grDevices::col2rgb(color) / 255
  scaled <- pmax(0, rgb * (1 - amount))
  grDevices::rgb(scaled[[1]], scaled[[2]], scaled[[3]])
}

#' Shot-map gradient: medium tint → base palette → darker base (no near-white lows)
palette_shot_map_gradient <- function(color = SDC_PALETTE[["blue"]], n = 11) {
  grDevices::colorRampPalette(c(
    gradient_lightest(color, mix = 0.44),
    color,
    gradient_darkest(color, amount = 0.32)
  ))(n)
}

#' Resolve single-hue gradient colours (shot maps, heatmaps, icons)
resolve_single_hue_gradient <- function(color = SDC_PALETTE[["blue"]],
                                        lightest_color = NULL,
                                        gradient_colors = NULL,
                                        n = 11,
                                        variant = c("default", "shot_map")) {
  variant <- match.arg(variant)
  if (!is.null(gradient_colors)) {
    return(gradient_colors)
  }

  if (variant == "shot_map") {
    return(palette_shot_map_gradient(color = color, n = n))
  }

  palette_single_gradient(
    color = color,
    n = n,
    lightest = lightest_color %||% gradient_lightest(color)
  )
}

#' Title-case label for an SDC palette key (e.g. blue -> Blue)
palette_color_label <- function(palette_key) {
  key <- tolower(as.character(palette_key))
  if (!key %in% names(SDC_PALETTE)) {
    stop("Unknown palette colour: ", palette_key, call. = FALSE)
  }
  paste0(toupper(substr(key, 1, 1)), substr(key, 2, nchar(key)))
}

#' Iterate over all six SDC palette colours
iterate_sdc_palette <- function(callback) {
  for (palette_key in names(SDC_PALETTE)) {
    callback(
      palette_key = palette_key,
      color_name = palette_color_label(palette_key),
      color_hex = SDC_PALETTE[[palette_key]]
    )
  }
  invisible(NULL)
}

#' Save one ggplot per SDC palette colour
save_palette_figures <- function(plot_fn,
                                 base_path,
                                 format = c("16_9", "4_5", "1_1"),
                                 color_param = c("color", "shot_color", "heat_color")) {
  color_param <- match.arg(color_param)
  iterate_sdc_palette(function(color_name, color_hex, palette_key) {
    args <- list(color_hex)
    names(args) <- color_param
    plot <- do.call(plot_fn, args)
    path <- paste0(base_path, "_", color_name, ".png")
    save_figure(plot, path, format = format)
    invisible(path)
  })
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

#' One-line match score for article titles
match_score_line <- function(meta) {
  paste0(
    meta$display_home, " ", meta$home_score, "–", meta$away_score, " ",
    meta$display_away
  )
}

#' Article-style title for a featured player shot map (includes the scoreline)
player_shot_map_goal_net_article_title <- function(meta, player_label) {
  paste0(player_label, " in ", match_score_line(meta))
}

#' Short subtitle for article graphics (competition + venue)
short_match_chart_subtitle <- function(meta) {
  parts <- c(
    meta$competition_name,
    meta$season_name,
    if (!is.na(meta$stadium) && nzchar(meta$stadium)) meta$stadium else NULL
  )
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = " · ")
}

#' Subtitle line shared across match charts
match_chart_subtitle <- function(meta) {
  paste0(
    meta$competition_name, " ", meta$season_name,
    " | ", meta$display_home, " ", meta$home_score, "–", meta$away_score, " ",
    meta$display_away,
    if (!is.na(meta$stadium) && nzchar(meta$stadium)) paste0(" | ", meta$stadium) else ""
  )
}

#' Default chart titles for a match (override per game via report params or scripts)
default_chart_titles <- function(meta) {
  matchup <- paste(meta$display_home, "vs", meta$display_away)
  list(
    shots_goals = paste("Shots and goals:", matchup),
    shots_bar = paste("Total shots:", matchup),
    shots_per90 = "Shots per 90 minutes",
    xg_contribution = "Expected goal contribution per 90",
    defensive_heatmap = paste("Where teams defend:", matchup),
    pass_map_suffix = "completed passes into the penalty area",
    shot_map_suffix = "shot map",
    shot_map_left_foot_suffix = "shot map (left foot)",
    shot_map_goal_net_suffix = "shot map and goal mouth",
    shot_map_goal_net_title = NULL,
    shot_map_goal_net_subtitle = NULL,
    team_shot_map_suffix = "team shot map",
    match_shot_map_suffix = "match shot map",
    team_shot_map_goal_net_suffix = "team shot map and goal mouth",
    match_shot_map_goal_net_suffix = "match shot map and goal mouth",
    wyscout_goals_assists = "Goals and assists by player",
    wyscout_minutes = "Minutes played"
  )
}

#' Merge default titles with optional overrides
resolve_chart_titles <- function(meta, overrides = NULL) {
  utils::modifyList(default_chart_titles(meta), overrides %||% list())
}

#' Player-facing chart title
player_chart_title <- function(player_label, suffix = "shot map") {
  paste0(player_label, ": ", suffix)
}

#' Article-style chart title (Barlow Condensed Bold per style guide)
article_chart_title <- function(subject, descriptor) {
  paste0(subject, ": ", descriptor)
}

#' Article-style caption / alt-text line (Open Sans Regular)
article_chart_caption <- function(...) {
  paste(..., sep = " ")
}

#' Resolve opponent display name from match meta
resolve_match_opponent <- function(meta, team_name) {
  if (is.null(meta) || is.null(team_name)) {
    return(NULL)
  }
  if (identical(team_name, meta$home_team)) {
    meta$display_away
  } else {
    meta$display_home
  }
}

#' Default article labels for a player chart
article_player_chart_labels <- function(meta,
                                        player_label,
                                        chart_descriptor,
                                        detail = NULL,
                                        team_name = NULL) {
  opponent <- resolve_match_opponent(meta, team_name)
  descriptor <- if (!is.null(opponent) && nzchar(opponent)) {
    paste0(chart_descriptor, " vs ", opponent)
  } else {
    chart_descriptor
  }

  list(
    title = article_chart_title(player_label, descriptor),
    subtitle = if (!is.null(meta)) match_chart_subtitle(meta) else NULL,
    caption = if (!is.null(detail)) article_chart_caption(detail) else NULL
  )
}

#' Article figure typography overrides (titles: Barlow; body/captions: Open Sans)
theme_sdc_article <- function(base_size = 13) {
  theme_sdc(base_size = base_size) +
    theme(
      plot.title = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = base_size + 9,
        colour = "#111111",
        hjust = 0.5,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = SDC_FONTS$body,
        size = base_size,
        colour = "#444444",
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        family = SDC_FONTS$body,
        size = base_size - 1,
        colour = "#555555",
        hjust = 0.5,
        margin = margin(t = 4)
      ),
      legend.title = element_text(
        family = SDC_FONTS$body,
        size = base_size - 1,
        colour = "#333333"
      ),
      legend.text = element_text(
        family = SDC_FONTS$body,
        size = base_size - 2,
        colour = "#333333"
      )
    )
}

#' Display label for a player from event rows
player_display_label <- function(events_df, player_id = NULL, player_name = NULL) {
  data <- events_df
  if (!is.null(player_id)) {
    data <- data %>% dplyr::filter(player.id == !!player_id)
  }
  if (!is.null(player_name)) {
    data <- data %>%
      dplyr::filter(player.name == !!player_name | player_display_name == !!player_name)
  }
  if (nrow(data) == 0) {
    return(player_name %||% NA_character_)
  }
  dplyr::coalesce(data$player_display_name[1], data$player.name[1])
}

#' Lookup a featured player for shot-map sections
resolve_featured_player <- function(events_df, player_name) {
  row <- events_df %>%
    dplyr::filter(
      player.name == !!player_name | player_display_name == !!player_name
    ) %>%
    dplyr::slice(1)

  if (nrow(row) == 0) {
    stop("Player not found in match events: ", player_name, call. = FALSE)
  }

  label <- player_display_label(events_df, player_id = row$player.id)
  list(
    player.id = row$player.id,
    player.name = row$player.name,
    player_label = label,
    slug = figure_slug(label)
  )
}
