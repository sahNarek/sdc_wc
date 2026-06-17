#' Event types counted as on-ball touches for reception-zone heatmaps
PLAYER_TOUCH_TYPES <- c(
  "Pass", "Carry", "Ball Receipt*", "Shot", "Dribble",
  "Ball Recovery", "Dispossessed", "Duel"
)

#' Event types counted as attacking actions for binned player heatmaps
PLAYER_ATTACKING_TYPES <- c(
  "Pass", "Carry", "Ball Receipt*", "Shot", "Dribble"
)

#' Blue-to-red gradient matching the article style-guide reception zones
palette_reception_zones <- function(n = 9) {
  grDevices::colorRampPalette(c(
    "#0B2D5B",
    SDC_PALETTE[["blue"]],
    "#5BA3D9",
    SDC_PALETTE[["orange"]],
    SDC_PALETTE[["red"]]
  ))(n)
}

#' Smooth pass-heatmap gradient (light blue → blue → orange → red)
palette_pass_heatmap <- function(n = 9) {
  grDevices::colorRampPalette(c(
    "#F7FBFF",
    "#C6DBEF",
    "#6BAED6",
    SDC_PALETTE[["blue"]],
    "#FEE0D2",
    SDC_PALETTE[["orange"]],
    SDC_PALETTE[["red"]]
  ))(n)
}

#' Binned zone heatmap gradient — avoids near-white lows (team / player grids)
palette_binned_heatmap <- function(color = SDC_PALETTE[["blue"]], n = 9) {
  grDevices::colorRampPalette(c(
    gradient_lightest(color, mix = 0.58),
    gradient_lightest(color, mix = 0.36),
    color,
    gradient_darkest(color, amount = 0.18)
  ))(n)
}

#' Infer whether a team attacks toward high x in each period (from shot locations)
infer_team_attacking_high_x <- function(events_df) {
  events_df %>%
    dplyr::filter(
      .data$type.name == "Shot",
      !is.na(.data$location.x),
      !is.na(.data$team.name),
      !is.na(.data$period)
    ) %>%
    dplyr::group_by(.data$team.name, .data$period) %>%
    dplyr::summarise(
      attacks_high_x = stats::median(.data$location.x, na.rm = TRUE) >= 60,
      .groups = "drop"
    )
}

#' Normalize event coordinates to the opponent's half (goal line at x = 120)
normalize_opponent_half_coords <- function(x, y, attacks_high_x) {
  if (attacks_high_x) {
    list(x = x, y = y)
  } else {
    list(x = 120 - x, y = 80 - y)
  }
}

#' Bin a player's touches in the opponent's half into a 3 (width) x 6 (depth) grid
compute_player_reception_zones <- function(events_df,
                                           player_id = NULL,
                                           player_name = NULL,
                                           match_id = NULL,
                                           n_cols = 3,
                                           n_rows = 6,
                                           touch_types = PLAYER_TOUCH_TYPES) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }
  if (!is.null(player_id)) {
    data <- data %>% dplyr::filter(.data$player.id == !!player_id)
  }
  if (!is.null(player_name)) {
    data <- data %>%
      dplyr::filter(
        .data$player.name == !!player_name |
          .data$player_display_name == !!player_name
      )
  }

  if (nrow(data) == 0) {
    stop("No events found for the selected player.", call. = FALSE)
  }

  team_name <- data$team.name[1]
  direction <- infer_team_attacking_high_x(events_df)

  touches <- data %>%
    dplyr::filter(
      .data$type.name %in% touch_types,
      !is.na(.data$location.x),
      !is.na(.data$location.y)
    ) %>%
    dplyr::left_join(direction, by = c("team.name", "period")) %>%
    dplyr::mutate(
      attacks_high_x = dplyr::coalesce(.data$attacks_high_x, TRUE),
      norm = purrr::pmap(
        list(.data$location.x, .data$location.y, .data$attacks_high_x),
        normalize_opponent_half_coords
      ),
      pitch_x = purrr::map_dbl(.data$norm, "x"),
      pitch_y = purrr::map_dbl(.data$norm, "y")
    ) %>%
    dplyr::filter(.data$pitch_x >= 60) %>%
    dplyr::mutate(
      depth = .data$pitch_x - 60,
      x_bin = as.integer(cut(
        .data$depth,
        breaks = seq(0, 60, length.out = n_rows + 1),
        include.lowest = TRUE,
        labels = FALSE
      )),
      y_bin = as.integer(cut(
        .data$pitch_y,
        breaks = seq(0, 80, length.out = n_cols + 1),
        include.lowest = TRUE,
        labels = FALSE
      ))
    ) %>%
    dplyr::filter(!is.na(.data$x_bin), !is.na(.data$y_bin))

  total_touches <- nrow(touches)
  if (total_touches == 0) {
    stop(
      "No on-ball touches found in the opponent's half for this player.",
      call. = FALSE
    )
  }

  depth_step <- 60 / n_rows
  width_step <- 80 / n_cols

  zone_counts <- touches %>%
    dplyr::count(.data$x_bin, .data$y_bin, name = "zone_touches")

  grid <- tidyr::expand_grid(
    x_bin = seq_len(n_rows),
    y_bin = seq_len(n_cols)
  ) %>%
    dplyr::left_join(zone_counts, by = c("x_bin", "y_bin")) %>%
    dplyr::mutate(
      zone_touches = dplyr::coalesce(.data$zone_touches, 0L),
      share_of_touches = .data$zone_touches / total_touches,
      xmin = (.data$y_bin - 1) * width_step,
      xmax = .data$y_bin * width_step,
      ymin = 60 + (.data$x_bin - 1) * depth_step,
      ymax = 60 + .data$x_bin * depth_step,
      label = sprintf("%d%%", round(.data$share_of_touches * 100)),
      team.name = team_name,
      total_touches = total_touches
    )

  grid
}

#' UC9: Player reception-zone heatmap (style-guide article layout)
viz_player_reception_zones <- function(events_df,
                                       player_id = NULL,
                                       player_name = NULL,
                                       match_id = NULL,
                                       meta = NULL,
                                       n_cols = 3,
                                       n_rows = 6,
                                       heat_colors = NULL,
                                       title = NULL,
                                       subtitle = NULL,
                                       section_label = "RECEPTION ZONES",
                                       touch_types = PLAYER_TOUCH_TYPES) {
  zones <- compute_player_reception_zones(
    events_df,
    player_id = player_id,
    player_name = player_name,
    match_id = match_id,
    n_cols = n_cols,
    n_rows = n_rows,
    touch_types = touch_types
  )

  player_label <- player_display_label(
    events_df,
    player_id = player_id,
    player_name = player_name
  )

  if (is.null(subtitle)) {
    matchup <- if (!is.null(meta)) {
      paste(meta$display_home, "vs", meta$display_away)
    } else {
      NULL
    }
    competition <- if (!is.null(meta) && !is.null(meta$competition_name)) {
      paste(meta$competition_name, meta$season_name)
    } else {
      "World Cup 2026"
    }
    subtitle <- paste0(
      section_label,
      "\nHeatmap of touches in the opponent's half (",
      competition,
      if (!is.null(matchup)) paste0(" | ", matchup) else "",
      ")"
    )
  }

  colors <- heat_colors %||% palette_reception_zones(9)

  ggplot(zones) +
    geom_rect(
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$share_of_touches
      ),
      colour = "white",
      linewidth = 0.45
    ) +
    geom_text(
      aes(
        x = (.data$xmin + .data$xmax) / 2,
        y = (.data$ymin + .data$ymax) / 2,
        label = .data$label
      ),
      colour = "white",
      fontface = "bold",
      size = 4.2,
      family = SDC_FONTS$body
    ) +
    draw_pitch_opponent_half(colour = "white", linewidth = 0.55) +
    scale_fill_gradientn(
      colours = colors,
      limits = c(0, NA),
      oob = scales::squish,
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, 80), expand = c(0, 0)) +
    scale_y_continuous(limits = c(60, 120), expand = c(0, 0)) +
    coord_fixed(ratio = 80 / 60) +
    labs(
      title = title %||% toupper(player_label),
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = paste0(
        "Share of ", zones$total_touches[1],
        " on-ball touches in the opponent's half."
      )
    ) +
    theme_sdc() +
    theme(
      plot.title = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 24,
        colour = SDC_PALETTE[["blue"]],
        hjust = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 12,
        colour = SDC_PALETTE[["blue"]],
        hjust = 0,
        lineheight = 1.25,
        margin = margin(b = 10)
      ),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 12, r = 16, b = 8, l = 16)
    )
}

#' Player action locations on a normalized full pitch
filter_player_pitch_locations <- function(events_df,
                                          player_id = NULL,
                                          player_name = NULL,
                                          match_id = NULL,
                                          action_types = PLAYER_ATTACKING_TYPES,
                                          normalize_direction = TRUE) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }
  if (!is.null(player_id)) {
    data <- data %>% dplyr::filter(.data$player.id == !!player_id)
  }
  if (!is.null(player_name)) {
    data <- data %>%
      dplyr::filter(
        .data$player.name == !!player_name |
          .data$player_display_name == !!player_name
      )
  }

  if (nrow(data) == 0) {
    stop("No events found for the selected player.", call. = FALSE)
  }

  actions <- data %>%
    dplyr::filter(
      .data$type.name %in% action_types,
      !is.na(.data$location.x),
      !is.na(.data$location.y)
    )

  if (nrow(actions) == 0) {
    stop("No located actions found for the selected player.", call. = FALSE)
  }

  if (!normalize_direction) {
    return(actions %>% dplyr::mutate(pitch_x = .data$location.x, pitch_y = .data$location.y))
  }

  direction <- infer_team_attacking_high_x(events_df)

  actions %>%
    dplyr::left_join(direction, by = c("team.name", "period")) %>%
    dplyr::mutate(
      attacks_high_x = dplyr::coalesce(.data$attacks_high_x, TRUE),
      norm = purrr::pmap(
        list(.data$location.x, .data$location.y, .data$attacks_high_x),
        normalize_opponent_half_coords
      ),
      pitch_x = purrr::map_dbl(.data$norm, "x"),
      pitch_y = purrr::map_dbl(.data$norm, "y")
    )
}

#' Player pass locations for a horizontal KDE heatmap
filter_player_pass_locations <- function(events_df,
                                         player_id = NULL,
                                         player_name = NULL,
                                         match_id = NULL,
                                         completed_only = FALSE,
                                         normalize_direction = TRUE) {
  passes <- filter_player_pitch_locations(
    events_df,
    player_id = player_id,
    player_name = player_name,
    match_id = match_id,
    action_types = "Pass",
    normalize_direction = normalize_direction
  )

  if (completed_only) {
    passes <- passes %>% dplyr::filter(is.na(.data$pass.outcome.name))
  }

  if (nrow(passes) == 0) {
    stop("No pass locations found for the selected player.", call. = FALSE)
  }

  passes
}

#' Bin a player's attacking actions into a pitch grid (default 6 x 5 zones)
compute_player_attacking_heatmap <- function(events_df,
                                               player_id = NULL,
                                               player_name = NULL,
                                               match_id = NULL,
                                               n_x_bins = 6,
                                               n_y_bins = 5,
                                               action_types = PLAYER_ATTACKING_TYPES,
                                               normalize_direction = TRUE) {
  actions <- filter_player_pitch_locations(
    events_df,
    player_id = player_id,
    player_name = player_name,
    match_id = match_id,
    action_types = action_types,
    normalize_direction = normalize_direction
  )

  x_breaks <- seq(0, 120, length.out = n_x_bins + 1)
  y_breaks <- seq(0, 80, length.out = n_y_bins + 1)

  total_actions <- nrow(actions)

  actions %>%
    dplyr::mutate(
      pitch_x = pmin(pmax(.data$pitch_x, 0), 120),
      pitch_y = pmin(pmax(.data$pitch_y, 0), 80),
      x_bin = as.integer(cut(
        .data$pitch_x,
        breaks = x_breaks,
        include.lowest = TRUE,
        labels = FALSE
      )),
      y_bin = as.integer(cut(
        .data$pitch_y,
        breaks = y_breaks,
        include.lowest = TRUE,
        labels = FALSE
      ))
    ) %>%
    dplyr::filter(!is.na(.data$x_bin), !is.na(.data$y_bin)) %>%
    dplyr::count(.data$x_bin, .data$y_bin, name = "zone_actions") %>%
    tidyr::complete(
      x_bin = seq_len(n_x_bins),
      y_bin = seq_len(n_y_bins),
      fill = list(zone_actions = 0L)
    ) %>%
    dplyr::mutate(
      share_of_actions = .data$zone_actions / total_actions,
      xmin = x_breaks[.data$x_bin],
      xmax = x_breaks[.data$x_bin + 1],
      ymin = y_breaks[.data$y_bin],
      ymax = y_breaks[.data$y_bin + 1],
      player_label = player_display_label(
        events_df,
        player_id = player_id,
        player_name = player_name
      ),
      total_actions = total_actions
    )
}

#' UC9b: Horizontal pass heatmap (smooth KDE contours, full pitch)
viz_player_pass_heatmap <- function(events_df,
                                      player_id = NULL,
                                      player_name = NULL,
                                      match_id = NULL,
                                      meta = NULL,
                                      opponent_name = NULL,
                                      completed_only = FALSE,
                                      normalize_direction = TRUE,
                                      bins = 9,
                                      alpha = 0.72,
                                      heat_colors = NULL,
                                      title = NULL,
                                      subtitle = NULL) {
  passes <- filter_player_pass_locations(
    events_df,
    player_id = player_id,
    player_name = player_name,
    match_id = match_id,
    completed_only = completed_only,
    normalize_direction = normalize_direction
  )

  player_label <- player_display_label(
    events_df,
    player_id = player_id,
    player_name = player_name
  )

  if (is.null(opponent_name) && !is.null(meta)) {
    opponent_name <- resolve_match_opponent(meta, passes$team.name[1])
  }

  labels <- if (is.null(title) && !is.null(meta)) {
    article_player_chart_labels(
      meta = meta,
      player_label = player_label,
      chart_descriptor = "pass heatmap",
      detail = paste0(
        "Kernel density of ", nrow(passes), " pass origins.",
        " Locations normalized so the player's team attacks left to right."
      ),
      team_name = passes$team.name[1]
    )
  } else {
    list(
      title = title %||% paste(
        "Pass heatmap", player_label, "vs", opponent_name %||% "opponent"
      ),
      subtitle = subtitle,
      caption = NULL
    )
  }

  if (!is.null(title)) {
    labels$title <- title
  }
  if (!is.null(subtitle)) {
    labels$subtitle <- subtitle
  }

  colors <- heat_colors %||% palette_pass_heatmap(bins)
  colors[1] <- grDevices::rgb(1, 1, 1, alpha = 0)

  ggplot(passes) +
    geom_density_2d_filled(
      aes(x = .data$pitch_x, y = .data$pitch_y, fill = after_stat(level)),
      bins = bins,
      alpha = alpha,
      colour = NA,
      show.legend = FALSE
    ) +
    draw_pitch_markings(colour = "black", linewidth = 0.55) +
    scale_fill_manual(values = colors, guide = "none") +
    scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 80 / 120) +
    labs(
      title = labels$title,
      subtitle = labels$subtitle,
      caption = labels$caption,
      x = NULL,
      y = NULL
    ) +
    theme_sdc_article() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 10, r = 12, b = 8, l = 12)
    )
}

#' UC9c: Binned attacking-actions heatmap for one player (6 x 5 grid)
viz_player_attacking_heatmap <- function(events_df,
                                         player_id = NULL,
                                         player_name = NULL,
                                         match_id = NULL,
                                         meta = NULL,
                                         n_x_bins = 6,
                                         n_y_bins = 5,
                                         heat_color = SDC_PALETTE[["blue"]],
                                         lightest_color = NULL,
                                         gradient_colors = NULL,
                                         action_types = PLAYER_ATTACKING_TYPES,
                                         normalize_direction = TRUE,
                                         title = NULL,
                                         subtitle = NULL,
                                         caption = NULL) {
  heatmap_df <- compute_player_attacking_heatmap(
    events_df,
    player_id = player_id,
    player_name = player_name,
    match_id = match_id,
    n_x_bins = n_x_bins,
    n_y_bins = n_y_bins,
    action_types = action_types,
    normalize_direction = normalize_direction
  )

  if (nrow(heatmap_df) == 0 || all(heatmap_df$zone_actions == 0)) {
    stop("No attacking actions found for the selected player.", call. = FALSE)
  }

  player_label <- heatmap_df$player_label[1]
  team_name <- filter_player_pitch_locations(
    events_df,
    player_id = player_id,
    player_name = player_name,
    match_id = match_id,
    action_types = action_types,
    normalize_direction = normalize_direction
  )$team.name[1]

  labels <- if (is.null(title) && !is.null(meta)) {
    article_player_chart_labels(
      meta = meta,
      player_label = player_label,
      chart_descriptor = "attacking actions heatmap",
      detail = paste0(
        "Share of on-ball attacking actions by pitch zone.",
        " Includes passes, carries, ball receipts, dribbles and shots."
      ),
      team_name = team_name
    )
  } else {
    list(
      title = title %||% player_label,
      subtitle = subtitle,
      caption = caption
    )
  }

  if (!is.null(title)) {
    labels$title <- title
  }
  if (!is.null(subtitle)) {
    labels$subtitle <- subtitle
  }
  if (!is.null(caption)) {
    labels$caption <- caption
  }

  heat_colors <- if (!is.null(gradient_colors)) {
    gradient_colors
  } else if (!is.null(lightest_color)) {
    resolve_single_hue_gradient(
      color = heat_color,
      lightest_color = lightest_color,
      n = 9
    )
  } else {
    palette_binned_heatmap(color = heat_color, n = 9)
  }

  legend_max <- max(heatmap_df$share_of_actions, na.rm = TRUE)
  legend_limit <- min(0.25, max(0.15, ceiling(legend_max * 100 / 5) * 5 / 100))

  pitch_plot <- ggplot(heatmap_df) +
    geom_rect(
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$share_of_actions
      ),
      colour = NA,
      alpha = 0.92
    ) +
    draw_pitch_markings(colour = "black", linewidth = 0.55) +
    draw_pitch_outer_border(colour = "black", linewidth = 1.0) +
    scale_fill_gradientn(
      colours = heat_colors,
      limits = c(0, legend_limit),
      oob = scales::squish,
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 80 / 120) +
    labs(
      title = labels$title,
      subtitle = labels$subtitle,
      caption = labels$caption,
      x = NULL,
      y = NULL
    ) +
    theme_sdc_article() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 10, r = 12, b = 4, l = 12)
    )

  assemble_player_heatmap(
    pitch_plot,
    heat_colors = heat_colors,
    legend_title = "Share of attacking actions",
    legend_limits = c(0, legend_limit)
  )
}

#' Resolve one or more players for heatmap charts
resolve_players_for_heatmap <- function(events_df,
                                        player_ids = NULL,
                                        player_names = NULL) {
  if (!is.null(player_ids)) {
    return(purrr::map(player_ids, function(pid) {
      pid <- as.numeric(pid)
      row <- events_df %>%
        dplyr::filter(.data$`player.id` == !!pid) %>%
        dplyr::slice(1)
      if (nrow(row) == 0) {
        stop("Player not found in match events: id ", pid, call. = FALSE)
      }
      list(
        player.id = pid,
        player.name = row$player.name,
        player_label = player_display_label(events_df, player_id = pid),
        slug = figure_slug(player_display_label(events_df, player_id = pid))
      )
    }))
  }

  if (!is.null(player_names)) {
    return(purrr::map(player_names, function(name) {
      resolve_featured_player(events_df, name)
    }))
  }

  stop("Specify player_ids or player_names.", call. = FALSE)
}

#' Shared heatmap palette and legend ceiling for one or more player grids
prepare_attacking_heatmap_scale <- function(heatmap_df,
                                            heat_color = SDC_PALETTE[["blue"]],
                                            lightest_color = NULL,
                                            gradient_colors = NULL) {
  heat_colors <- if (!is.null(gradient_colors)) {
    gradient_colors
  } else if (!is.null(lightest_color)) {
    resolve_single_hue_gradient(
      color = heat_color,
      lightest_color = lightest_color,
      n = 9
    )
  } else {
    palette_binned_heatmap(color = heat_color, n = 9)
  }

  legend_max <- max(heatmap_df$share_of_actions, na.rm = TRUE)
  legend_limit <- min(
    0.25,
    max(0.15, ceiling(legend_max * 100 / 5) * 5 / 100)
  )

  list(
    heat_colors = heat_colors,
    legend_limit = legend_limit
  )
}

#' Collect binned heatmap data for an ordered set of players
collect_attacking_heatmap_row <- function(events_df,
                                          player_ids = NULL,
                                          player_names = NULL,
                                          match_id = NULL,
                                          n_x_bins = 6,
                                          n_y_bins = 5,
                                          action_types = PLAYER_ATTACKING_TYPES,
                                          normalize_direction = TRUE) {
  players <- resolve_players_for_heatmap(
    events_df,
    player_ids = player_ids,
    player_names = player_names
  )
  player_levels <- vapply(players, `[[`, character(1), "player_label")

  heatmap_df <- purrr::map_dfr(players, function(player) {
    compute_player_attacking_heatmap(
      events_df,
      player_id = player$player.id,
      match_id = match_id,
      n_x_bins = n_x_bins,
      n_y_bins = n_y_bins,
      action_types = action_types,
      normalize_direction = normalize_direction
    ) %>%
      dplyr::mutate(player_label = player$player_label)
  }) %>%
    dplyr::mutate(
      player_label = factor(.data$player_label, levels = player_levels)
    )

  list(
    players = players,
    player_levels = player_levels,
    heatmap_df = heatmap_df
  )
}

#' Build one faceted attacking-heatmap row (no title or legend)
build_attacking_heatmap_row_plot <- function(heatmap_df,
                                             heat_colors,
                                             legend_limit,
                                             player_levels = NULL,
                                             show_direction_arrow = TRUE,
                                             strip_size = 10,
                                             panel_spacing = unit(0.7, "lines")) {
  if (is.null(player_levels)) {
    player_levels <- levels(heatmap_df$player_label)
  }

  heatmap_df <- heatmap_df %>%
    dplyr::mutate(
      player_label = factor(.data$player_label, levels = player_levels)
    )

  pitch_plot <- ggplot(heatmap_df) +
    geom_rect(
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$share_of_actions
      ),
      colour = NA,
      alpha = 0.92
    ) +
    draw_pitch_markings(colour = "black", linewidth = 0.55) +
    draw_pitch_outer_border(colour = "black", linewidth = 1.0)

  if (isTRUE(show_direction_arrow)) {
    arrow_df <- tibble::tibble(
      player_label = factor(player_levels, levels = player_levels),
      x = 18,
      xend = 102,
      y = 83.5,
      yend = 83.5
    )
    pitch_plot <- pitch_plot +
      geom_segment(
        data = arrow_df,
        aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
        arrow = arrow(
          length = unit(0.1, "inches"),
          ends = "last",
          type = "closed"
        ),
        linewidth = 0.35,
        colour = "black",
        inherit.aes = FALSE
      )
  }

  pitch_plot +
    scale_fill_gradientn(
      colours = heat_colors,
      limits = c(0, legend_limit),
      oob = scales::squish,
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_y_reverse(limits = c(86, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 80 / 120) +
    facet_wrap(~player_label, nrow = 1) +
    labs(x = NULL, y = NULL) +
    theme_sdc_article() +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      strip.text = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = strip_size,
        colour = "#111111"
      ),
      strip.background = element_blank(),
      panel.spacing.x = panel_spacing,
      plot.margin = margin(t = 4, r = 6, b = 0, l = 6)
    )
}

#' UC9d: Binned attacking-actions heatmaps for multiple players in one row
viz_players_attacking_heatmap_row <- function(events_df,
                                              player_ids = NULL,
                                              player_names = NULL,
                                              match_id = NULL,
                                              meta = NULL,
                                              n_x_bins = 6,
                                              n_y_bins = 5,
                                              heat_color = SDC_PALETTE[["blue"]],
                                              lightest_color = NULL,
                                              gradient_colors = NULL,
                                              action_types = PLAYER_ATTACKING_TYPES,
                                              normalize_direction = TRUE,
                                              title = NULL,
                                              subtitle = NULL,
                                              caption = NULL,
                                              show_direction_arrow = TRUE) {
  row_data <- collect_attacking_heatmap_row(
    events_df,
    player_ids = player_ids,
    player_names = player_names,
    match_id = match_id,
    n_x_bins = n_x_bins,
    n_y_bins = n_y_bins,
    action_types = action_types,
    normalize_direction = normalize_direction
  )
  heatmap_df <- row_data$heatmap_df

  if (nrow(heatmap_df) == 0 || all(heatmap_df$zone_actions == 0)) {
    stop("No attacking actions found for the selected players.", call. = FALSE)
  }

  scale_info <- prepare_attacking_heatmap_scale(
    heatmap_df,
    heat_color = heat_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors
  )

  if (is.null(title) && !is.null(meta)) {
    title <- article_chart_title(
      meta$display_home,
      paste0("attacking actions heatmap vs ", meta$display_away)
    )
  }
  if (is.null(subtitle) && !is.null(meta)) {
    subtitle <- match_chart_subtitle(meta)
  }
  if (is.null(caption)) {
    caption <- paste0(
      "Share of on-ball attacking actions by pitch zone.",
      " Includes passes, carries, ball receipts, dribbles and shots."
    )
  }

  n_players <- length(row_data$players)
  strip_size <- if (n_players >= 4) 10 else 12
  panel_spacing <- if (n_players >= 4) {
    unit(0.7, "lines")
  } else {
    unit(1.4, "lines")
  }

  pitch_plot <- build_attacking_heatmap_row_plot(
    heatmap_df = heatmap_df,
    heat_colors = scale_info$heat_colors,
    legend_limit = scale_info$legend_limit,
    player_levels = row_data$player_levels,
    show_direction_arrow = show_direction_arrow,
    strip_size = strip_size,
    panel_spacing = panel_spacing
  ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme(
      plot.title = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 22,
        colour = "#111111",
        hjust = 0.5,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = SDC_FONTS$body,
        size = 13,
        colour = "#444444",
        hjust = 0.5,
        margin = margin(b = 8)
      ),
      plot.caption = element_text(
        family = SDC_FONTS$body,
        size = 12,
        colour = "#555555",
        hjust = 0.5,
        margin = margin(t = 4)
      ),
      plot.margin = margin(t = 8, r = 8, b = 2, l = 8)
    )

  assemble_player_heatmap(
    pitch_plot,
    heat_colors = scale_info$heat_colors,
    legend_title = "Share of attacking actions",
    legend_limits = c(0, scale_info$legend_limit),
    legend_height_frac = if (n_players >= 4) 0.12 else 0.14
  )
}

#' UC9e: Two-row attacking heatmaps (home row + away row) in one 16:9 figure
viz_match_attacking_heatmaps_grid <- function(events_df,
                                              home_player_ids = NULL,
                                              away_player_ids = NULL,
                                              home_player_names = NULL,
                                              away_player_names = NULL,
                                              match_id = NULL,
                                              meta = NULL,
                                              home_heat_color = "#74ACDF",
                                              away_heat_color = SDC_PALETTE[["green"]],
                                              n_x_bins = 6,
                                              n_y_bins = 5,
                                              action_types = PLAYER_ATTACKING_TYPES,
                                              normalize_direction = TRUE,
                                              title = NULL,
                                              subtitle = NULL,
                                              caption = NULL,
                                              show_direction_arrow = TRUE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  home_row <- collect_attacking_heatmap_row(
    events_df,
    player_ids = home_player_ids,
    player_names = home_player_names,
    match_id = match_id,
    n_x_bins = n_x_bins,
    n_y_bins = n_y_bins,
    action_types = action_types,
    normalize_direction = normalize_direction
  )
  away_row <- collect_attacking_heatmap_row(
    events_df,
    player_ids = away_player_ids,
    player_names = away_player_names,
    match_id = match_id,
    n_x_bins = n_x_bins,
    n_y_bins = n_y_bins,
    action_types = action_types,
    normalize_direction = normalize_direction
  )

  if (nrow(home_row$heatmap_df) == 0 || nrow(away_row$heatmap_df) == 0) {
    stop("No attacking actions found for the selected players.", call. = FALSE)
  }

  home_scale <- prepare_attacking_heatmap_scale(
    home_row$heatmap_df,
    heat_color = home_heat_color
  )
  away_scale <- prepare_attacking_heatmap_scale(
    away_row$heatmap_df,
    heat_color = away_heat_color
  )
  legend_limit <- max(home_scale$legend_limit, away_scale$legend_limit)

  if (is.null(title) && !is.null(meta)) {
    title <- paste0(
      "Attacking actions heatmap: ",
      meta$display_home,
      " vs ",
      meta$display_away
    )
  }
  if (is.null(subtitle) && !is.null(meta)) {
    subtitle <- match_chart_subtitle(meta)
  }
  if (is.null(caption)) {
    caption <- paste0(
      "Share of on-ball attacking actions by pitch zone.",
      " Includes passes, carries, ball receipts, dribbles and shots."
    )
  }

  home_plot <- build_attacking_heatmap_row_plot(
    heatmap_df = home_row$heatmap_df,
    heat_colors = home_scale$heat_colors,
    legend_limit = legend_limit,
    player_levels = home_row$player_levels,
    show_direction_arrow = show_direction_arrow,
    strip_size = 10,
    panel_spacing = unit(0.7, "lines")
  )
  away_plot <- build_attacking_heatmap_row_plot(
    heatmap_df = away_row$heatmap_df,
    heat_colors = away_scale$heat_colors,
    legend_limit = legend_limit,
    player_levels = away_row$player_levels,
    show_direction_arrow = show_direction_arrow,
    strip_size = 10,
    panel_spacing = unit(0.7, "lines")
  )

  legend_block <- plot_heatmap_share_legend_stacked(
    top_colors = home_scale$heat_colors,
    bottom_colors = away_scale$heat_colors,
    title = "Share of attacking actions",
    limits = c(0, legend_limit)
  )

  grid_block <- patchwork::wrap_plots(
    list(home_plot, away_plot),
    ncol = 1,
    heights = c(1, 1)
  )

  patchwork::wrap_plots(
    list(grid_block, legend_block),
    ncol = 1,
    heights = c(1, 0.13)
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption,
      theme = theme_sdc_article()
    )
}
