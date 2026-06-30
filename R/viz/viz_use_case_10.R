#' Short display label for passing-network nodes
passing_network_label <- function(name, display_name = NULL) {
  label <- dplyr::coalesce(display_name, name)
  if (is.na(label) || !nzchar(label)) {
    return("")
  }
  parts <- strsplit(label, " ", fixed = TRUE)[[1]]
  if (length(parts) <= 1) {
    return(label)
  }
  parts[[length(parts)]]
}

#' Map half label to StatsBomb period id
half_to_period <- function(half) {
  if (is.null(half)) {
    return(NULL)
  }
  half <- match.arg(half, c("first", "second"))
  if (half == "first") {
    1L
  } else {
    2L
  }
}

#' Filter events to one match half (period 1 or 2)
filter_events_by_half <- function(events_df, half = NULL) {
  period_id <- half_to_period(half)
  if (is.null(period_id)) {
    return(events_df)
  }
  events_df %>%
    dplyr::filter(.data$period == .env$period_id)
}

#' Completed team passes with passer and recipient ids
filter_team_completed_passes <- function(events_df,
                                         team_name,
                                         match_id = NULL,
                                         half = NULL) {
  data <- events_df %>%
    dplyr::filter(
      .data$`type.name` == "Pass",
      is.na(.data$`pass.outcome.name`),
      .data$`team.name` == !!team_name,
      !is.na(.data$`player.id`),
      !is.na(.data$`pass.recipient.id`)
    )

  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }
  if (!is.null(half)) {
    data <- filter_events_by_half(data, half = half)
  }

  data
}

#' Average pitch coordinates per player for network node placement
compute_passing_network_positions <- function(events_df,
                                              team_name,
                                              match_id = NULL,
                                              half = NULL,
                                              normalize_direction = TRUE) {
  touches <- events_df %>%
    dplyr::filter(
      .data$`team.name` == !!team_name,
      !is.na(.data$`player.id`),
      !is.na(.data$location.x),
      !is.na(.data$location.y)
    )

  if (!is.null(match_id)) {
    touches <- touches %>% dplyr::filter(.data$match_id == !!match_id)
  }
  if (!is.null(half)) {
    touches <- filter_events_by_half(touches, half = half)
  }

  if (nrow(touches) == 0) {
    return(tibble::tibble())
  }

  if (isTRUE(normalize_direction)) {
    direction <- infer_team_attacking_high_x(events_df)
    touches <- touches %>%
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
  } else {
    touches <- touches %>%
      dplyr::mutate(
        pitch_x = .data$location.x,
        pitch_y = .data$location.y
      )
  }

  touches %>%
    dplyr::group_by(.data$`player.id`) %>%
    dplyr::summarise(
      player_label = passing_network_label(
        .data$`player.name`[1],
        .data$player_display_name[1]
      ),
      x = mean(.data$pitch_x, na.rm = TRUE),
      y = mean(.data$pitch_y, na.rm = TRUE),
      touches = dplyr::n(),
      .groups = "drop"
    )
}

#' Edge list and node positions for one team's passing network
compute_passing_network <- function(events_df,
                                    team_name,
                                    match_id = NULL,
                                    half = NULL,
                                    min_passes = 3,
                                    normalize_direction = TRUE) {
  passes <- filter_team_completed_passes(
    events_df,
    team_name = team_name,
    match_id = match_id,
    half = half
  )

  if (nrow(passes) == 0) {
    half_label <- if (!is.null(half)) paste0(" (", half, " half)") else ""
    stop(
      "No completed passes with recipients found for ",
      team_name,
      half_label,
      call. = FALSE
    )
  }

  positions <- compute_passing_network_positions(
    events_df,
    team_name = team_name,
    match_id = match_id,
    half = half,
    normalize_direction = normalize_direction
  )

  edges <- passes %>%
    dplyr::transmute(
      passer_id = .data$`player.id`,
      recipient_id = .data$`pass.recipient.id`
    ) %>%
    dplyr::count(.data$passer_id, .data$recipient_id, name = "pass_count") %>%
    dplyr::filter(.data$pass_count >= min_passes)

  if (nrow(edges) == 0) {
    stop(
      "No pass combinations with at least ", min_passes,
      " passes for ", team_name,
      call. = FALSE
    )
  }

  active_ids <- unique(c(edges$passer_id, edges$recipient_id))
  nodes <- positions %>%
    dplyr::filter(.data$`player.id` %in% active_ids) %>%
    dplyr::rename(player_id = `player.id`)

  if (nrow(nodes) == 0) {
    stop("No node positions found for passing network: ", team_name, call. = FALSE)
  }

  edges <- edges %>%
    dplyr::inner_join(
      nodes %>% dplyr::select(player_id, x_from = x, y_from = y),
      by = c("passer_id" = "player_id")
    ) %>%
    dplyr::inner_join(
      nodes %>% dplyr::select(player_id, x_to = x, y_to = y),
      by = c("recipient_id" = "player_id")
    )

  node_degree <- edges %>%
    dplyr::select(player_id = passer_id, pass_count) %>%
    dplyr::bind_rows(
      edges %>% dplyr::select(player_id = recipient_id, pass_count)
    ) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(total_passes = sum(.data$pass_count), .groups = "drop")

  nodes <- nodes %>%
    dplyr::left_join(node_degree, by = c("player_id" = "player_id")) %>%
    dplyr::mutate(
      total_passes = dplyr::coalesce(.data$total_passes, 0L),
      team_name = team_name
    )

  list(nodes = nodes, edges = edges)
}

#' Build one team's passing-network ggplot layer (no title)
build_passing_network_plot <- function(network,
                                       team_color = SDC_PALETTE[["blue"]],
                                       substitute_ids = NULL,
                                       edge_alpha = NULL,
                                       edge_alpha_range = c(0.04, 0.92),
                                       max_edge_width = 4.5,
                                       label_size = 2.8,
                                       compact = FALSE,
                                       show_substitute_rings = FALSE,
                                       pitch_style = c("sb", "default")) {
  pitch_style <- match.arg(pitch_style)
  nodes <- network$nodes
  edges <- network$edges
  max_count <- max(edges$pass_count, na.rm = TRUE)
  min_count <- min(edges$pass_count, na.rm = TRUE)
  substitute_ids <- substitute_ids %||% integer(0)
  nodes <- nodes %>%
    dplyr::mutate(is_sub = .data$player_id %in% substitute_ids)

  size_range <- if (isTRUE(compact)) c(3.2, 8.5) else c(5.5, 13)
  stroke_base <- if (isTRUE(compact)) 0.85 else 1.05
  pitch_linewidth <- if (isTRUE(compact)) 0.4 else 0.35
  plot_margin <- if (isTRUE(compact)) 4 else 1

  pitch_layers <- if (pitch_style == "sb") {
    c(
      list(
        ggplot2::annotate(
          "rect",
          xmin = 0,
          xmax = 120,
          ymin = 0,
          ymax = 80,
          fill = "#FAFAFA",
          colour = NA
        )
      ),
      draw_pitch_markings(colour = "#C8C8C8", linewidth = pitch_linewidth),
      list(
        ggplot2::scale_y_reverse(),
        ggplot2::coord_fixed(ratio = 105 / 100, clip = "off")
      )
    )
  } else {
    c(
      draw_pitch_markings(
        colour = "black",
        linewidth = if (compact) 0.45 else 0.55
      ),
      draw_pitch_outer_border(
        colour = "black",
        linewidth = if (compact) 0.85 else 1.0
      )
    )
  }

  p <- ggplot2::ggplot()
  for (layer in pitch_layers) {
    p <- p + layer
  }

  edge_layer <- if (is.null(edge_alpha)) {
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(
        x = .data$x_from,
        y = .data$y_from,
        xend = .data$x_to,
        yend = .data$y_to,
        linewidth = .data$pass_count,
        alpha = .data$pass_count
      ),
      colour = team_color,
      lineend = "round"
    )
  } else {
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(
        x = .data$x_from,
        y = .data$y_from,
        xend = .data$x_to,
        yend = .data$y_to,
        linewidth = .data$pass_count
      ),
      colour = team_color,
      alpha = edge_alpha,
      lineend = "round"
    )
  }

  p <- p +
    edge_layer +
    ggplot2::scale_linewidth_continuous(
      range = c(0.25, max_edge_width),
      limits = c(min_count, max_count),
      guide = "none"
    )

  if (is.null(edge_alpha)) {
    p <- p +
      ggplot2::scale_alpha_continuous(
        range = edge_alpha_range,
        limits = c(min_count, max_count),
        guide = "none"
      )
  }

  if (isTRUE(show_substitute_rings) && any(nodes$is_sub)) {
    p <- p +
      ggplot2::geom_point(
        data = nodes %>% dplyr::filter(.data$is_sub),
        ggplot2::aes(x = .data$x, y = .data$y),
        shape = 1,
        size = if (compact) 4.2 else 5.4,
        colour = team_color,
        stroke = 1.2
      )
  }

  p <- p +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$total_passes),
      fill = team_color,
      colour = team_color,
      shape = 21,
      stroke = stroke_base
    ) +
    ggplot2::scale_size_continuous(range = size_range, guide = "none") +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$player_label),
      family = SDC_FONTS$body,
      size = label_size,
      colour = "#111111",
      fontface = "bold",
      vjust = -1.05
    ) +
    ggplot2::labs(x = NULL, y = NULL)

  if (pitch_style == "default") {
    p <- p +
      ggplot2::scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
      ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
      ggplot2::coord_fixed(ratio = 80 / 120, clip = "off")
  }

  p +
    theme_sdc(base_size = if (compact) 9 else 10) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(plot_margin, plot_margin, plot_margin, plot_margin)
    )
}

#' UC10: Match passing networks for home and away in one 16:9 figure
viz_match_passing_networks <- function(events_df,
                                       match_id = NULL,
                                       meta = NULL,
                                       home_team = NULL,
                                       away_team = NULL,
                                       home_color = SDC_PALETTE[["blue"]],
                                       away_color = SDC_PALETTE[["green"]],
                                       min_passes = 3,
                                       normalize_direction = TRUE,
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       team_labels = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(home_team) || is.null(away_team)) {
    if (is.null(meta) || nrow(meta) == 0) {
      stop("Provide home_team/away_team or match meta.", call. = FALSE)
    }
    home_team <- meta$home_team[1]
    away_team <- meta$away_team[1]
  }

  home_label <- if (!is.null(team_labels)) {
    team_labels[[home_team]] %||% meta$display_home[1]
  } else {
    meta$display_home[1] %||% home_team
  }
  away_label <- if (!is.null(team_labels)) {
    team_labels[[away_team]] %||% meta$display_away[1]
  } else {
    meta$display_away[1] %||% away_team
  }

  home_net <- compute_passing_network(
    events_df,
    team_name = home_team,
    match_id = match_id,
    min_passes = min_passes,
    normalize_direction = normalize_direction
  )
  away_net <- compute_passing_network(
    events_df,
    team_name = away_team,
    match_id = match_id,
    min_passes = min_passes,
    normalize_direction = normalize_direction
  )

  if (is.null(title) && !is.null(meta)) {
    title <- paste0(
      "Passing networks: ",
      home_label,
      " vs ",
      away_label
    )
  }
  if (is.null(subtitle) && !is.null(meta)) {
    subtitle <- match_chart_subtitle(meta)
  }
  if (is.null(caption)) {
    caption <- paste0(
      "Completed passes between players. Line thickness shows pass volume.",
      " Only combinations with at least ", min_passes, " passes are shown."
    )
  }

  home_plot <- build_passing_network_plot(home_net, team_color = home_color) +
    ggplot2::labs(title = home_label) +
    theme_sdc_article() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 14,
        colour = "#111111",
        hjust = 0.5,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 0, l = 6)
    )

  away_plot <- build_passing_network_plot(away_net, team_color = away_color) +
    ggplot2::labs(title = away_label) +
    theme_sdc_article() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 14,
        colour = "#111111",
        hjust = 0.5,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 0, l = 6)
    )

  patchwork::wrap_plots(
    list(home_plot, away_plot),
    ncol = 2
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption,
      theme = theme_sdc_article()
    )
}

#' Players who entered during the match half (for substitute rings)
identify_substitute_players <- function(events_df,
                                        team_name,
                                        half = c("first", "second"),
                                        lineups_df = NULL) {
  half <- match.arg(half)
  data <- ensure_viz_aliases(events_df) %>%
    dplyr::filter(.data$`team.name` == !!team_name, !is.na(.data$`player.id`))

  first_period <- data %>%
    dplyr::group_by(.data$`player.id`) %>%
    dplyr::summarise(first_period = min(.data$period, na.rm = TRUE), .groups = "drop")

  if (half == "second") {
    return(first_period %>%
      dplyr::filter(.data$first_period >= 2L) %>%
      dplyr::pull(.data$`player.id`))
  }

  sub_ids <- data %>%
    dplyr::filter(.data$`type.name` == "Substitution") %>%
    dplyr::pull(.data$`player.id`)
  sub_ids <- sub_ids[!is.na(sub_ids)]

  first_period %>%
    dplyr::filter(.data$first_period >= 2L, .data$`player.id` %in% sub_ids) %>%
    dplyr::pull(.data$`player.id`)
}

#' Safe passing network compute with adjustable min edge weight
try_passing_network <- function(events_df,
                                team_name,
                                match_id = NULL,
                                half = NULL,
                                min_passes = 3,
                                normalize_direction = TRUE) {
  tryCatch(
    compute_passing_network(
      events_df,
      team_name = team_name,
      match_id = match_id,
      half = half,
      min_passes = min_passes,
      normalize_direction = normalize_direction
    ),
    error = function(e) NULL
  )
}

#' One half passing network panel (internal)
build_half_passing_network_panel <- function(events_df,
                                             team_name,
                                             team_color,
                                             half,
                                             match_id = NULL,
                                             lineups_df = NULL,
                                             min_passes = 3,
                                             compact = FALSE,
                                             pitch_style = "sb") {
  net <- try_passing_network(
    events_df,
    team_name = team_name,
    match_id = match_id,
    half = half,
    min_passes = min_passes
  )
  if (is.null(net)) {
    net <- try_passing_network(
      events_df,
      team_name = team_name,
      match_id = match_id,
      half = half,
      min_passes = max(2L, min_passes - 1L)
    )
  }
  if (is.null(net)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Insufficient pass links",
          family = SDC_FONTS$body,
          size = 3.2,
          colour = "#666666"
        ) +
        ggplot2::theme_void()
    )
  }

  sub_ids <- identify_substitute_players(
    events_df,
    team_name = team_name,
    half = half,
    lineups_df = lineups_df
  )

  build_passing_network_plot(
    net,
    team_color = team_color,
    substitute_ids = sub_ids,
    compact = compact,
    pitch_style = pitch_style,
    label_size = if (compact) 2.6 else 3.4,
    max_edge_width = if (compact) 4.0 else 6.0,
    edge_alpha = NULL,
    show_substitute_rings = FALSE
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}

#' Full-match passing network panel (both halves combined)
build_full_match_passing_network_panel <- function(events_df,
                                                   team_name,
                                                   team_color,
                                                   match_id = NULL,
                                                   min_passes = 4L,
                                                   team_label = NULL,
                                                   label_size = 3.8) {
  net <- try_passing_network(
    events_df,
    team_name = team_name,
    match_id = match_id,
    half = NULL,
    min_passes = min_passes
  )
  if (is.null(net)) {
    net <- try_passing_network(
      events_df,
      team_name = team_name,
      match_id = match_id,
      half = NULL,
      min_passes = max(2L, min_passes - 1L)
    )
  }
  if (is.null(net)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "Insufficient pass links",
          family = SDC_FONTS$body,
          size = 3.2,
          colour = "#666666"
        ) +
        ggplot2::theme_void()
    )
  }

  build_passing_network_plot(
    net,
    team_color = team_color,
    substitute_ids = integer(0),
    edge_alpha = NULL,
    edge_alpha_range = c(0.03, 0.95),
    label_size = label_size,
    max_edge_width = 5.5,
    show_substitute_rings = FALSE,
    pitch_style = "sb"
  ) +
    ggplot2::labs(title = team_label) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$body,
        face = "bold",
        size = 11,
        colour = team_color,
        hjust = 0.5,
        margin = ggplot2::margin(b = 1, t = 0)
      ),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

#' UC10c: full-match passing networks (one pitch per team, both halves combined)
viz_match_passing_networks_combined <- function(events_df,
                                                match_id = NULL,
                                                meta = NULL,
                                                home_team = NULL,
                                                away_team = NULL,
                                                home_color = SDC_PALETTE[["green"]],
                                                away_color = SDC_PALETTE[["red"]],
                                                min_passes_home = 4L,
                                                min_passes_away = 4L,
                                                title = "Passing networks",
                                                subtitle = "Full match · darker links = more passes between players") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(home_team) || is.null(away_team)) {
    if (is.null(meta) || nrow(meta) == 0) {
      stop("Provide home_team/away_team or match meta.", call. = FALSE)
    }
    home_team <- meta$home_team[[1]]
    away_team <- meta$away_team[[1]]
  }

  home_label <- meta$display_home[[1]] %||% home_team
  away_label <- meta$display_away[[1]] %||% away_team

  p_home <- build_full_match_passing_network_panel(
    events_df,
    team_name = home_team,
    team_color = home_color,
    match_id = match_id,
    min_passes = min_passes_home,
    team_label = home_label,
    label_size = 3.9
  )

  p_away <- build_full_match_passing_network_panel(
    events_df,
    team_name = away_team,
    team_color = away_color,
    match_id = match_id,
    min_passes = min_passes_away,
    team_label = away_label,
    label_size = 3.9
  )

  patchwork::wrap_plots(
    list(p_home, p_away),
    ncol = 2,
    widths = c(0.5, 0.5)
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme_sdc(base_size = 10) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            family = SDC_FONTS$title,
            face = "bold",
            size = 12,
            hjust = 0,
            colour = "#111111"
          ),
          plot.subtitle = ggplot2::element_text(
            family = SDC_FONTS$body,
            size = 9,
            hjust = 0,
            colour = "#555555"
          ),
          plot.margin = ggplot2::margin(b = 0, l = 0, r = 0, t = 2)
        )
    )
}

#' Legend strip for half-by-half passing networks
passing_network_halves_legend <- function(home_color,
                                          away_color,
                                          home_label,
                                          away_label) {
  link_x <- c(0.52, 0.58, 0.64, 0.72, 0.78, 0.84)
  link_w <- c(0.35, 0.55, 0.85, 0.35, 0.55, 0.85)

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.02,
      y = 0.72,
      label = home_label,
      hjust = 0,
      family = SDC_FONTS$body,
      size = 3.2,
      fontface = "bold",
      colour = home_color
    ) +
    ggplot2::annotate(
      "point",
      x = 0.015,
      y = 0.72,
      size = 2.8,
      colour = home_color,
      fill = home_color,
      shape = 21,
      stroke = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = 0.02,
      y = 0.38,
      label = away_label,
      hjust = 0,
      family = SDC_FONTS$body,
      size = 3.2,
      fontface = "bold",
      colour = away_color
    ) +
    ggplot2::annotate(
      "point",
      x = 0.015,
      y = 0.38,
      size = 2.8,
      colour = away_color,
      fill = away_color,
      shape = 21,
      stroke = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x = 0.50,
      y = 0.88,
      label = "Link strength",
      family = SDC_FONTS$body,
      size = 2.8,
      fontface = "bold",
      colour = "#333333"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.58,
      y = 0.88,
      label = "By half",
      family = SDC_FONTS$body,
      size = 2.8,
      fontface = "bold",
      colour = "#333333"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.86,
      y = 0.88,
      label = "Player status",
      family = SDC_FONTS$body,
      size = 2.8,
      fontface = "bold",
      colour = "#333333"
    )

  for (i in seq_along(link_x[1:3])) {
    p <- p +
      ggplot2::annotate(
        "segment",
        x = link_x[i],
        xend = link_x[i] + 0.04,
        y = 0.62,
        yend = 0.62,
        colour = home_color,
        linewidth = link_w[i],
        lineend = "round"
      )
  }
  for (i in seq_along(link_x[4:6])) {
    idx <- i + 3
    p <- p +
      ggplot2::annotate(
        "segment",
        x = link_x[idx],
        xend = link_x[idx] + 0.04,
        y = 0.28,
        yend = 0.28,
        colour = away_color,
        linewidth = link_w[idx],
        lineend = "round"
      )
  }

  p +
    ggplot2::annotate(
      "point",
      x = 0.84,
      y = 0.62,
      size = 2.8,
      colour = home_color,
      fill = home_color,
      shape = 21
    ) +
    ggplot2::annotate(
      "text",
      x = 0.88,
      y = 0.62,
      label = "Starter",
      hjust = 0,
      family = SDC_FONTS$body,
      size = 2.6,
      colour = "#333333"
    ) +
    ggplot2::annotate(
      "point",
      x = 0.84,
      y = 0.28,
      size = 3.2,
      colour = home_color,
      shape = 1,
      stroke = 1
    ) +
    ggplot2::annotate(
      "text",
      x = 0.88,
      y = 0.28,
      label = "Substitute",
      hjust = 0,
      family = SDC_FONTS$body,
      size = 2.6,
      colour = "#333333"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 4, 0, 4))
}

#' UC10b: 2x2 half-by-half passing networks (teams x halves)
viz_match_passing_networks_halves <- function(events_df,
                                              match_id = NULL,
                                              meta = NULL,
                                              home_team = NULL,
                                              away_team = NULL,
                                              home_color = SDC_PALETTE[["green"]],
                                              away_color = SDC_PALETTE[["red"]],
                                              lineups_df = NULL,
                                              min_passes_home = 3L,
                                              min_passes_away = 3L,
                                              min_passes_away_second = 2L,
                                              title = "Passing behaviour by half",
                                              subtitle = "Larger nodes = greater completed-pass involvement") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(home_team) || is.null(away_team)) {
    if (is.null(meta) || nrow(meta) == 0) {
      stop("Provide home_team/away_team or match meta.", call. = FALSE)
    }
    home_team <- meta$home_team[[1]]
    away_team <- meta$away_team[[1]]
  }

  home_label <- meta$display_home[[1]] %||% home_team
  away_label <- meta$display_away[[1]] %||% away_team

  p_home_1 <- build_half_passing_network_panel(
    events_df, home_team, home_color, "first", match_id, lineups_df, min_passes_home
  )
  p_home_2 <- build_half_passing_network_panel(
    events_df, home_team, home_color, "second", match_id, lineups_df, min_passes_home
  )
  p_away_1 <- build_half_passing_network_panel(
    events_df, away_team, away_color, "first", match_id, lineups_df, min_passes_away
  )
  p_away_2 <- build_half_passing_network_panel(
    events_df,
    away_team,
    away_color,
    "second",
    match_id,
    lineups_df,
    min_passes_away_second
  )

  col_header <- function(label) {
    ggplot2::ggplot() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = toupper(label),
        family = SDC_FONTS$body,
        fontface = "bold",
        size = 4.2,
        colour = SDC_PALETTE[["blue"]]
      ) +
      ggplot2::theme_void()
  }

  legend <- passing_network_halves_legend(
    home_color = home_color,
    away_color = away_color,
    home_label = home_label,
    away_label = away_label
  )

  top_row <- patchwork::wrap_plots(
    list(p_home_1, p_home_2),
    ncol = 2
  )

  mid_row <- patchwork::wrap_plots(
    list(col_header("First half"), col_header("Second half")),
    ncol = 2
  )

  bottom_row <- patchwork::wrap_plots(
    list(p_away_1, p_away_2),
    ncol = 2
  )

  patchwork::wrap_plots(
    list(legend, top_row, mid_row, bottom_row),
    ncol = 1,
    heights = c(0.09, 0.43, 0.05, 0.43)
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme_sdc(base_size = 10) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            family = SDC_FONTS$title,
            face = "bold",
            size = 12,
            hjust = 0,
            colour = "#111111"
          ),
          plot.subtitle = ggplot2::element_text(
            family = SDC_FONTS$body,
            size = 9,
            hjust = 0,
            colour = "#555555"
          ),
          plot.margin = ggplot2::margin(b = 2, l = 2, r = 2, t = 2)
        )
    )
}
