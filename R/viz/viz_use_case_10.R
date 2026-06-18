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

#' Completed team passes with passer and recipient ids
filter_team_completed_passes <- function(events_df,
                                         team_name,
                                         match_id = NULL) {
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

  data
}

#' Average pitch coordinates per player for network node placement
compute_passing_network_positions <- function(events_df,
                                              team_name,
                                              match_id = NULL,
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
                                    min_passes = 3,
                                    normalize_direction = TRUE) {
  passes <- filter_team_completed_passes(
    events_df,
    team_name = team_name,
    match_id = match_id
  )

  if (nrow(passes) == 0) {
    stop("No completed passes with recipients found for ", team_name, call. = FALSE)
  }

  positions <- compute_passing_network_positions(
    events_df,
    team_name = team_name,
    match_id = match_id,
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
                                       edge_alpha = 0.55,
                                       max_edge_width = 4.5) {
  nodes <- network$nodes
  edges <- network$edges
  max_count <- max(edges$pass_count, na.rm = TRUE)

  ggplot2::ggplot() +
    draw_pitch_markings(colour = "black", linewidth = 0.55) +
    draw_pitch_outer_border(colour = "black", linewidth = 1.0) +
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
    ) +
    ggplot2::scale_linewidth_continuous(
      range = c(0.35, max_edge_width),
      limits = c(1, max_count),
      guide = "none"
    ) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, size = .data$total_passes),
      fill = "white",
      colour = team_color,
      shape = 21,
      stroke = 0.9
    ) +
    ggplot2::scale_size_continuous(range = c(2.8, 6.5), guide = "none") +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$player_label),
      family = SDC_FONTS$body,
      size = 2.8,
      colour = "#222222",
      fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / 120) +
    ggplot2::labs(x = NULL, y = NULL)
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
