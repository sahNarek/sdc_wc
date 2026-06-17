#' Wyscout UC-W1: Goals and assists by player (aggregate gold CSV)
viz_wyscout_goals_assists <- function(player_match_stats_df,
                                      match_id = NULL,
                                      team_name = NULL,
                                      top_n = 12,
                                      title = NULL,
                                      subtitle = NULL,
                                      team_labels = NULL) {
  data <- player_match_stats_df
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }

  if (!is.null(team_name)) {
    data <- data %>% dplyr::filter(.data$team_name == !!team_name)
  }

  data <- data %>%
    dplyr::mutate(
      goals = dplyr::coalesce(.data$player_match_goals, 0L),
      assists = dplyr::coalesce(.data$player_match_assists, 0L),
      contribution = .data$goals + .data$assists
    ) %>%
    dplyr::filter(.data$contribution > 0) %>%
    dplyr::arrange(dplyr::desc(.data$contribution)) %>%
    dplyr::slice_head(n = top_n)

  if (nrow(data) == 0) {
    stop("No goals or assists found in Wyscout player match stats.", call. = FALSE)
  }

  if (!is.null(team_labels) && "team_name" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        team_name = dplyr::recode(
          .data$team_name,
          !!!team_labels,
          .default = .data$team_name
        )
      )
  }

  plot_df <- data %>%
    dplyr::transmute(
      Player = .data$player_name,
      Team = .data$team_name,
      goals = .data$goals,
      assists = .data$assists
    ) %>%
    tidyr::pivot_longer(
      cols = c(.data$goals, .data$assists),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::recode(.data$metric, goals = "Goals", assists = "Assists")
    )

  ggplot(plot_df, aes(x = reorder(.data$Player, .data$value), y = .data$value, fill = .data$metric)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c(Goals = SDC_PALETTE[["blue"]], Assists = SDC_PALETTE[["orange"]])) +
    labs(
      title = title %||% "Goals and assists by player",
      subtitle = subtitle,
      x = NULL,
      y = "Count",
      fill = NULL,
      caption = "Player-level totals from match lineups. Event coordinates not available for this source."
    ) +
    coord_flip() +
    theme_sdc() +
    theme(legend.position = "top")
}

#' Wyscout UC-W2: Minutes played by starters
viz_wyscout_minutes_played <- function(player_match_stats_df,
                                       match_id = NULL,
                                       team_name = NULL,
                                       top_n = 14,
                                       title = NULL,
                                       subtitle = NULL,
                                       fill_colors = NULL) {
  data <- player_match_stats_df
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }

  if (!is.null(team_name)) {
    data <- data %>% dplyr::filter(.data$team_name == !!team_name)
  }

  data <- data %>%
    dplyr::filter(!is.na(.data$player_match_minutes), .data$player_match_minutes > 0) %>%
    dplyr::arrange(dplyr::desc(.data$player_match_minutes)) %>%
    dplyr::slice_head(n = top_n)

  if (nrow(data) == 0) {
    stop("No minutes data found in Wyscout player match stats.", call. = FALSE)
  }

  if (is.null(fill_colors)) {
    fill_colors <- setNames(
      SDC_PALETTE[c("blue", "orange")],
      unique(data$team_name)[seq_len(min(2, length(unique(data$team_name))))]
    )
  }

  ggplot(data, aes(x = reorder(.data$player_name, .data$player_match_minutes), y = .data$player_match_minutes, fill = .data$team_name)) +
    geom_col(width = 0.65, show.legend = TRUE) +
    scale_fill_manual(values = fill_colors, name = "Team") +
    labs(
      title = title %||% "Minutes played",
      subtitle = subtitle,
      x = NULL,
      y = "Minutes",
      caption = "Top players by minutes played."
    ) +
    coord_flip() +
    theme_sdc()
}
