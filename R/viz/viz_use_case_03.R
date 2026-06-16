#' UC3: Player shots per 90
compute_player_shots_per90 <- function(events_df,
                                       player_match_stats_df = NULL,
                                       match_id = NULL,
                                       min_minutes = 45) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  player_shots <- data %>%
    filter(!is.na(player.id)) %>%
    group_by(player.name, player.id, team.name) %>%
    summarise(shots = sum(type.name == "Shot", na.rm = TRUE), .groups = "drop")

  if (!is.null(player_match_stats_df)) {
    minutes_df <- player_match_stats_df %>%
      {
        if (!is.null(match_id)) filter(., match_id == !!match_id) else .
      } %>%
      transmute(
        player.id = player_id,
        minutes = player_match_minutes
      ) %>%
      group_by(player.id) %>%
      summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop")
  } else {
    minutes_df <- data %>%
      filter(!is.na(player.id)) %>%
      group_by(player.id) %>%
      summarise(minutes = max(minute, na.rm = TRUE), .groups = "drop")
  }

  player_shots %>%
    left_join(minutes_df, by = "player.id") %>%
    mutate(
      nineties = minutes / 90,
      shots_per90 = if_else(nineties > 0, shots / nineties, NA_real_)
    ) %>%
    filter(!is.na(minutes), minutes >= min_minutes) %>%
    arrange(desc(shots_per90)) %>%
    rename(
      Player = player.name,
      Team = team.name
    )
}

viz_player_shots_per90 <- function(events_df,
                                   player_match_stats_df = NULL,
                                   match_id = NULL,
                                   min_minutes = 45,
                                   top_n = 10,
                                   title = NULL,
                                   subtitle = NULL,
                                   team_labels = NULL) {
  chart_df <- compute_player_shots_per90(
    events_df,
    player_match_stats_df = player_match_stats_df,
    match_id = match_id,
    min_minutes = min_minutes
  ) %>%
    apply_team_display_labels(name_map = team_labels) %>%
    slice_max(shots_per90, n = top_n, with_ties = FALSE)

  ggplot(chart_df, aes(x = reorder(Player, shots_per90), y = shots_per90, fill = Team)) +
    geom_col(width = 0.6) +
    scale_fill_sdc() +
    labs(
      title = title %||% "Shots per 90 minutes",
      subtitle = subtitle,
      x = NULL,
      y = "Shots per 90 minutes",
      fill = "Team"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    theme_sdc()
}
