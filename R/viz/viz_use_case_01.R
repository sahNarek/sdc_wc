#' UC1: Team shots and goals totals
compute_team_shots_goals <- function(events_df,
                                     match_id = NULL,
                                     per_game = FALSE) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  result <- data %>%
    group_by(team.name) %>%
    summarise(
      shots = sum(type.name == "Shot", na.rm = TRUE),
      goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
      .groups = "drop"
    )

  if (per_game) {
    n_matches <- n_distinct(data$match_id)
    if (n_matches > 1) {
      result <- result %>%
        mutate(
          shots = shots / n_matches,
          goals = goals / n_matches
        )
    }
  }

  result %>%
    rename(Team = team.name)
}

viz_team_shots_goals <- function(events_df,
                                 match_id = NULL,
                                 per_game = FALSE,
                                 title = NULL,
                                 subtitle = NULL) {
  shots_goals <- compute_team_shots_goals(
    events_df,
    match_id = match_id,
    per_game = per_game
  )

  y_label <- if (per_game) "Shots per match" else "Total shots"

  ggplot(shots_goals, aes(x = reorder(Team, shots), y = shots, fill = Team)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = goals), hjust = -0.2, family = SDC_FONTS$body, size = 4) +
    scale_fill_sdc() +
    labs(
      title = title %||% "Shots and goals by team",
      subtitle = subtitle,
      x = NULL,
      y = y_label,
      caption = "Numbers on bars indicate goals scored."
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    theme_sdc()
}
