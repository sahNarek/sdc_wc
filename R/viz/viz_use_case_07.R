#' UC7: Shot map coloured by expected goals
viz_shot_map <- function(events_df,
                         player_name = NULL,
                         player_id = NULL,
                         match_id = NULL,
                         title = NULL,
                         subtitle = NULL,
                         exclude_penalties = TRUE,
                         shot_color = SDC_PALETTE[["blue"]]) {
  data <- events_df %>%
    filter(type.name == "Shot")

  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  if (exclude_penalties) {
    data <- data %>%
      filter(shot.type.name != "Penalty" | is.na(shot.type.name))
  }

  if (!is.null(player_id)) {
    data <- data %>% filter(player.id == player_id)
  }

  if (!is.null(player_name)) {
    data <- data %>%
      filter(player.name == player_name | player_display_name == player_name)
  }

  if (nrow(data) == 0) {
    stop("No shots found for the selected player.", call. = FALSE)
  }

  label <- coalesce(data$player_display_name[1], data$player.name[1])
  shot_colors <- palette_single_gradient(color = shot_color, n = 9, lightest = "#EAF3FA")

  ggplot() +
    draw_pitch_half_attacking() +
    geom_point(
      data = data,
      aes(
        x = location.x,
        y = location.y,
        fill = shot.statsbomb_xg,
        shape = shot.body_part.name
      ),
      size = 5,
      alpha = 0.9,
      colour = "#333333"
    ) +
    scale_fill_gradientn(
      colours = shot_colors,
      limits = c(0, 0.8),
      oob = scales::squish,
      name = "Expected goals\n(xG)"
    ) +
    scale_shape_manual(
      values = c(
        "Head" = 21,
        "Right Foot" = 23,
        "Left Foot" = 24,
        "Other" = 22
      ),
      name = "Body part"
    ) +
    guides(
      fill = guide_colourbar(title.position = "top"),
      shape = guide_legend(override.aes = list(size = 5, fill = "#333333"))
    ) +
    labs(
      title = title %||% paste0(label, ": shot map"),
      subtitle = subtitle,
      caption = "Marker size is fixed; colour intensity reflects shot quality (xG). Penalties excluded."
    ) +
    theme_sdc() +
    theme(
      legend.position = "top",
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

#' Pick top goal scorer for shot map
top_goal_scorer <- function(events_df, match_id = NULL, team_name = NULL) {
  data <- events_df %>%
    filter(type.name == "Shot", shot.outcome.name == "Goal")

  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  if (!is.null(team_name)) {
    data <- data %>% filter(team.name == team_name)
  }

  data %>%
    count(player.name, player.id, sort = TRUE) %>%
    slice(1)
}
