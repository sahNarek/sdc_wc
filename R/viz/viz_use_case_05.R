#' UC5: Expected goals and expected assists per 90
compute_xg_xga_contribution <- function(events_df,
                                        player_match_stats_df = NULL,
                                        match_id = NULL,
                                        min_minutes = 45,
                                        top_n = 15) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  xga_lookup <- data %>%
    filter(type.name == "Shot", !is.na(shot.key_pass_id)) %>%
    transmute(key_pass_id = shot.key_pass_id, xGA = shot.statsbomb_xg)

  shot_assists <- data %>%
    left_join(xga_lookup, by = c("id" = "key_pass_id")) %>%
    filter(pass.shot_assist == TRUE | pass.goal_assist == TRUE) %>%
    group_by(player.name, player.id, team.name) %>%
    summarise(xGA = sum(xGA, na.rm = TRUE), .groups = "drop")

  player_xg <- data %>%
    filter(type.name == "Shot") %>%
    filter(shot.type.name != "Penalty" | is.na(shot.type.name)) %>%
    group_by(player.name, player.id, team.name) %>%
    summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE), .groups = "drop") %>%
    left_join(shot_assists, by = c("player.name", "player.id", "team.name")) %>%
    mutate(
      xGA = replace_na(xGA, 0),
      xG_xGA = xG + xGA
    )

  if (!is.null(player_match_stats_df)) {
    minutes_df <- player_match_stats_df %>%
      {
        if (!is.null(match_id)) filter(., match_id == !!match_id) else .
      } %>%
      transmute(player.id = player_id, minutes = player_match_minutes) %>%
      group_by(player.id) %>%
      summarise(minutes = sum(minutes, na.rm = TRUE), .groups = "drop")

    player_xg <- player_xg %>%
      left_join(minutes_df, by = "player.id") %>%
      filter(minutes >= min_minutes) %>%
      mutate(
        nineties = minutes / 90,
        npxg_per90 = round(xG / nineties, 2),
        xga_per90 = round(xGA / nineties, 2),
        total_per90 = round(xG_xGA / nineties, 2)
      )
  } else {
    player_xg <- player_xg %>%
      mutate(
        npxg_per90 = xG,
        xga_per90 = xGA,
        total_per90 = xG_xGA
      )
  }

  player_xg %>%
    arrange(desc(total_per90)) %>%
    slice_head(n = top_n) %>%
    rename(Player = player.name, Team = team.name)
}

viz_xg_xga_contribution <- function(events_df,
                                    player_match_stats_df = NULL,
                                    match_id = NULL,
                                    min_minutes = 45,
                                    top_n = 15,
                                    title = "Expected goal contribution per 90",
                                    subtitle = NULL) {
  chart <- compute_xg_xga_contribution(
    events_df,
    player_match_stats_df = player_match_stats_df,
    match_id = match_id,
    min_minutes = min_minutes,
    top_n = top_n
  )

  chart_long <- chart %>%
    mutate(Player = factor(Player, levels = rev(Player[order(total_per90)]))) %>%
    select(
      Player,
      `Non-penalty xG` = npxg_per90,
      `xG assisted` = xga_per90
    ) %>%
    pivot_longer(-Player, names_to = "Metric", values_to = "Value")

  ggplot(chart_long, aes(
    x = Player,
    y = Value,
    fill = Metric
  )) +
    geom_col(colour = "white") +
    scale_fill_manual(
      values = c(
        "Non-penalty xG" = SDC_PALETTE[["blue"]],
        "xG assisted" = SDC_PALETTE[["orange"]]
      )
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Expected goals per 90 minutes",
      fill = "Metric",
      caption = paste(
        "Non-penalty xG measures the quality of shots taken.",
        "xG assisted measures the quality of chances created for teammates."
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_sdc()
}
