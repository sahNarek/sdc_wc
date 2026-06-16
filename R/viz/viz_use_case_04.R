#' UC4: Player pass map on pitch
viz_player_pass_map <- function(events_df,
                                player_id,
                                match_id = NULL,
                                title = NULL,
                                subtitle = NULL,
                                title_suffix = "completed passes into the penalty area",
                                pass_color = SDC_PALETTE[["blue"]],
                                box_only = TRUE) {
  data <- events_df %>%
    filter(
      type.name == "Pass",
      is.na(pass.outcome.name),
      player.id == player_id
    )

  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  if (box_only) {
    data <- data %>%
      filter(
        pass.end_location.x >= 102,
        pass.end_location.y >= 18,
        pass.end_location.y <= 62
      )
  }

  player_label <- data$player_display_name[1]
  if (is.na(player_label) || !nzchar(player_label)) {
    player_label <- data$player.name[1]
  }

  ggplot() +
    draw_pitch_sb() +
    geom_segment(
      data = data,
      aes(
        x = location.x,
        y = location.y,
        xend = pass.end_location.x,
        yend = pass.end_location.y
      ),
      lineend = "round",
      linewidth = 0.5,
      colour = pass_color,
      arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")
    ) +
    labs(
      title = title %||% player_chart_title(player_label, title_suffix),
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = "Arrows show completed passes ending inside the opposition box."
    ) +
    theme_sdc() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}
