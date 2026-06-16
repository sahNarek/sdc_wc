#' UC2: Horizontal bar chart from aggregated team stats
viz_team_shots_bar <- function(shots_goals_df,
                               title = "Shots by team",
                               subtitle = NULL,
                               fill_colors = NULL) {
  chart_df <- shots_goals_df
  if ("team.name" %in% names(chart_df) && !"Team" %in% names(chart_df)) {
    chart_df <- chart_df %>% rename(Team = team.name)
  }

  if (is.null(fill_colors)) {
    fill_colors <- setNames(SDC_PALETTE_VEC[seq_len(nrow(chart_df))], chart_df$Team)
  }

  ggplot(chart_df, aes(x = reorder(Team, shots), y = shots, fill = Team)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    scale_fill_manual(values = fill_colors) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Total shots"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_flip() +
    theme_sdc()
}
