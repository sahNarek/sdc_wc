#' UC6: Defensive action heatmap (single-hue palette gradient)
compute_defensive_heatmap <- function(events_df,
                                      match_id = NULL,
                                      bin_width = 20) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  x_breaks <- seq(0, 120, by = bin_width)
  y_breaks <- seq(0, 80, by = bin_width)

  data %>%
    filter(
      type.name %in% c("Pressure", "Foul Committed", "Interception", "Block") |
        duel.type.name == "Tackle"
    ) %>%
    mutate(
      pitch_x = pmin(pmax(location.x, 0), 120),
      pitch_y = pmin(pmax(location.y, 0), 80),
      x_bin = as.integer(cut(
        pitch_x,
        breaks = x_breaks,
        include.lowest = TRUE,
        labels = FALSE
      )),
      y_bin = as.integer(cut(
        pitch_y,
        breaks = y_breaks,
        include.lowest = TRUE,
        labels = FALSE
      ))
    ) %>%
    filter(!is.na(x_bin), !is.na(y_bin)) %>%
    group_by(team.name) %>%
    mutate(total_actions = dplyr::n()) %>%
    group_by(team.name, x_bin, y_bin) %>%
    summarise(
      total_actions = max(total_actions),
      zone_actions = dplyr::n(),
      share_of_actions = zone_actions / total_actions,
      .groups = "drop"
    ) %>%
    mutate(
      xmin = (x_bin - 1) * bin_width,
      xmax = x_bin * bin_width,
      ymin = (y_bin - 1) * bin_width,
      ymax = y_bin * bin_width
    ) %>%
    rename(Team = team.name)
}

viz_defensive_heatmap <- function(events_df,
                                  match_id = NULL,
                                  team_name = NULL,
                                  bin_width = 20,
                                  heat_color = SDC_PALETTE[["blue"]],
                                  lightest_color = NULL,
                                  gradient_colors = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  team_labels = NULL) {
  heatmap_df <- compute_defensive_heatmap(
    events_df,
    match_id = match_id,
    bin_width = bin_width
  ) %>%
    apply_team_display_labels(name_map = team_labels)

  if (!is.null(team_name)) {
    team_filter <- team_labels[[team_name]] %||% team_name
    heatmap_df <- heatmap_df %>% filter(Team == team_filter)
  }

  if (nrow(heatmap_df) == 0) {
    stop("No defensive events found for the selected match.", call. = FALSE)
  }

  heat_colors <- resolve_single_hue_gradient(
    color = heat_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 9
  )

  ggplot(heatmap_df) +
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = share_of_actions
      ),
      colour = NA,
      alpha = 0.92
    ) +
    draw_pitch_markings(colour = "white", linewidth = 0.5) +
    scale_fill_gradientn(
      colours = heat_colors,
      labels = scales::percent_format(accuracy = 1),
      name = "Share of defensive\nactions",
      limits = c(0, NA),
      oob = scales::squish
    ) +
    scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 105 / 100) +
    labs(
      title = title %||% "Where teams defend",
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = "Includes pressures, tackles, interceptions, blocks and fouls committed."
    ) +
    facet_wrap(~Team) +
    theme_sdc() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.spacing = unit(1.2, "lines"),
      legend.position = "bottom"
    )
}
