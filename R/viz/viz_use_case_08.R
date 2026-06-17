#' Map StatsBomb shot end_location to front-on goal panel coordinates
#'
#' Horizontal (net_x): shot_end_location_y along the 8-yard goal mouth.
#' Vertical (net_y): centred at crossbar mid-height; StatsBomb has no ball height.
map_shot_to_goal_panel <- function(end_x,
                                   end_y,
                                   goal_y_min = GOAL_POST_Y_MIN,
                                   goal_y_max = GOAL_POST_Y_MAX) {
  goal_width <- goal_y_max - goal_y_min
  tibble::tibble(
    net_x = (end_y - goal_y_min) / goal_width * 8,
    net_y = rep(2.67 / 2, length(end_y))
  )
}

#' Shots with valid end locations; split net-panel vs pitch-only (blocked)
filter_shots_for_goal_net <- function(data,
                                      net_outcomes = c("Goal", "Saved", "Off T"),
                                      net_min_x = GOAL_NET_MIN_X) {
  if (!"shot.end_location.x" %in% names(data)) {
    stop(
      "Shot end locations are not available. Rebuild processed data with ",
      "shot.end_location parsing enabled.",
      call. = FALSE
    )
  }

  data <- data %>%
    dplyr::filter(
      !is.na(.data$`shot.end_location.x`),
      !is.na(.data$`shot.end_location.y`)
    )

  if (nrow(data) == 0) {
    stop("No shots with end-location data found.", call. = FALSE)
  }

  on_net <- data %>%
    dplyr::filter(
      .data$`shot.outcome.name` %in% net_outcomes,
      .data$`shot.end_location.x` >= net_min_x
    ) %>%
    dplyr::mutate(
      on_goal_panel = TRUE,
      net_x = (.data$`shot.end_location.y` - GOAL_POST_Y_MIN) /
        (GOAL_POST_Y_MAX - GOAL_POST_Y_MIN) * 8,
      net_y = 2.67 / 2
    )

  pitch_only <- data %>%
    dplyr::filter(
      !(.data$`shot.outcome.name` %in% net_outcomes &
          .data$`shot.end_location.x` >= net_min_x)
    ) %>%
    dplyr::mutate(on_goal_panel = FALSE, net_x = NA_real_, net_y = NA_real_)

  dplyr::bind_rows(on_net, pitch_only)
}

#' Summary counts for optional UC8 header strip
compute_shot_summary_stats <- function(data) {
  goals <- sum(data$`shot.outcome.name` == "Goal", na.rm = TRUE)
  outside_box <- sum(
    data$`shot.outcome.name` == "Goal" & data$`location.x` < 102,
    na.rm = TRUE
  )
  on_target <- sum(
    data$`shot.outcome.name` %in% c("Goal", "Saved"),
    na.rm = TRUE
  )

  list(
    goals = goals,
    goals_outside_box = outside_box,
    on_target = on_target,
    total_shots = nrow(data),
    label = paste0(
      "Goals: ", goals,
      "  |  Goals outside the box: ", outside_box,
      "  |  On target: ", on_target,
      "  |  Shots: ", nrow(data)
    )
  )
}

shot_body_part_shapes <- function() {
  c(
    "Head" = 21,
    "Right Foot" = 23,
    "Left Foot" = 24,
    "Other" = 22
  )
}

#' UC8: Combined shot map (pitch + trajectories) and goal-mouth panel
viz_shot_map_goal_net <- function(events_df,
                                  player_name = NULL,
                                  player_id = NULL,
                                  match_id = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  title_suffix = "shot map and goal mouth",
                                  exclude_penalties = TRUE,
                                  shot_color = SDC_PALETTE[["blue"]],
                                  lightest_color = NULL,
                                  gradient_colors = NULL,
                                  show_xg_labels = TRUE,
                                  show_trajectories = TRUE,
                                  show_summary = TRUE,
                                  net_outcomes = c("Goal", "Saved", "Off T"),
                                  xg_limits = c(0, 0.8),
                                  marker_size = 4.5) {
  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    match_id = match_id,
    exclude_penalties = exclude_penalties
  )

  data <- filter_shots_for_goal_net(data, net_outcomes = net_outcomes)

  label <- dplyr::coalesce(data$player_display_name[1], data$player.name[1])
  shot_colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 9
  )
  stats <- compute_shot_summary_stats(data)
  net_data <- data %>%
    dplyr::filter(.data$on_goal_panel) %>%
    dplyr::mutate(
      marker_alpha = ifelse(.data$`shot.outcome.name` == "Goal", 1, 0.75)
    )

  goal_plot <- ggplot() +
    draw_goal_net() +
    geom_point(
      data = net_data,
      aes(
        x = .data$net_x,
        y = .data$net_y,
        fill = .data$`shot.statsbomb_xg`,
        shape = .data$`shot.body_part.name`,
        alpha = .data$marker_alpha
      ),
      size = marker_size,
      colour = "#333333"
    ) +
    scale_fill_gradientn(
      colours = shot_colors,
      limits = xg_limits,
      oob = scales::squish,
      guide = "none"
    ) +
    scale_shape_manual(values = shot_body_part_shapes(), guide = "none") +
    scale_alpha_identity() +
    labs(subtitle = "Goal mouth (shot end position)") +
    coord_fixed(ratio = 8 / 2.67, xlim = c(-0.8, 8.8), ylim = c(-0.2, 3)) +
    theme_sdc() +
    theme(
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )

  if (show_xg_labels && nrow(net_data) > 0) {
    goal_plot <- goal_plot +
      geom_text(
        data = net_data,
        aes(
          x = .data$net_x,
          y = .data$net_y + 0.35,
          label = sprintf("xG %.2f", .data$`shot.statsbomb_xg`)
        ),
        size = 2.8,
        colour = "#333333",
        fontface = "bold"
      )
  }

  pitch_plot <- ggplot() +
    draw_pitch_half_attacking()

  if (show_trajectories) {
    pitch_plot <- pitch_plot +
      geom_segment(
        data = data,
        aes(
          x = .data$`location.x`,
          y = .data$`location.y`,
          xend = .data$`shot.end_location.x`,
          yend = .data$`shot.end_location.y`
        ),
        linetype = "dashed",
        colour = "#666666",
        linewidth = 0.35,
        alpha = 0.65
      )
  }

  pitch_plot <- pitch_plot +
    geom_point(
      data = data,
      aes(
        x = .data$`location.x`,
        y = .data$`location.y`,
        fill = .data$`shot.statsbomb_xg`,
        shape = .data$`shot.body_part.name`
      ),
      size = marker_size,
      alpha = 0.9,
      colour = "#333333"
    ) +
    scale_fill_gradientn(
      colours = shot_colors,
      limits = xg_limits,
      oob = scales::squish,
      name = "Expected goals\n(xG)"
    ) +
    scale_shape_manual(
      values = shot_body_part_shapes(),
      name = "Body part"
    ) +
    guides(
      fill = guide_colourbar(title.position = "top"),
      shape = guide_legend(override.aes = list(size = marker_size, fill = "#333333"))
    ) +
    labs(
      subtitle = "Shot origins and trajectories",
      x = NULL,
      y = NULL
    ) +
    theme_sdc() +
    theme(
      legend.position = "top",
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  panels <- list(goal_plot, pitch_plot)
  heights <- c(0.38, 0.62)

  if (show_summary) {
    summary_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = stats$label,
        size = 3.8,
        fontface = "bold"
      ) +
      theme_void() +
      coord_cartesian(clip = "off")

    panels <- c(list(summary_plot), panels)
    heights <- c(0.08, heights)
  }

  combined <- patchwork::wrap_plots(panels, ncol = 1, heights = heights) +
    patchwork::plot_annotation(
      title = title %||% player_chart_title(label, title_suffix),
      subtitle = subtitle,
      caption = paste(
        "Dashed lines show shot path. Goal panel shows ball position at shot end.",
        "Blocked shots appear on the pitch only. Penalties excluded."
      ),
      theme = theme_sdc()
    )

  combined
}
