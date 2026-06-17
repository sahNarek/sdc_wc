#' Default outcome colours for the goal-mouth panel (SDC palette)
default_goal_outcome_colors <- function(goal_color = SDC_PALETTE[["blue"]]) {
  list(
    Goal = goal_color,
    Saved = SDC_PALETTE[["green"]],
    "Off T" = "#333333",
    Post = SDC_PALETTE[["orange"]]
  )
}

#' Resolve fill colour per shot outcome on the goal panel
goal_panel_outcome_colors <- function(outcomes,
                                      outcome_colors = NULL,
                                      goal_color = NULL) {
  colors <- outcome_colors %||% default_goal_outcome_colors(goal_color = goal_color)
  if (!is.null(goal_color)) {
    colors$Goal <- goal_color
  }
  vapply(
    outcomes,
    function(outcome) {
      col <- colors[[outcome]]
      if (is.null(col)) "#333333" else col
    },
    character(1)
  )
}

#' Binary goal-mouth shapes: scored vs not scored
goal_scored_shapes <- function() {
  c(
    Goal = 21,
    `No Goal` = 22
  )
}

#' Per-outcome goal-mouth shapes (optional detail mode)
goal_outcome_shapes <- function() {
  c(
    Goal = 21,
    Saved = 22,
    "Off T" = 24,
    Post = 23
  )
}

#' Map shot outcomes to goal-panel shape groups
goal_panel_shape_values <- function(outcomes, shape_by = c("binary", "outcome")) {
  shape_by <- match.arg(shape_by)
  if (shape_by == "binary") {
    ifelse(outcomes == "Goal", "Goal", "No Goal")
  } else {
    outcomes
  }
}

#' Map StatsBomb shot end_location to front-on goal panel coordinates (metres)
#'
#' Horizontal (\code{net_x}): \code{shot.end_location.y} along the 8-yard mouth.
#' Vertical (\code{net_y}): \code{shot.end_location.z} in metres above ground.
map_shot_to_goal_panel <- function(end_y,
                                   end_z,
                                   goal_y_min = GOAL_POST_Y_MIN,
                                   goal_y_max = GOAL_POST_Y_MAX,
                                   goal_width_m = GOAL_WIDTH_M,
                                   goal_height_m = GOAL_HEIGHT_M,
                                   clip = TRUE) {
  goal_width_sb <- goal_y_max - goal_y_min
  net_x <- (end_y - goal_y_min) / goal_width_sb * goal_width_m
  net_y <- end_z

  if (!clip) {
    return(tibble::tibble(net_x = net_x, net_y = net_y))
  }

  tibble::tibble(
    net_x = pmin(pmax(net_x, 0), goal_width_m),
    net_y = pmin(pmax(net_y, 0), goal_height_m),
    net_x_raw = net_x,
    net_y_raw = net_y,
    clipped = (net_x < 0 | net_x > goal_width_m | net_y < 0 | net_y > goal_height_m)
  )
}

#' Shots with valid end locations; split net-panel vs pitch-only (blocked / no height)
filter_shots_for_goal_net <- function(data,
                                      net_outcomes = c("Goal", "Saved", "Off T"),
                                      net_min_x = GOAL_NET_MIN_X,
                                      goal_width_m = GOAL_WIDTH_M,
                                      goal_height_m = GOAL_HEIGHT_M) {
  if (!"shot.end_location.x" %in% names(data)) {
    stop(
      "Shot end locations are not available. Rebuild processed data with ",
      "shot.end_location parsing enabled.",
      call. = FALSE
    )
  }

  if (!"shot.end_location.z" %in% names(data)) {
    stop(
      "Shot end height (shot.end_location.z) is not available. Rebuild processed ",
      "data with shot.end_location z parsing enabled.",
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
      .data$`shot.end_location.x` >= net_min_x,
      !is.na(.data$`shot.end_location.z`)
    )

  if (nrow(on_net) > 0) {
    mapped <- map_shot_to_goal_panel(
      end_y = on_net$`shot.end_location.y`,
      end_z = on_net$`shot.end_location.z`,
      goal_width_m = goal_width_m,
      goal_height_m = goal_height_m,
      clip = TRUE
    )
    on_net <- dplyr::bind_cols(on_net, mapped) %>%
      dplyr::mutate(on_goal_panel = TRUE)
  } else {
    on_net <- on_net %>% dplyr::mutate(
      on_goal_panel = TRUE,
      net_x = double(),
      net_y = double(),
      net_x_raw = double(),
      net_y_raw = double(),
      clipped = logical()
    )
  }

  pitch_only <- data %>%
    dplyr::filter(
      !(.data$`shot.outcome.name` %in% net_outcomes &
          .data$`shot.end_location.x` >= net_min_x &
          !is.na(.data$`shot.end_location.z`))
    ) %>%
    dplyr::mutate(
      on_goal_panel = FALSE,
      net_x = NA_real_,
      net_y = NA_real_,
      net_x_raw = NA_real_,
      net_y_raw = NA_real_,
      clipped = NA
    )

  dplyr::bind_rows(on_net, pitch_only)
}

#' Summary counts for optional UC8 header strip (player- or team-filtered data)
compute_shot_summary_stats <- function(data, subject_label = NULL) {
  goals <- sum(data$`shot.outcome.name` == "Goal", na.rm = TRUE)
  outside_box <- sum(
    data$`shot.outcome.name` == "Goal" & data$`location.x` < 102,
    na.rm = TRUE
  )
  on_target <- sum(
    data$`shot.outcome.name` %in% c("Goal", "Saved"),
    na.rm = TRUE
  )

  name_prefix <- if (!is.null(subject_label) && nzchar(subject_label)) {
    paste0(toupper(subject_label), " — ")
  } else {
    ""
  }

  list(
    goals = goals,
    goals_outside_box = outside_box,
    on_target = on_target,
    total_shots = nrow(data),
    label = paste0(
      name_prefix,
      "GOALS: ", goals,
      "  |  GOALS OUTSIDE THE BOX: ", outside_box,
      "  |  ON TARGET: ", on_target,
      "  |  SHOTS: ", nrow(data)
    )
  )
}

#' Compute minute-label positions that clear the ball icon and goal frame
add_goal_net_minute_label_positions <- function(net_data,
                                              goal_width_m = GOAL_WIDTH_M,
                                              goal_height_m = GOAL_HEIGHT_M,
                                              icon_size = 0.22) {
  icon_r <- goal_height_m * icon_size * 0.42
  text_gap <- goal_height_m * 0.07
  frame_pad <- goal_height_m * 0.05
  side_pad <- goal_width_m * 0.07

  net_data %>%
    dplyr::mutate(
      .above_y = .data$net_y + .env$icon_r + .env$text_gap,
      .below_y = .data$net_y - .env$icon_r - .env$text_gap,
      label_y = dplyr::case_when(
        .data$.above_y > .env$goal_height_m - .env$frame_pad ~
          .env$goal_height_m + .env$text_gap * 1.35,
        .data$net_y < .env$icon_r + .env$frame_pad ~ .data$.above_y,
        TRUE ~ .data$.above_y
      ),
      label_vjust = 0,
      label_x = dplyr::case_when(
        .data$net_x < .env$side_pad ~ .data$net_x + .env$side_pad * 0.85,
        .data$net_x > .env$goal_width_m - .env$side_pad ~
          .data$net_x - .env$side_pad * 0.85,
        TRUE ~ .data$net_x
      ),
      label_hjust = dplyr::case_when(
        .data$net_x < .env$side_pad ~ 0,
        .data$net_x > .env$goal_width_m - .env$side_pad ~ 1,
        TRUE ~ 0.5
      )
    ) %>%
    dplyr::select(-dplyr::starts_with("."))
}

#' UC8: Combined shot map (pitch + trajectories) and goal-mouth panel
viz_shot_map_goal_net <- function(events_df,
                                  player_name = NULL,
                                  player_id = NULL,
                                  team_name = NULL,
                                  both_teams = FALSE,
                                  match_id = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  title_suffix = "shot map and goal mouth",
                                  exclude_penalties = TRUE,
                                  shot_color = SDC_PALETTE[["blue"]],
                                  team_colors = NULL,
                                  lightest_color = NULL,
                                  gradient_colors = NULL,
                                  goal_net_bg_svg = NULL,
                                  goal_width_m = GOAL_WIDTH_M,
                                  goal_height_m = GOAL_HEIGHT_M,
                                  goal_shape_by = c("binary", "outcome"),
                                  show_minute_labels = TRUE,
                                  show_trajectories = TRUE,
                                  show_summary = TRUE,
                                  net_outcomes = c("Goal", "Saved", "Off T"),
                                  xg_limits = c(0, 0.8),
                                  outcome_colors = NULL,
                                  goal_panel_width_frac = 0.54,
                                  goal_section_height = 0.43,
                                  goal_display_tallness = 2.35,
                                  marker_size = 4.5,
                                  icon_size = 0.095,
                                  icon_set = "footprint",
                                  goal_net_use_ball_icons = TRUE,
                                  goal_net_icon_size = 0.22,
                                  team_labels = NULL,
                                  display_home = NULL,
                                  display_away = NULL) {
  goal_shape_by <- match.arg(goal_shape_by)

  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    team_name = team_name,
    both_teams = both_teams,
    match_id = match_id,
    exclude_penalties = exclude_penalties
  )

  data <- filter_shots_for_goal_net(
    data,
    net_outcomes = net_outcomes,
    goal_width_m = goal_width_m,
    goal_height_m = goal_height_m
  )

  label <- resolve_shot_map_label(
    data,
    player_name = player_name,
    team_name = team_name,
    both_teams = both_teams,
    team_labels = team_labels,
    display_home = display_home,
    display_away = display_away
  )
  shot_colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 9,
    variant = "shot_map"
  )
  stats <- compute_shot_summary_stats(data, subject_label = label)
  resolved_outcome_colors <- outcome_colors %||% default_goal_outcome_colors(
    goal_color = shot_color
  )

  goal_shape_values <- if (goal_shape_by == "binary") {
    goal_scored_shapes()
  } else {
    goal_outcome_shapes()
  }

  net_data <- data %>%
    dplyr::filter(.data$on_goal_panel) %>%
    dplyr::mutate(
      goal_shape = goal_panel_shape_values(
        .data$`shot.outcome.name`,
        shape_by = goal_shape_by
      ),
      marker_fill = goal_panel_outcome_colors(
        .data$`shot.outcome.name`,
        outcome_colors = resolved_outcome_colors,
        goal_color = shot_color
      ),
      marker_alpha = dplyr::case_when(
        .data$`shot.outcome.name` == "Goal" ~ 1,
        .data$`shot.outcome.name` == "Saved" ~ 0.9,
        isTRUE(.data$clipped) ~ 0.55,
        TRUE ~ 0.8
      )
    )

  if (isTRUE(goal_net_use_ball_icons)) {
    ensure_ball_icon()
    net_data <- add_goal_net_ball_icons(net_data)
  }

  goal_layout <- goal_panel_layout(
    goal_width_m = goal_width_m,
    goal_height_m = goal_height_m,
    display_tallness = goal_display_tallness
  )

  goal_marker_size <- marker_size * 1.08

  goal_plot <- ggplot() +
    draw_goal_net(
      width_m = goal_width_m,
      height_m = goal_height_m,
      bg_svg = goal_net_bg_svg
    )

  if (isTRUE(goal_net_use_ball_icons)) {
    if (!requireNamespace("ggimage", quietly = TRUE)) {
      install.packages("ggimage", repos = "https://cloud.r-project.org")
    }

    goal_plot <- goal_plot +
      ggimage::geom_image(
        data = net_data,
        aes(
          x = .data$net_x,
          y = .data$net_y,
          image = .data$colored_icon
        ),
        size = goal_net_icon_size
      )
  } else {
    goal_plot <- goal_plot +
      geom_point(
        data = net_data,
        aes(
          x = .data$net_x,
          y = .data$net_y,
          shape = .data$goal_shape,
          alpha = .data$marker_alpha
        ),
        fill = net_data$marker_fill,
        size = goal_marker_size,
        colour = "#222222",
        stroke = 0.35
      ) +
      scale_shape_manual(values = goal_shape_values, guide = "none") +
      scale_alpha_identity()
  }

  goal_plot <- goal_plot +
    labs(subtitle = "Goal mouth (shot end position)") +
    coord_fixed(
      ratio = goal_layout$coord_ratio,
      xlim = goal_layout$xlim,
      ylim = goal_layout$ylim,
      clip = "off",
      expand = FALSE
    ) +
    theme_sdc() +
    theme(
      aspect.ratio = goal_layout$aspect_ratio,
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = rel(1.05)),
      plot.margin = margin(t = 10, r = 2, b = 2, l = 2),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )

  label_data <- if (isTRUE(show_minute_labels) && nrow(net_data) > 0) {
    net_data %>%
      dplyr::filter(!is.na(.data$minute)) %>%
      add_goal_net_minute_label_positions(
        goal_width_m = goal_width_m,
        goal_height_m = goal_height_m,
        icon_size = goal_net_icon_size
      )
  } else {
    net_data[0, , drop = FALSE]
  }

  if (nrow(label_data) > 0) {
    goal_plot <- goal_plot +
      geom_label(
        data = label_data,
        aes(
          x = .data$label_x,
          y = .data$label_y,
          label = paste0(.data$minute, "'"),
          colour = .data$marker_fill,
          vjust = .data$label_vjust,
          hjust = .data$label_hjust
        ),
        size = 3.1,
        fontface = "bold",
        family = SDC_FONTS$body,
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.15, "lines"),
        alpha = 0.92,
        show.legend = FALSE
      ) +
      scale_colour_identity()
  }

  pitch_data <- add_shot_trajectory_endpoints(data) %>%
    add_colored_shot_icons(
      shot_color = shot_color,
      lightest_color = lightest_color,
      gradient_colors = gradient_colors,
      team_colors = team_colors,
      limits = xg_limits,
      icon_set = icon_set
    )

  pitch_plot <- ggplot() +
    draw_pitch_half_attacking() +
    build_shot_map_pitch_icon_layers(
      data = pitch_data,
      shot_colors = shot_colors,
      shot_color = shot_color,
      team_colors = team_colors,
      icon_size = icon_size,
      xg_limits = xg_limits,
      show_trajectories = show_trajectories,
      icon_set = icon_set
    ) +
    labs(
      subtitle = "Shot origins and trajectories",
      x = NULL,
      y = NULL
    ) +
    theme_sdc() +
    theme(
      legend.position = "none",
      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = rel(1.05)),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  pitch_plot <- assemble_shot_map(
    pitch_plot,
    icon_set = icon_set,
    icon_color = shot_color,
    shot_colors = shot_colors,
    xg_limits = xg_limits,
    show_xg_legend = is.null(team_colors),
    show_goal_net_ball_legend = FALSE,
    show_trajectory_legend = isTRUE(show_trajectories)
  )

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  goal_legend <- if (isTRUE(goal_net_use_ball_icons)) {
    plot_goal_mouth_ball_legend()
  } else {
    NULL
  }

  goal_row <- wrap_goal_panel_block(
    goal_plot,
    legend_plot = goal_legend,
    width_frac = goal_panel_width_frac
  )

  pitch_height <- 1 - goal_section_height
  panels <- list(goal_row, pitch_plot)
  heights <- c(goal_section_height, pitch_height)

  if (show_summary) {
    summary_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = stats$label,
        size = 4.1,
        fontface = "bold",
        family = SDC_FONTS$body
      ) +
      theme_void() +
      coord_cartesian(clip = "off")

    panels <- c(list(summary_plot), panels)
    summary_height <- 0.07
    scale <- (1 - summary_height)
    heights <- c(summary_height, goal_section_height * scale, pitch_height * scale)
  }

  combined <- patchwork::wrap_plots(panels, ncol = 1, heights = heights) +
    patchwork::plot_annotation(
      title = title %||% player_chart_title(label, title_suffix),
      subtitle = subtitle,
      caption = if (isTRUE(goal_net_use_ball_icons)) {
        paste(
          "Pitch icons show body part; colour reflects xG.",
          "Penalties excluded."
        )
      } else {
        paste(
          "Goal panel: circle = goal, square = no goal.",
          "Pitch icons show body part; colour reflects xG (team hue when both sides shown).",
          "Penalties excluded."
        )
      },
      theme = theme_sdc()
    )

  combined
}
