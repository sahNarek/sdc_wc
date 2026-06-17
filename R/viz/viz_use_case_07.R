#' Body-part point shapes shared by UC7 and UC8 pitch panels
shot_body_part_shapes <- function() {
  c(
    "Head" = 21,
    "Right Foot" = 23,
    "Left Foot" = 24,
    "Other" = 22
  )
}

#' Resolve display label for shot-map scope (player, team, or full match)
resolve_shot_map_label <- function(data,
                                   player_name = NULL,
                                   team_name = NULL,
                                   both_teams = FALSE,
                                   team_labels = NULL,
                                   display_home = NULL,
                                   display_away = NULL) {
  if (isTRUE(both_teams)) {
    if (!is.null(display_home) && !is.null(display_away)) {
      return(paste(display_home, "vs", display_away))
    }
    teams <- unique(data$`team.name`)
    if (length(teams) >= 2) {
      return(paste(teams[[1]], "vs", teams[[2]]))
    }
    return(teams[[1]] %||% "Match")
  }

  if (!is.null(team_name)) {
    if (!is.null(team_labels) && team_name %in% names(team_labels)) {
      return(team_labels[[team_name]])
    }
    return(team_name)
  }

  dplyr::coalesce(
    data$player_display_name[1],
    data$`player.name`[1],
    player_name
  )
}

#' UC7: Shot map — standard ggplot body-part markers coloured by xG
viz_shot_map <- function(events_df,
                         player_name = NULL,
                         player_id = NULL,
                         team_name = NULL,
                         both_teams = FALSE,
                         match_id = NULL,
                         title = NULL,
                         subtitle = NULL,
                         title_suffix = "shot map",
                         exclude_penalties = TRUE,
                         shot_color = SDC_PALETTE[["blue"]],
                         team_colors = NULL,
                         lightest_color = NULL,
                         gradient_colors = NULL,
                         xg_limits = c(0, 0.8),
                         marker_size = 5,
                         use_body_part_icons = TRUE,
                         icon_size = 0.095,
                         icon_set = "footprint",
                         team_labels = NULL,
                         display_home = NULL,
                         display_away = NULL) {
  if (isTRUE(use_body_part_icons)) {
    return(viz_shot_map_icons(
      events_df = events_df,
      player_name = player_name,
      player_id = player_id,
      team_name = team_name,
      both_teams = both_teams,
      match_id = match_id,
      title = title,
      subtitle = subtitle,
      title_suffix = title_suffix,
      exclude_penalties = exclude_penalties,
      shot_color = shot_color,
      team_colors = team_colors,
      lightest_color = lightest_color,
      gradient_colors = gradient_colors,
      icon_size = icon_size,
      xg_limits = xg_limits,
      icon_set = icon_set,
      team_labels = team_labels,
      display_home = display_home,
      display_away = display_away
    ))
  }

  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    team_name = team_name,
    both_teams = both_teams,
    match_id = match_id,
    exclude_penalties = exclude_penalties
  )

  label <- resolve_shot_map_label(
    data,
    player_name = player_name,
    team_name = team_name,
    both_teams = both_teams
  )
  shot_colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 9,
    variant = "shot_map"
  )

  ggplot() +
    draw_pitch_half_attacking() +
    build_shot_map_pitch_layers(
      data = data,
      shot_colors = shot_colors,
      marker_size = marker_size,
      xg_limits = xg_limits,
      show_trajectories = FALSE
    ) +
    labs(
      title = title %||% player_chart_title(label, title_suffix),
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = "Marker size is fixed; colour intensity reflects shot quality (xG). Penalties excluded."
    ) +
    theme_sdc() +
    theme(
      legend.position = "top",
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

#' UC7b: Shot map — custom body-part icons (footprint SVGs) coloured by xG
viz_shot_map_icons <- function(events_df,
                               player_name = NULL,
                               player_id = NULL,
                               team_name = NULL,
                               both_teams = FALSE,
                               match_id = NULL,
                               title = NULL,
                               subtitle = NULL,
                               title_suffix = "shot map",
                               exclude_penalties = TRUE,
                               shot_color = SDC_PALETTE[["blue"]],
                               team_colors = NULL,
                               lightest_color = NULL,
                               gradient_colors = NULL,
                               icon_size = 0.095,
                               xg_limits = c(0, 0.8),
                               icon_set = "footprint",
                               team_labels = NULL,
                               display_home = NULL,
                               display_away = NULL) {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  ensure_shot_icons(icon_set = icon_set)

  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    team_name = team_name,
    both_teams = both_teams,
    match_id = match_id,
    exclude_penalties = exclude_penalties
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
    n = 11,
    variant = "shot_map"
  )

  data <- data %>%
    add_colored_shot_icons(
      shot_color = shot_color,
      lightest_color = lightest_color,
      gradient_colors = gradient_colors,
      team_colors = team_colors,
      limits = xg_limits,
      icon_set = icon_set
    )

  main_plot <- ggplot() +
    draw_pitch_half_attacking() +
    build_shot_map_pitch_icon_layers(
      data = data,
      shot_colors = shot_colors,
      shot_color = shot_color,
      team_colors = team_colors,
      icon_size = icon_size,
      xg_limits = xg_limits,
      show_trajectories = FALSE,
      icon_set = icon_set
    ) +
    labs(
      title = title %||% player_chart_title(label, title_suffix),
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = if (is.null(team_colors)) {
        "Icon colour shows shot quality (xG). Icon shape shows the body part used. Penalties excluded."
      } else {
        "Icon colour shows team hue by xG. Icon shape shows the body part used. Penalties excluded."
      }
    ) +
    theme_sdc() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  assemble_shot_map(
    main_plot,
    icon_set = icon_set,
    icon_color = shot_color,
    shot_colors = shot_colors,
    xg_limits = xg_limits,
    show_xg_legend = is.null(team_colors)
  )
}

#' Outcome colours for dashed shot trajectories on the pitch
shot_trajectory_outcome_colors <- function() {
  c(
    Goal = SDC_PALETTE[["green"]],
    Saved = SDC_PALETTE[["orange"]],
    Other = SDC_PALETTE[["red"]]
  )
}

#' Map a shot outcome to a trajectory line colour
shot_trajectory_line_color <- function(outcome) {
  if (identical(outcome, "Goal")) {
    return(SDC_PALETTE[["green"]])
  }
  if (identical(outcome, "Saved")) {
    return(SDC_PALETTE[["orange"]])
  }
  SDC_PALETTE[["red"]]
}

#' Add per-shot trajectory line colours
add_shot_trajectory_colors <- function(data) {
  data %>%
    dplyr::mutate(
      traj_colour = purrr::map_chr(
        .data$`shot.outcome.name`,
        shot_trajectory_line_color
      )
    )
}

#' Dashed trajectory segments coloured by shot outcome
shot_trajectory_layers <- function(data) {
  traj_data <- add_shot_trajectory_colors(data)

  list(
    geom_segment(
      data = traj_data,
      aes(
        x = .data$`location.x`,
        y = .data$`location.y`,
        xend = .data$traj_xend,
        yend = .data$traj_yend
      ),
      colour = traj_data$traj_colour,
      linetype = "dashed",
      linewidth = 0.45,
      alpha = 0.85
    )
  )
}

#' Body-part icon layers for pitch shot maps (optionally with trajectories)
build_shot_map_pitch_icon_layers <- function(data,
                                             shot_colors,
                                             shot_color = SDC_PALETTE[["blue"]],
                                             team_colors = NULL,
                                             icon_size = 0.095,
                                             xg_limits = c(0, 0.8),
                                             show_trajectories = FALSE,
                                             icon_set = "footprint") {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  ensure_shot_icons(icon_set = icon_set)

  layers <- list()

  if (show_trajectories) {
    layers <- c(layers, shot_trajectory_layers(data))
  }

  layers <- c(layers, list(
    ggimage::geom_image(
      data = data,
      aes(
        x = .data$`location.x`,
        y = .data$`location.y`,
        image = .data$colored_icon
      ),
      size = icon_size
    )
  ))

  layers
}

#' ggplot layers for UC7-style pitch shot map (optionally with trajectories)
build_shot_map_pitch_layers <- function(data,
                                        shot_colors,
                                        marker_size = 5,
                                        xg_limits = c(0, 0.8),
                                        show_trajectories = FALSE) {
  layers <- list()

  if (show_trajectories) {
    layers <- c(layers, shot_trajectory_layers(data))
  }

  c(
    layers,
    list(
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
      ),
      scale_fill_gradientn(
        colours = shot_colors,
        limits = xg_limits,
        oob = scales::squish,
        name = "Expected goals\n(xG)"
      ),
      scale_shape_manual(
        values = shot_body_part_shapes(),
        name = "Body part"
      ),
      guides(
        fill = guide_colourbar(title.position = "top"),
        shape = guide_legend(
          override.aes = list(size = marker_size, fill = "#333333")
        )
      )
    )
  )
}

#' Add trajectory endpoints for UC8 pitch panel
#'
#' On-goal shots extend to the goal line at \code{shot.end_location.y}; others
#' use the recorded end coordinate. Requires attacking-half pitch coords from
#' \code{draw_pitch_half_attacking()} (no y-axis reverse).
add_shot_trajectory_endpoints <- function(data) {
  data %>%
    dplyr::mutate(
      traj_xend = dplyr::if_else(
        .data$on_goal_panel,
        GOAL_LINE_X,
        .data$`shot.end_location.x`
      ),
      traj_yend = .data$`shot.end_location.y`
    )
}

#' Shared shot filtering for UC7 variants
filter_shot_map_data <- function(events_df,
                                 player_name = NULL,
                                 player_id = NULL,
                                 team_name = NULL,
                                 both_teams = FALSE,
                                 match_id = NULL,
                                 exclude_penalties = TRUE) {
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
    player_id <- as.numeric(player_id)
    data <- data %>%
      dplyr::filter(.data$`player.id` == !!player_id)
  } else if (!is.null(player_name)) {
    data <- data %>%
      dplyr::filter(
        .data$`player.name` == !!player_name |
          .data$player_display_name == !!player_name
      )
  } else if (!is.null(team_name)) {
    data <- data %>%
      dplyr::filter(.data$`team.name` == !!team_name)
  } else if (!isTRUE(both_teams)) {
    stop(
      "Specify player_id, player_name, team_name, or both_teams = TRUE.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    stop("No shots found for the selected filter.", call. = FALSE)
  }

  data <- data %>%
    filter(location.x >= 85, location.x <= 120)

  if (nrow(data) == 0) {
    stop("No shots found in the attacking third for the selected filter.", call. = FALSE)
  }

  data
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
    dplyr::count(player.name, player.id, sort = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      player_label = player_display_label(events_df, player_id = player.id)
    )
}

#' Pick player with the most left-foot shots (for icon shot-map examples)
top_left_foot_shooter <- function(events_df, match_id = NULL, team_name = NULL) {
  data <- events_df %>%
    filter(
      type.name == "Shot",
      shot.body_part.name == "Left Foot",
      shot.type.name != "Penalty" | is.na(shot.type.name),
      location.x >= 85,
      location.x <= 120
    )

  if (!is.null(match_id)) {
    data <- data %>% filter(match_id == !!match_id)
  }

  if (!is.null(team_name)) {
    data <- data %>% filter(team.name == team_name)
  }

  if (nrow(data) == 0) {
    stop("No left-foot shots found in the attacking third.", call. = FALSE)
  }

  data %>%
    dplyr::count(player.name, player.id, sort = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      player_label = player_display_label(events_df, player_id = player.id)
    )
}
