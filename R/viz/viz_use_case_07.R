#' UC7: Shot map — standard ggplot body-part markers coloured by xG
viz_shot_map <- function(events_df,
                         player_name = NULL,
                         player_id = NULL,
                         match_id = NULL,
                         title = NULL,
                         subtitle = NULL,
                         title_suffix = "shot map",
                         exclude_penalties = TRUE,
                         shot_color = SDC_PALETTE[["blue"]],
                         lightest_color = NULL,
                         gradient_colors = NULL,
                         xg_limits = c(0, 0.8),
                         marker_size = 5) {
  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    match_id = match_id,
    exclude_penalties = exclude_penalties
  )

  label <- coalesce(data$player_display_name[1], data$player.name[1])
  shot_colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 9
  )

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
      shape = guide_legend(override.aes = list(size = marker_size, fill = "#333333"))
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
                               match_id = NULL,
                               title = NULL,
                               subtitle = NULL,
                               title_suffix = "shot map",
                               exclude_penalties = TRUE,
                               shot_color = SDC_PALETTE[["blue"]],
                               lightest_color = NULL,
                               gradient_colors = NULL,
                               icon_size = 0.058,
                               xg_limits = c(0, 0.8),
                               icon_set = "footprint") {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  ensure_shot_icons(icon_set = icon_set)

  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    match_id = match_id,
    exclude_penalties = exclude_penalties
  )

  label <- coalesce(data$player_display_name[1], data$player.name[1])
  shot_colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 11
  )

  data <- data %>%
    add_colored_shot_icons(
      shot_color = shot_color,
      lightest_color = lightest_color,
      gradient_colors = gradient_colors,
      limits = xg_limits,
      icon_set = icon_set
    )

  main_plot <- ggplot() +
    draw_pitch_half_attacking() +
    ggimage::geom_image(
      data = data,
      aes(x = location.x, y = location.y, image = colored_icon),
      size = icon_size
    ) +
    geom_point(
      data = data,
      aes(
        x = location.x,
        y = location.y,
        fill = shot.statsbomb_xg
      ),
      shape = 21,
      size = 0.1,
      alpha = 0,
      stroke = 0
    ) +
    scale_fill_gradientn(
      colours = shot_colors,
      limits = xg_limits,
      oob = scales::squish,
      name = "Expected goals\n(xG)"
    ) +
    guides(
      fill = guide_colourbar(title.position = "top")
    ) +
    labs(
      title = title %||% player_chart_title(label, title_suffix),
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = "Icon colour shows shot quality (xG). Icon shape shows the body part used. Penalties excluded."
    ) +
    theme_sdc() +
    theme(
      legend.position = "top",
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  assemble_shot_map(main_plot, icon_set = icon_set, icon_color = shot_color)
}

#' Shared shot filtering for UC7 variants
filter_shot_map_data <- function(events_df,
                                 player_name = NULL,
                                 player_id = NULL,
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
    data <- data %>% filter(player.id == player_id)
  }

  if (!is.null(player_name)) {
    data <- data %>%
      filter(player.name == player_name | player_display_name == player_name)
  }

  if (nrow(data) == 0) {
    stop("No shots found for the selected player.", call. = FALSE)
  }

  data <- data %>%
    filter(location.x >= 85, location.x <= 120)

  if (nrow(data) == 0) {
    stop("No shots found in the attacking third for the selected player.", call. = FALSE)
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
