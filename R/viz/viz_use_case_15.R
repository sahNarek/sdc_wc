#' Non-penalty shot rows for momentum / OBV timelines
filter_np_shot_events <- function(events_df, match_id = NULL) {
  data <- ensure_viz_aliases(events_df)
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }
  data %>%
    dplyr::filter(
      .data$`type.name` == "Shot",
      is.na(.data$`shot.type.name`) | .data$`shot.type.name` != "Penalty",
      !is.na(.data$minute)
    ) %>%
    dplyr::mutate(
      match_minute = .data$minute + dplyr::coalesce(.data$second, 0L) / 60,
      xg = dplyr::coalesce(.data$`shot.statsbomb_xg`, 0)
    )
}

#' Match end minute from events (includes stoppage time)
match_end_minute <- function(events_df, match_id = NULL) {
  data <- ensure_viz_aliases(events_df)
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }
  max(data$minute + dplyr::coalesce(data$second, 0L) / 60, na.rm = TRUE) %||% 96
}

#' Cumulative NP xG step data for both teams
compute_xg_momentum_timeline <- function(events_df,
                                         meta,
                                         match_id = NULL,
                                         home_color = SDC_PALETTE[["green"]],
                                         away_color = SDC_PALETTE[["red"]]) {
  shots <- filter_np_shot_events(events_df, match_id = match_id)
  teams <- c(meta$home_team[[1]], meta$away_team[[1]])
  max_minute <- max(c(shots$match_minute, match_end_minute(events_df, match_id)), na.rm = TRUE)

  dplyr::bind_rows(lapply(teams, function(team) {
    team_shots <- shots %>%
      dplyr::filter(.data$`team.name` == team) %>%
      dplyr::arrange(.data$match_minute, .data$index)

    if (nrow(team_shots) == 0) {
      return(tibble::tibble(
        team_name = team,
        match_minute = c(0, max_minute),
        cumulative_xg = c(0, 0),
        is_goal = c(FALSE, FALSE)
      ))
    }

    dplyr::bind_rows(
      tibble::tibble(
        team_name = team,
        match_minute = 0,
        cumulative_xg = 0,
        is_goal = FALSE
      ),
      team_shots %>%
        dplyr::mutate(
          cumulative_xg = cumsum(.data$xg),
          is_goal = .data$`shot.outcome.name` == "Goal"
        ) %>%
        dplyr::select(
          "team_name",
          "match_minute",
          "cumulative_xg",
          "is_goal"
        ),
      tibble::tibble(
        team_name = team,
        match_minute = max_minute,
        cumulative_xg = sum(team_shots$xg, na.rm = TRUE),
        is_goal = FALSE
      )
    ) %>%
      dplyr::distinct(.data$team_name, .data$match_minute, .keep_all = TRUE) %>%
      dplyr::arrange(.data$match_minute)
  })) %>%
    dplyr::mutate(
      display_team = dplyr::case_when(
        .data$team_name == meta$home_team[[1]] ~ meta$display_home[[1]],
        .data$team_name == meta$away_team[[1]] ~ meta$display_away[[1]],
        TRUE ~ .data$team_name
      ),
      team_color = dplyr::if_else(
        .data$team_name == meta$home_team[[1]],
        home_color,
        away_color
      )
    )
}

#' Goal events for xG timeline markers
compute_xg_timeline_goal_markers <- function(events_df,
                                             meta,
                                             match_id = NULL,
                                             home_color = SDC_PALETTE[["green"]],
                                             away_color = SDC_PALETTE[["red"]],
                                             root = get_project_root()) {
  shots <- filter_np_shot_events(events_df, match_id = match_id) %>%
    dplyr::filter(.data$`shot.outcome.name` == "Goal") %>%
    dplyr::transmute(
      minute = .data$minute,
      match_minute = .data$match_minute,
      team_name = .data$`team.name`,
      team_color = dplyr::if_else(
        .data$team_name == meta$home_team[[1]],
        home_color,
        away_color
      ),
      minute_label = paste0(.data$minute, "'")
    )

  if (nrow(shots) == 0) {
    return(shots %>% dplyr::mutate(x = numeric(), y = numeric(), icon = character()))
  }

  timeline <- compute_xg_momentum_timeline(
    events_df,
    meta,
    match_id = match_id,
    home_color = home_color,
    away_color = away_color
  )

  shots %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      y = {
        team_line <- timeline %>%
          dplyr::filter(.data$team_name == .env$team_name) %>%
          dplyr::filter(.data$match_minute <= .env$match_minute) %>%
          dplyr::slice_tail(n = 1)
        team_line$cumulative_xg[[1]] %||% 0
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      x = .data$match_minute,
      icon = purrr::map_chr(.data$team_color, colored_ball_icon_path, root = root)
    )
}

#' Panel A — cumulative NP xG momentum timeline
viz_xg_momentum_timeline <- function(events_df,
                                     meta,
                                     match_id = NULL,
                                     home_color = SDC_PALETTE[["green"]],
                                     away_color = SDC_PALETTE[["red"]],
                                     root = get_project_root(),
                                     title = "When the chances arrived",
                                     subtitle = "Cumulative non-penalty expected goals by minute") {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  timeline_df <- compute_xg_momentum_timeline(
    events_df,
    meta,
    match_id = match_id,
    home_color = home_color,
    away_color = away_color
  )
  goal_markers <- compute_xg_timeline_goal_markers(
    events_df,
    meta,
    match_id = match_id,
    home_color = home_color,
    away_color = away_color,
    root = root
  )

  color_map <- stats::setNames(
    c(home_color, away_color),
    c(meta$home_team[[1]], meta$away_team[[1]])
  )

  finals <- timeline_df %>%
    dplyr::group_by(.data$team_name) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  max_minute <- max(timeline_df$match_minute, na.rm = TRUE)
  x_breaks <- unique(c(seq(0, 90, by = 15), if (max_minute > 90) round(max_minute)))
  x_plot_max <- max(max_minute * 1.14, max(x_breaks, na.rm = TRUE) * 1.02)

  p <- ggplot2::ggplot(
    timeline_df,
    ggplot2::aes(
      x = .data$match_minute,
      y = .data$cumulative_xg,
      colour = .data$team_name
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "#CCCCCC", linewidth = 0.35) +
    ggplot2::geom_step(linewidth = 1.1, direction = "hv") +
    ggplot2::scale_colour_manual(
      values = color_map,
      labels = c(meta$display_home[[1]], meta$display_away[[1]]),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, x_plot_max),
      breaks = x_breaks,
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(1.4, max(timeline_df$cumulative_xg, na.rm = TRUE) * 1.15)),
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Minute",
      y = "Cumulative NP xG"
    ) +
    theme_sdc(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 8.5, hjust = 0, colour = "#555555"),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.85), colour = NA),
      legend.text = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 7.5),
      plot.margin = ggplot2::margin(4, 28, 6, 4)
    ) +
    ggplot2::coord_cartesian(clip = "off")

  if (nrow(finals) > 0) {
    finals <- finals %>%
      dplyr::arrange(.data$cumulative_xg) %>%
      dplyr::mutate(
        label_x = .data$match_minute + max(1.5, max_minute * 0.015),
        label_y = .data$cumulative_xg + (dplyr::row_number() - 1.5) * 0.04,
        label_text = paste0(
          format(round(.data$cumulative_xg, 2), nsmall = 2),
          " xG"
        )
      )
    p <- p +
      ggplot2::geom_text(
        data = finals,
        mapping = ggplot2::aes(
          x = .data$label_x,
          y = .data$label_y,
          label = .data$label_text
        ),
        colour = I(finals$team_color),
        hjust = 0,
        vjust = 0.35,
        family = SDC_FONTS$body,
        size = 2.5,
        fontface = "bold",
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  if (nrow(goal_markers) > 0) {
    p <- p +
      ggimage::geom_image(
        data = goal_markers,
        ggplot2::aes(x = .data$x, y = .data$y, image = .data$icon),
        inherit.aes = FALSE,
        size = 0.035
      ) +
      ggplot2::geom_text(
        data = goal_markers,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$minute_label),
        inherit.aes = FALSE,
        vjust = -0.8,
        family = SDC_FONTS$body,
        size = 2.2,
        fontface = "bold",
        colour = I(goal_markers$team_color),
        show.legend = FALSE
      )
  }

  p
}

#' Player skill columns available in StatsBomb player_match_stats (when JSON exists)
PLAYER_SKILL_PMS_COLUMNS <- c(
  "player_match_np_xg",
  "player_match_key_passes",
  "player_match_dribbles",
  "player_match_touches",
  "player_match_pressures",
  "player_match_op_xgchain",
  "player_match_deep_progressions",
  "player_match_interceptions"
)

#' Sum event-level OBV by player from raw match JSON (fallback when cache lacks OBV)
compute_player_obv_from_raw_events <- function(match_id, root = get_project_root()) {
  path <- resolve_match_file(match_id, "events.json", root = root)
  if (is.null(path)) {
    return(tibble::tibble(
      player_id = integer(),
      player_name = character(),
      team_name = character(),
      obv = numeric()
    ))
  }

  events <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rows <- lapply(events, function(event) {
    player_id <- if (!is.null(event$player) && !is.null(event$player$id)) {
      as.integer(event$player$id)
    } else {
      NA_integer_
    }
    if (is.na(player_id)) {
      return(NULL)
    }
    tibble::tibble(
      player_id = player_id,
      player_name = as.character(event$player$name),
      team_name = as.character(event$team$name),
      obv = as.numeric(event$obv_total_net %||% 0)
    )
  })

  if (length(rows) == 0) {
    return(tibble::tibble(
      player_id = integer(),
      player_name = character(),
      team_name = character(),
      obv = numeric()
    ))
  }

  dplyr::bind_rows(rows) %>%
    dplyr::group_by(.data$player_id, .data$player_name, .data$team_name) %>%
    dplyr::summarise(obv = sum(.data$obv, na.rm = TRUE), .groups = "drop")
}

#' Detect best available OBV column in player match stats
detect_player_obv_column <- function(player_match_stats_df) {
  if (is.null(player_match_stats_df) || nrow(player_match_stats_df) == 0) {
    return(NULL)
  }
  candidates <- c(
    "player_match_obv",
    "player_match_obv_total_net",
    "player_match_obv_pass",
    "player_match_obv_shot"
  )
  hit <- candidates[candidates %in% names(player_match_stats_df)]
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  NULL
}

#' Event-derived per-player skill counts for one match
compute_player_skill_from_events <- function(events_df,
                                             match_id = NULL,
                                             player_match_stats_df = NULL,
                                             root = get_project_root()) {
  data <- ensure_viz_aliases(events_df)
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }

  pick_players <- function(df) {
    df %>%
      dplyr::filter(!is.na(.data$player_id)) %>%
      dplyr::select("player_id", "player_name", "team_name") %>%
      dplyr::distinct()
  }

  np_xg <- filter_np_shot_events(data, match_id = NULL) %>%
    dplyr::filter(!is.na(.data$`player.id`)) %>%
    dplyr::group_by(.data$`player.id`, .data$`player.name`, .data$`team.name`) %>%
    dplyr::summarise(np_xg = sum(.data$xg, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(
      player_id = "player.id",
      player_name = "player.name",
      team_name = "team.name"
    )

  key_passes <- data %>%
    dplyr::filter(
      .data$`type.name` == "Pass",
      .data$`pass.shot_assist` %in% TRUE,
      !is.na(.data$`player.id`)
    ) %>%
    dplyr::count(.data$`player.id`, .data$`player.name`, .data$`team.name`, name = "key_passes") %>%
    dplyr::rename(
      player_id = "player.id",
      player_name = "player.name",
      team_name = "team.name"
    )

  carries <- data %>%
    dplyr::filter(.data$`type.name` == "Carry", !is.na(.data$`player.id`)) %>%
    dplyr::count(.data$`player.id`, .data$`player.name`, .data$`team.name`, name = "carries") %>%
    dplyr::rename(
      player_id = "player.id",
      player_name = "player.name",
      team_name = "team.name"
    )

  pressures <- data %>%
    dplyr::filter(.data$`type.name` == "Pressure", !is.na(.data$`player.id`)) %>%
    dplyr::count(.data$`player.id`, .data$`player.name`, .data$`team.name`, name = "pressures") %>%
    dplyr::rename(
      player_id = "player.id",
      player_name = "player.name",
      team_name = "team.name"
    )

  pass_stats <- data %>%
    dplyr::filter(.data$`type.name` == "Pass", !is.na(.data$`player.id`)) %>%
    dplyr::group_by(.data$`player.id`, .data$`player.name`, .data$`team.name`) %>%
    dplyr::summarise(
      pass_attempts = dplyr::n(),
      pass_completed = sum(is.na(.data$`pass.outcome.name`)),
      pass_accuracy = dplyr::if_else(
        .data$pass_attempts > 0,
        .data$pass_completed / .data$pass_attempts,
        0
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = "player.id",
      player_name = "player.name",
      team_name = "team.name"
    )

  shots_on_target <- data %>%
    dplyr::filter(.data$`type.name` == "Shot", !is.na(.data$`player.id`)) %>%
    dplyr::group_by(.data$`player.id`, .data$`player.name`, .data$`team.name`) %>%
    dplyr::summarise(
      shots_on_target = sum(
        .data$`shot.outcome.name` %in% c("Goal", "Saved"),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = "player.id",
      player_name = "player.name",
      team_name = "team.name"
    )

  if ("obv_total_net" %in% names(data) && any(!is.na(data$obv_total_net))) {
    obv <- data %>%
      dplyr::filter(!is.na(.data$`player.id`)) %>%
      dplyr::group_by(.data$`player.id`, .data$`player.name`, .data$`team.name`) %>%
      dplyr::summarise(obv = sum(.data$obv_total_net, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename(
        player_id = "player.id",
        player_name = "player.name",
        team_name = "team.name"
      )
  } else if (!is.null(match_id)) {
    obv <- compute_player_obv_from_raw_events(match_id, root = root)
  } else {
    obv <- tibble::tibble(
      player_id = integer(),
      player_name = character(),
      team_name = character(),
      obv = numeric()
    )
  }

  obv_col <- detect_player_obv_column(player_match_stats_df)
  if (!is.null(obv_col) && !is.null(player_match_stats_df) && nrow(player_match_stats_df) > 0) {
    pms_obv <- player_match_stats_df %>%
      dplyr::filter(!is.na(.data$player_id)) %>%
      dplyr::transmute(
        player_id = .data$player_id,
        player_name = .data$player_name,
        team_name = .data$team_name,
        obv_pms = .data[[obv_col]]
      )
    obv <- obv %>%
      dplyr::full_join(pms_obv, by = c("player_id", "player_name", "team_name")) %>%
      dplyr::mutate(obv = dplyr::coalesce(.data$obv, .data$obv_pms, 0)) %>%
      dplyr::select(-"obv_pms")
  }

  all_players <- dplyr::bind_rows(
    pick_players(np_xg),
    pick_players(key_passes),
    pick_players(carries),
    pick_players(pressures),
    pick_players(pass_stats),
    pick_players(shots_on_target),
    pick_players(obv)
  ) %>%
    dplyr::distinct(.data$player_id, .data$player_name, .data$team_name)

  all_players %>%
    dplyr::left_join(np_xg, by = c("player_id", "player_name", "team_name")) %>%
    dplyr::left_join(key_passes, by = c("player_id", "player_name", "team_name")) %>%
    dplyr::left_join(carries, by = c("player_id", "player_name", "team_name")) %>%
    dplyr::left_join(pressures, by = c("player_id", "player_name", "team_name")) %>%
    dplyr::left_join(
      pass_stats %>%
        dplyr::select("player_id", "player_name", "team_name", "pass_accuracy"),
      by = c("player_id", "player_name", "team_name")
    ) %>%
    dplyr::left_join(shots_on_target, by = c("player_id", "player_name", "team_name")) %>%
    dplyr::left_join(obv, by = c("player_id", "player_name", "team_name")) %>%
    dplyr::mutate(
      dplyr::across(
        c(
          "np_xg",
          "key_passes",
          "carries",
          "pressures",
          "pass_accuracy",
          "shots_on_target",
          "obv"
        ),
        ~ dplyr::coalesce(.x, 0)
      ),
      skill_index = .data$np_xg +
        0.12 * .data$key_passes +
        0.004 * .data$carries +
        0.002 * .data$pressures +
        0.08 * .data$shots_on_target +
        .data$obv
    )
}

PLAYER_SKILL_METRIC_SPEC <- tibble::tribble(
  ~field, ~label, ~type,
  "np_xg", "NP xG", "decimal",
  "key_passes", "Key passes", "count",
  "carries", "Carries", "count",
  "pressures", "Pressures", "count",
  "pass_accuracy", "Passing accuracy", "rate",
  "shots_on_target", "Shots on target", "count",
  "obv", "OBV", "decimal"
)

#' Per-player skill board for both teams (event-derived)
compute_player_skill_board <- function(events_df,
                                       meta,
                                       match_id = NULL,
                                       player_match_stats_df = NULL,
                                       home_color = SDC_PALETTE[["green"]],
                                       away_color = SDC_PALETTE[["red"]],
                                       root = get_project_root()) {
  board <- compute_player_skill_from_events(
    events_df,
    match_id = match_id,
    player_match_stats_df = player_match_stats_df,
    root = root
  )

  if (nrow(board) == 0) {
    return(board)
  }

  board %>%
    dplyr::mutate(
      display_team = dplyr::case_when(
        .data$team_name == meta$home_team[[1]] ~ meta$display_home[[1]],
        .data$team_name == meta$away_team[[1]] ~ meta$display_away[[1]],
        TRUE ~ .data$team_name
      ),
      team_color = dplyr::if_else(
        .data$team_name == meta$home_team[[1]],
        home_color,
        away_color
      ),
      player_label = vapply(
        .data$player_name,
        passing_network_label,
        character(1)
      )
    )
}

#' Top performer per team by event-derived skill index
find_team_top_performers <- function(board, meta) {
  home <- board %>%
    dplyr::filter(.data$team_name == meta$home_team[[1]]) %>%
    dplyr::slice_max(.data$skill_index, n = 1, with_ties = FALSE)
  away <- board %>%
    dplyr::filter(.data$team_name == meta$away_team[[1]]) %>%
    dplyr::slice_max(.data$skill_index, n = 1, with_ties = FALSE)

  if (nrow(home) == 0 || nrow(away) == 0) {
    stop("Could not identify top performers for both teams.", call. = FALSE)
  }

  list(home = home, away = away)
}

#' Share-bar rows comparing two players on skill metrics
compute_player_metric_share <- function(home_row, away_row) {
  purrr::pmap_dfr(
    PLAYER_SKILL_METRIC_SPEC,
    function(field, label, type) {
      home_val <- dplyr::coalesce(home_row[[field]], 0)
      away_val <- dplyr::coalesce(away_row[[field]], 0)
      total <- home_val + away_val
      home_share <- if (total > 0) home_val / total else 0.5

      fmt <- switch(
        type,
        decimal = function(x) format(round(x, 2), nsmall = 2),
        count = function(x) format(as.integer(x), big.mark = ","),
        rate = function(x) scales::percent(x, accuracy = 1),
        function(x) as.character(x)
      )

      tibble::tibble(
        metric = label,
        home_share = home_share,
        away_share = 1 - home_share,
        home_label = fmt(home_val),
        away_label = fmt(away_val)
      )
    }
  )
}

#' Panel B — top-performer metric share bars (Game Quality style)
viz_player_skill_comparison <- function(events_df,
                                        meta,
                                        match_id = NULL,
                                        player_match_stats_df = NULL,
                                        home_color = SDC_PALETTE[["green"]],
                                        away_color = SDC_PALETTE[["red"]],
                                        title = "Who stood out",
                                        subtitle = NULL,
                                        root = get_project_root()) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  board <- compute_player_skill_board(
    events_df,
    meta = meta,
    match_id = match_id,
    player_match_stats_df = player_match_stats_df,
    home_color = home_color,
    away_color = away_color,
    root = root
  )

  if (nrow(board) == 0) {
    stop("No player skill metrics available for comparison.", call. = FALSE)
  }

  tops <- find_team_top_performers(board, meta)
  share_df <- compute_player_metric_share(tops$home, tops$away)

  home_header <- paste0(
    tops$home$player_label[[1]],
    " (",
    tops$home$display_team[[1]],
    ")"
  )
  away_header <- paste0(
    tops$away$player_label[[1]],
    " (",
    tops$away$display_team[[1]],
    ")"
  )

  if (is.null(subtitle)) {
    subtitle <- paste0(
      tops$home$player_label[[1]],
      " vs ",
      tops$away$player_label[[1]],
      " — each row is a 100% share of the combined total"
    )
  }

  panel <- viz_match_share_section(
    share_df,
    section_title = "Individual metrics",
    home_color = tops$home$team_color[[1]],
    away_color = tops$away$team_color[[1]],
    display_home = home_header,
    display_away = away_header,
    bar_half = 0.24,
    row_step = 1.16,
    bar_y_base = 0.35,
    header_bar_gap = 0.78,
    value_label_size = 3.05,
    metric_label_size = 2.75,
    metric_label_family = SDC_FONTS$title,
    metric_label_offset = 0.24
  )

  panel +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme_sdc(base_size = 10) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0),
          plot.subtitle = ggplot2::element_text(size = 9, hjust = 0, colour = "#555555"),
          plot.margin = ggplot2::margin(t = 6, r = 0, b = 2, l = 0)
        )
    )
}

#' Compact featured-player shot panel for grid footer
viz_featured_player_shots_compact <- function(events_df,
                                              meta,
                                              player_name = "Riyad Mahrez",
                                              match_id = NULL,
                                              team_color = SDC_PALETTE[["green"]],
                                              title = "Mahrez: both Algeria goals") {
  featured <- resolve_featured_player(events_df, player_name)
  data <- filter_shot_map_data(
    events_df,
    player_id = featured$player.id,
    match_id = match_id,
    exclude_penalties = TRUE
  )

  if (nrow(data) == 0) {
    stop("No shots found for featured player.", call. = FALSE)
  }

  goals_n <- sum(data$`shot.outcome.name` == "Goal", na.rm = TRUE)
  np_xg <- sum(data$`shot.statsbomb_xg`, na.rm = TRUE)

  ggplot2::ggplot(data) +
    draw_pitch_half_attacking(colour = "black", linewidth = 0.45) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$location.x,
        y = .data$location.y,
        size = .data$`shot.statsbomb_xg`,
        alpha = .data$`shot.outcome.name` == "Goal"
      ),
      colour = team_color,
      fill = team_color
    ) +
    ggplot2::scale_size_continuous(range = c(2.5, 7), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
    ggplot2::annotate(
      "text",
      x = 6,
      y = 74,
      label = paste0(
        featured$player_label,
        " · ",
        goals_n,
        " goal",
        if (goals_n != 1) "s" else "",
        " · ",
        format(round(np_xg, 2), nsmall = 2),
        " NP xG"
      ),
      hjust = 0,
      family = SDC_FONTS$body,
      size = 2.8,
      fontface = "bold",
      colour = "#222222"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / 120) +
    ggplot2::labs(title = title, subtitle = NULL, x = NULL, y = NULL) +
    theme_sdc(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 4, 2, 4)
    )
}

#' Assemble Chart 2 structure / momentum / networks grid
viz_structure_momentum_grid <- function(events_df,
                                        meta,
                                        team_match_stats_df = NULL,
                                        player_match_stats_df = NULL,
                                        lineups_df = NULL,
                                        match_id = NULL,
                                        home_color = "#006233",
                                        away_color = "#ED2939",
                                        title = "Same scoreline, different rhythms",
                                        subtitle = NULL,
                                        root = get_project_root()) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  match_id <- match_id %||% meta$match_id[[1]]
  if (is.null(subtitle)) {
    subtitle <- match_chart_subtitle(meta)
  }

  panel_a <- viz_xg_momentum_timeline(
    events_df,
    meta = meta,
    match_id = match_id,
    home_color = home_color,
    away_color = away_color,
    root = root
  )

  panel_b <- viz_player_skill_comparison(
    events_df,
    meta = meta,
    match_id = match_id,
    player_match_stats_df = player_match_stats_df,
    home_color = home_color,
    away_color = away_color,
    root = root
  )

  panel_c <- viz_match_passing_networks_combined(
    events_df,
    match_id = match_id,
    meta = meta,
    home_color = home_color,
    away_color = away_color
  )

  top_row <- patchwork::wrap_plots(
    list(panel_a, panel_b),
    ncol = 2,
    widths = c(0.70, 0.30)
  )

  grid_body <- patchwork::wrap_plots(
    top_row,
    patchwork::plot_spacer(),
    panel_c,
    ncol = 1,
    heights = c(0.36, 0.006, 0.634)
  )

  grid_body +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme_sdc_grid()
    )
}
