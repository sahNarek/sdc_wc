#' Interval labels for 10-minute match bins
MATCH_INTERVAL_LABELS <- c(
  "0-10", "11-20", "21-30", "31-40", "41-50",
  "51-60", "61-70", "71-80", "81-90", "91-100"
)

SHARE_SECTION_LABELS <- c(
  circulation = "Keeping the ball",
  pressing = "Press and recover",
  threat = "Threat in the final third"
)

DEFENSIVE_ACTION_TYPES <- c(
  "Pressure", "Block", "Interception", "Foul Committed", "Duel"
)

#' Theme for composite grid titles (centred article header)
theme_sdc_grid <- function(base_size = 13) {
  theme_sdc(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = base_size + 12,
        colour = "#111111",
        hjust = 0.5,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = base_size + 1,
        colour = "#444444",
        hjust = 0.5,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = base_size - 2,
        colour = "#666666",
        hjust = 0.5,
        margin = ggplot2::margin(t = 6)
      )
    )
}

#' Map match minute to a 10-minute interval label
match_minute_interval <- function(minute) {
  dplyr::case_when(
    minute <= 10 ~ "0-10",
    minute <= 20 ~ "11-20",
    minute <= 30 ~ "21-30",
    minute <= 40 ~ "31-40",
    minute <= 50 ~ "41-50",
    minute <= 60 ~ "51-60",
    minute <= 70 ~ "61-70",
    minute <= 80 ~ "71-80",
    minute <= 90 ~ "81-90",
    TRUE ~ "91-100"
  )
}

#' Inclusive minute bounds for each interval label
interval_minute_bounds <- function(interval_label) {
  bounds <- list(
    "0-10" = c(0L, 10L),
    "11-20" = c(11L, 20L),
    "21-30" = c(21L, 30L),
    "31-40" = c(31L, 40L),
    "41-50" = c(41L, 50L),
    "51-60" = c(51L, 60L),
    "61-70" = c(61L, 70L),
    "71-80" = c(71L, 80L),
    "81-90" = c(81L, 90L),
    "91-100" = c(91L, 100L)
  )
  bounds[[interval_label]]
}

#' Progress through an interval (0 at period start, 1 at period end)
interval_minute_fraction <- function(minute) {
  interval_label <- match_minute_interval(minute)
  bounds <- interval_minute_bounds(interval_label)
  span <- bounds[[2]] - bounds[[1]]
  if (span <= 0) {
    return(0)
  }
  (minute - bounds[[1]]) / span
}

#' Display label for a goal minute on the PPDA chart
format_ppda_goal_minute_label <- function(minute) {
  paste0(minute, "'")
}

#' Nudge overlapping goal labels that share similar x positions
assign_ppda_goal_label_offsets <- function(goals_df,
                                           home_team,
                                           x_threshold = 0.65,
                                           y_step = 0.28) {
  if (nrow(goals_df) == 0) {
    return(goals_df)
  }

  goals_df <- goals_df %>%
    dplyr::arrange(.data$minute) %>%
    dplyr::mutate(
      label_x = .data$x_num,
      label_y = .data$y + 0.12,
      label_hjust = 0.5
    )

  for (i in seq_len(nrow(goals_df))) {
    for (j in seq_len(i - 1L)) {
      if (abs(goals_df$x_num[i] - goals_df$x_num[j]) < x_threshold) {
        goals_df$label_y[i] <- max(goals_df$label_y[i], goals_df$label_y[j] + y_step)
        is_home <- goals_df$team_name[i] == home_team
        goals_df$label_x[i] <- if (is_home) {
          goals_df$x_num[i] - 0.12
        } else {
          goals_df$x_num[i] + 0.12
        }
        goals_df$label_hjust[i] <- if (is_home) 1 else 0
      }
    }
  }

  goals_df
}

#' Interpolate goal position along the PPDA line segment for its interval
interpolate_goal_on_ppda_line <- function(minute, team_name, ppda_df) {
  interval_label <- match_minute_interval(minute)
  idx <- match(interval_label, MATCH_INTERVAL_LABELS)
  frac <- interval_minute_fraction(minute)
  n_intervals <- length(MATCH_INTERVAL_LABELS)
  whistle_segment_cap <- 0.92

  team_ppda <- ppda_df %>%
    dplyr::filter(.data$team_name == .env$team_name) %>%
    dplyr::arrange(.data$interval) %>%
    dplyr::pull(.data$ppda)

  if (idx < n_intervals) {
    y_start <- team_ppda[[idx]]
    y_end <- team_ppda[[idx + 1]]
    y <- y_start + frac * (y_end - y_start)
    x_num <- idx + frac
  } else {
    # Stoppage-time goals sit on the segment before the final whistle (81-90 -> end)
    y_start <- team_ppda[[idx - 1]]
    y_end <- team_ppda[[idx]]
    capped_frac <- frac * whistle_segment_cap
    y <- y_start + capped_frac * (y_end - y_start)
    x_num <- (idx - 1) + capped_frac
  }

  list(
    x_num = x_num,
    y = y,
    interval = interval_label
  )
}

#' Whether team attacks toward x = 120 in a given period
team_attacks_high_x <- function(events_df, team_name) {
  shots <- events_df %>%
    dplyr::filter(
      .data$`type.name` == "Shot",
      .data$`team.name` == team_name,
      !is.na(.data$`location.x`)
    )
  if (nrow(shots) == 0) {
    return(TRUE)
  }
  stats::median(shots$`location.x`, na.rm = TRUE) >= 60
}

#' Opponent passes in the defending 60% of the pitch (PPDA denominator context)
opponent_passes_in_defending_sixty <- function(events_df,
                                               defending_team,
                                               interval_label = NULL) {
  data <- ensure_viz_aliases(events_df)
  teams <- unique(data$`team.name`)
  teams <- teams[!is.na(teams)]
  opponent <- setdiff(teams, defending_team)
  if (length(opponent) != 1) {
    return(0)
  }
  opponent <- opponent[[1]]

  opp_passes <- data %>%
    dplyr::filter(
      .data$`type.name` == "Pass",
      .data$`team.name` == opponent,
      !is.na(.data$`location.x`)
    )

  if (!is.null(interval_label)) {
    opp_passes <- opp_passes %>%
      dplyr::mutate(.interval = match_minute_interval(.data$minute)) %>%
      dplyr::filter(.data$.interval == .env$interval_label)
  }

  attacks_high <- team_attacks_high_x(data, opponent)
  if (attacks_high) {
    sum(opp_passes$`location.x` <= 72, na.rm = TRUE)
  } else {
    sum(opp_passes$`location.x` >= 48, na.rm = TRUE)
  }
}

#' Defensive actions by a team (PPDA numerator context)
team_defensive_actions <- function(events_df, team_name, interval_label = NULL) {
  data <- ensure_viz_aliases(events_df)
  actions <- data %>%
    dplyr::filter(
      .data$`type.name` %in% DEFENSIVE_ACTION_TYPES,
      .data$`team.name` == team_name
    )
  if (!is.null(interval_label)) {
    actions <- actions %>%
      dplyr::mutate(.interval = match_minute_interval(.data$minute)) %>%
      dplyr::filter(.data$.interval == .env$interval_label)
  }
  nrow(actions)
}

#' PPDA for one team over one interval (events-derived)
compute_team_interval_ppda <- function(events_df, team_name, interval_label) {
  passes <- opponent_passes_in_defending_sixty(
    events_df,
    defending_team = team_name,
    interval_label = interval_label
  )
  actions <- team_defensive_actions(events_df, team_name, interval_label = interval_label)
  if (actions == 0) {
    return(NA_real_)
  }
  passes / actions
}

#' Full-match PPDA from events (fallback when stats JSON missing)
compute_team_match_ppda <- function(events_df, team_name) {
  passes <- opponent_passes_in_defending_sixty(events_df, defending_team = team_name)
  actions <- team_defensive_actions(events_df, team_name)
  if (actions == 0) {
    return(NA_real_)
  }
  passes / actions
}

#' Load team / player aggregates; prefer official JSON when present
load_match_quality_stats <- function(match_id,
                                     events = NULL,
                                     meta = NULL,
                                     team_match_stats_df = NULL,
                                     player_match_stats_df = NULL,
                                     root = get_project_root()) {
  match_id <- as.integer(match_id)

  raw_tms <- read_match_json_optional(match_id, "team_match_stats.json", root = root)
  raw_pms <- read_match_json_optional(match_id, "player_match_stats.json", root = root)

  if (length(raw_tms) > 0) {
    tms <- parse_team_match_stats_json(raw_tms, match_id)
    stats_source <- "official"
  } else {
    tms <- team_match_stats_df
    if (is.null(tms) || nrow(tms) == 0 || !"team_match_ppda" %in% names(tms)) {
      tms <- estimate_team_quality_stats(events, meta, match_id)
    }
    stats_source <- "estimated"
  }

  if (length(raw_pms) > 0) {
    pms <- parse_player_match_stats_json(raw_pms, match_id)
  } else {
    pms <- player_match_stats_df
    if (is.null(pms) || nrow(pms) == 0 || !"player_match_key_passes" %in% names(pms)) {
      pms <- estimate_player_quality_stats(events, match_id)
    }
  }

  list(
    team_match_stats = tms,
    player_match_stats = pms,
    stats_source = stats_source
  )
}

#' Official full-match PPDA per team (NULL if unavailable)
get_official_ppda <- function(team_match_stats_df) {
  if (is.null(team_match_stats_df) || nrow(team_match_stats_df) == 0) {
    return(tibble::tibble())
  }
  if (!"team_match_ppda" %in% names(team_match_stats_df)) {
    return(tibble::tibble())
  }
  team_match_stats_df %>%
    dplyr::transmute(
      team_name = .data$team_name,
      ppda = .data$team_match_ppda
    )
}

#' Estimate team-level quality metrics from events when stats JSON is absent
estimate_team_quality_stats <- function(events_df, meta, match_id) {
  data <- ensure_viz_aliases(events_df)
  teams <- c(meta$home_team[[1]], meta$away_team[[1]])

  dplyr::bind_rows(lapply(teams, function(team) {
    opp <- setdiff(teams, team)[[1]]
    passes <- data %>% dplyr::filter(.data$`type.name` == "Pass", .data$`team.name` == team)
    completed <- passes %>% dplyr::filter(is.na(.data$`pass.outcome.name`))
    shots <- data %>%
      dplyr::filter(.data$`type.name` == "Shot", .data$`team.name` == team) %>%
      dplyr::filter(is.na(.data$`shot.type.name`) | .data$`shot.type.name` != "Penalty")

    attacks_high <- team_attacks_high_x(data, team)
    pressures <- data %>%
      dplyr::filter(.data$`type.name` == "Pressure", .data$`team.name` == team)
    if (attacks_high) {
      fhalf_pressures <- sum(pressures$`location.x` >= 60, na.rm = TRUE)
      box_entries <- data %>%
        dplyr::filter(.data$`team.name` == team, .data$`type.name` == "Pass") %>%
        dplyr::filter(
          !is.na(.data$`pass.end_location.x`),
          .data$`pass.end_location.x` >= 102,
          .data$`pass.end_location.y` >= 18,
          .data$`pass.end_location.y` <= 62
        )
      deep_prog <- data %>%
        dplyr::filter(.data$`team.name` == team, .data$`type.name` %in% c("Pass", "Carry")) %>%
        dplyr::filter(!is.na(.data$`location.x`), .data$`location.x` >= 80)
    } else {
      fhalf_pressures <- sum(pressures$`location.x` <= 60, na.rm = TRUE)
      box_entries <- data %>%
        dplyr::filter(.data$`team.name` == team, .data$`type.name` == "Pass") %>%
        dplyr::filter(
          !is.na(.data$`pass.end_location.x`),
          .data$`pass.end_location.x` <= 18,
          .data$`pass.end_location.y` >= 18,
          .data$`pass.end_location.y` <= 62
        )
      deep_prog <- data %>%
        dplyr::filter(.data$`team.name` == team, .data$`type.name` %in% c("Pass", "Carry")) %>%
        dplyr::filter(!is.na(.data$`location.x`), .data$`location.x` <= 40)
    }

    recoveries_opp_half <- compute_high_recoveries(data, team)
    recoveries_all <- data %>%
      dplyr::filter(.data$`type.name` == "Ball Recovery", .data$`team.name` == team) %>%
      nrow()
    carries <- data %>%
      dplyr::filter(.data$`type.name` == "Carry", .data$`team.name` == team) %>%
      nrow()
    interceptions <- data %>%
      dplyr::filter(.data$`type.name` == "Interception", .data$`team.name` == team) %>%
      nrow()
    tackles <- data %>%
      dplyr::filter(
        .data$`type.name` == "Duel",
        .data$`duel.type.name` == "Tackle",
        .data$`team.name` == team
      ) %>%
      nrow()
    fouls_committed <- data %>%
      dplyr::filter(.data$`type.name` == "Foul Committed", .data$`team.name` == team) %>%
      nrow()

    tibble::tibble(
      match_id = as.integer(match_id),
      team_name = team,
      opposition_name = opp,
      team_match_goals = as.integer(meta$home_score[[1]] * (team == meta$home_team[[1]]) +
        meta$away_score[[1]] * (team == meta$away_team[[1]])),
      team_match_possession = {
        pass_n <- nrow(passes)
        opp_passes_n <- nrow(data %>% dplyr::filter(.data$`type.name` == "Pass", .data$`team.name` == opp))
        total <- pass_n + opp_passes_n
        if (total > 0) pass_n / total else 0.5
      },
      team_match_successful_passes = nrow(completed),
      team_match_passes = nrow(passes),
      team_match_passing_ratio = if (nrow(passes) > 0) {
        nrow(completed) / nrow(passes)
      } else {
        NA_real_
      },
      team_match_carries = carries,
      team_match_fhalf_pressures = fhalf_pressures,
      team_match_pressure_regains = recoveries_all,
      team_match_interceptions = interceptions,
      team_match_tackles = tackles,
      team_match_fouls_committed = fouls_committed,
      team_match_deep_progressions = nrow(deep_prog),
      team_match_passes_inside_box = nrow(box_entries),
      team_match_np_xg = sum(shots$`shot.statsbomb_xg`, na.rm = TRUE),
      team_match_np_shots = nrow(shots),
      team_match_ppda = compute_team_match_ppda(data, team)
    )
  }))
}

#' Estimate player-level fields needed for team sums
estimate_player_quality_stats <- function(events_df, match_id) {
  data <- ensure_viz_aliases(events_df)
  data %>%
    dplyr::filter(!is.na(.data$`player.id`)) %>%
    dplyr::group_by(.data$`player.id`, .data$`player.name`, .data$`team.name`) %>%
    dplyr::summarise(
      player_match_key_passes = sum(
        .data$`type.name` == "Pass" &
          (.data$`pass.shot_assist` %in% TRUE),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      match_id = as.integer(match_id),
      team_name = .data$`team.name`,
      player_id = as.integer(.data$`player.id`),
      player_name = .data$`player.name`,
      player_match_key_passes = .data$player_match_key_passes
    )
}

#' Ball recoveries in the opponent's half
compute_high_recoveries <- function(events_df, team_name) {
  data <- ensure_viz_aliases(events_df)
  teams <- unique(data$`team.name`)
  teams <- teams[!is.na(teams)]
  opponent <- setdiff(teams, team_name)
  if (length(opponent) != 1) {
    return(0L)
  }

  rec <- data %>%
    dplyr::filter(
      .data$`type.name` == "Ball Recovery",
      .data$`team.name` == team_name,
      !is.na(.data$`location.x`)
    )

  attacks_high <- team_attacks_high_x(data, team_name)
  if (attacks_high) {
    sum(rec$`location.x` >= 60, na.rm = TRUE)
  } else {
    sum(rec$`location.x` <= 60, na.rm = TRUE)
  }
}

#' PPDA by 10-minute interval for both teams
compute_ppda_by_interval <- function(events_df, meta) {
  teams <- c(meta$home_team[[1]], meta$away_team[[1]])
  intervals <- factor(MATCH_INTERVAL_LABELS, levels = MATCH_INTERVAL_LABELS)

  dplyr::bind_rows(lapply(teams, function(team) {
    tibble::tibble(
      team_name = team,
      interval = intervals,
      ppda = vapply(
        MATCH_INTERVAL_LABELS,
        function(iv) compute_team_interval_ppda(events_df, team, iv),
        numeric(1)
      )
    )
  })) %>%
    dplyr::mutate(
      interval = factor(.data$interval, levels = MATCH_INTERVAL_LABELS),
      display_team = dplyr::if_else(
        .data$team_name == meta$home_team[[1]],
        meta$display_home[[1]],
        meta$display_away[[1]]
      )
    )
}

#' Goals vs non-penalty xG summary for Panel B
compute_goals_vs_xg_summary <- function(team_match_stats_df, meta) {
  tms <- team_match_stats_df
  if (!"team_match_np_xg" %in% names(tms)) {
    stop("team_match_np_xg not available in team match stats.", call. = FALSE)
  }

  goals_col <- if ("team_match_goals" %in% names(tms)) {
    "team_match_goals"
  } else {
    NULL
  }

  tms %>%
    dplyr::transmute(
      team_name = .data$team_name,
      display_team = dplyr::case_when(
        .data$team_name == meta$home_team[[1]] ~ meta$display_home[[1]],
        .data$team_name == meta$away_team[[1]] ~ meta$display_away[[1]],
        TRUE ~ .data$team_name
      ),
      goals = if (!is.null(goals_col)) {
        .data[[goals_col]]
      } else {
        dplyr::if_else(
          .data$team_name == meta$home_team[[1]],
          meta$home_score[[1]],
          meta$away_score[[1]]
        )
      },
      np_xg = .data$team_match_np_xg,
      delta = .data$goals - .data$np_xg
    ) %>%
    dplyr::mutate(
      team_color = dplyr::if_else(
        .data$team_name == meta$home_team[[1]],
        SDC_PALETTE[["green"]],
        SDC_PALETTE[["red"]]
      ),
      team_order = dplyr::if_else(.data$team_name == meta$home_team[[1]], 1L, 2L)
    ) %>%
    dplyr::arrange(.data$team_order)
}

#' Share of passes that are backward or sideways (safe circulation)
compute_safe_pass_share <- function(events_df, team_name) {
  data <- ensure_viz_aliases(events_df)
  passes <- data %>%
    dplyr::filter(
      .data$`type.name` == "Pass",
      .data$`team.name` == team_name,
      !is.na(.data$`location.x`),
      !is.na(.data$`pass.end_location.x`)
    )

  if (nrow(passes) == 0) {
    return(0)
  }

  attacks_high <- team_attacks_high_x(data, team_name)
  dx <- passes$`pass.end_location.x` - passes$`location.x`
  if (!attacks_high) {
    dx <- -dx
  }

  safe <- sum(abs(dx) < 5 | dx < 0, na.rm = TRUE)
  safe / nrow(passes)
}

#' Build long-format rows for match-share section panels
compute_match_share_metrics <- function(team_match_stats_df,
                                        player_match_stats_df,
                                        events_df,
                                        meta) {
  home <- meta$home_team[[1]]
  away <- meta$away_team[[1]]
  tms <- team_match_stats_df
  pms <- player_match_stats_df

  event_derived_fields <- c(
    "team_match_passes",
    "team_match_carries",
    "team_match_interceptions",
    "team_match_tackles",
    "team_match_fouls_committed"
  )
  missing_fields <- setdiff(event_derived_fields, names(tms))
  if (length(missing_fields) > 0) {
    estimated <- estimate_team_quality_stats(
      events_df,
      meta,
      tms$match_id[[1]] %||% meta$match_id[[1]]
    )
    tms <- tms %>%
      dplyr::left_join(
        estimated %>%
          dplyr::select("team_name", dplyr::any_of(missing_fields)),
        by = "team_name"
      )
  }

  row_value <- function(field, label, section_key, type = "count") {
    home_val <- tms %>%
      dplyr::filter(.data$team_name == home) %>%
      dplyr::pull(.data[[field]])
    away_val <- tms %>%
      dplyr::filter(.data$team_name == away) %>%
      dplyr::pull(.data[[field]])
    home_val <- home_val[[1]] %||% 0
    away_val <- away_val[[1]] %||% 0

    if (type == "rate") {
      total <- home_val + away_val
      home_share <- if (total > 0) home_val / total else 0.5
      away_share <- 1 - home_share
      home_label <- scales::percent(home_val, accuracy = 1)
      away_label <- scales::percent(away_val, accuracy = 1)
    } else if (type == "share") {
      total <- home_val + away_val
      home_share <- if (total > 0) home_val / total else 0.5
      away_share <- 1 - home_share
      home_label <- if (field == "team_match_possession") {
        scales::percent(home_val, accuracy = 1)
      } else {
        format(round(home_val, ifelse(grepl("xg", field), 2, 0)), nsmall = ifelse(grepl("xg", field), 2, 0))
      }
      away_label <- if (field == "team_match_possession") {
        scales::percent(away_val, accuracy = 1)
      } else {
        format(round(away_val, ifelse(grepl("xg", field), 2, 0)), nsmall = ifelse(grepl("xg", field), 2, 0))
      }
    } else {
      total <- home_val + away_val
      home_share <- if (total > 0) home_val / total else 0.5
      away_share <- 1 - home_share
      home_label <- format(round(home_val, 0), big.mark = ",")
      away_label <- format(round(away_val, 0), big.mark = ",")
    }

    tibble::tibble(
      section_key = section_key,
      section = SHARE_SECTION_LABELS[[section_key]],
      metric = label,
      home_share = home_share,
      away_share = away_share,
      home_label = home_label,
      away_label = away_label
    )
  }

  key_home <- pms %>%
    dplyr::filter(.data$team_name == home) %>%
    dplyr::summarise(v = sum(.data$player_match_key_passes, na.rm = TRUE)) %>%
    dplyr::pull(.data$v)
  key_away <- pms %>%
    dplyr::filter(.data$team_name == away) %>%
    dplyr::summarise(v = sum(.data$player_match_key_passes, na.rm = TRUE)) %>%
    dplyr::pull(.data$v)
  key_total <- key_home + key_away

  high_home <- compute_high_recoveries(events_df, home)
  high_away <- compute_high_recoveries(events_df, away)
  high_total <- high_home + high_away

  pass_acc_home <- tms %>% dplyr::filter(.data$team_name == home) %>% dplyr::pull(.data$team_match_passing_ratio)
  pass_acc_away <- tms %>% dplyr::filter(.data$team_name == away) %>% dplyr::pull(.data$team_match_passing_ratio)

  safe_home <- compute_safe_pass_share(events_df, home)
  safe_away <- compute_safe_pass_share(events_df, away)
  safe_total <- safe_home + safe_away

  dplyr::bind_rows(
    row_value("team_match_possession", "Possession", "circulation", "share"),
    row_value("team_match_successful_passes", "Completed passes", "circulation", "count"),
    tibble::tibble(
      section_key = "circulation",
      section = SHARE_SECTION_LABELS[["circulation"]],
      metric = "Passing accuracy",
      home_share = {
        h <- pass_acc_home[[1]] %||% 0
        a <- pass_acc_away[[1]] %||% 0
        if ((h + a) > 0) h / (h + a) else 0.5
      },
      away_share = {
        h <- pass_acc_home[[1]] %||% 0
        a <- pass_acc_away[[1]] %||% 0
        if ((h + a) > 0) a / (h + a) else 0.5
      },
      home_label = scales::percent(pass_acc_home[[1]] %||% 0, accuracy = 1),
      away_label = scales::percent(pass_acc_away[[1]] %||% 0, accuracy = 1)
    ),
    tibble::tibble(
      section_key = "circulation",
      section = SHARE_SECTION_LABELS[["circulation"]],
      metric = "Safe pass share",
      home_share = if (safe_total > 0) safe_home / safe_total else 0.5,
      away_share = if (safe_total > 0) safe_away / safe_total else 0.5,
      home_label = scales::percent(safe_home, accuracy = 1),
      away_label = scales::percent(safe_away, accuracy = 1)
    ),
    row_value("team_match_passes", "Pass attempts", "circulation", "count"),
    row_value("team_match_carries", "Carries", "circulation", "count"),
    row_value("team_match_fhalf_pressures", "Opponent-half pressures", "pressing", "count"),
    row_value("team_match_pressure_regains", "Pressure regains", "pressing", "count"),
    tibble::tibble(
      section_key = "pressing",
      section = SHARE_SECTION_LABELS[["pressing"]],
      metric = "High recoveries",
      home_share = if (high_total > 0) high_home / high_total else 0.5,
      away_share = if (high_total > 0) high_away / high_total else 0.5,
      home_label = format(high_home, big.mark = ","),
      away_label = format(high_away, big.mark = ",")
    ),
    row_value("team_match_interceptions", "Interceptions", "pressing", "count"),
    row_value("team_match_tackles", "Tackles", "pressing", "count"),
    row_value("team_match_fouls_committed", "Fouls committed", "pressing", "count"),
    row_value("team_match_deep_progressions", "Deep progressions", "threat", "count"),
    row_value("team_match_passes_inside_box", "Box entries", "threat", "count"),
    row_value("team_match_np_xg", "Non-penalty xG", "threat", "share"),
    {
      goals_home <- tms %>%
        dplyr::filter(.data$team_name == home) %>%
        dplyr::pull(.data$team_match_goals)
      goals_away <- tms %>%
        dplyr::filter(.data$team_name == away) %>%
        dplyr::pull(.data$team_match_goals)
      goals_home <- goals_home[[1]] %||% meta$home_score[[1]]
      goals_away <- goals_away[[1]] %||% meta$away_score[[1]]
      goals_total <- goals_home + goals_away
      tibble::tibble(
        section_key = "threat",
        section = SHARE_SECTION_LABELS[["threat"]],
        metric = "Goals",
        home_share = if (goals_total > 0) goals_home / goals_total else 0.5,
        away_share = if (goals_total > 0) goals_away / goals_total else 0.5,
        home_label = format(goals_home, big.mark = ","),
        away_label = format(goals_away, big.mark = ",")
      )
    },
    tibble::tibble(
      section_key = "threat",
      section = SHARE_SECTION_LABELS[["threat"]],
      metric = "Key passes",
      home_share = if (key_total > 0) key_home / key_total else 0.5,
      away_share = if (key_total > 0) key_away / key_total else 0.5,
      home_label = format(key_home, big.mark = ","),
      away_label = format(key_away, big.mark = ",")
    ),
    row_value("team_match_np_shots", "Shots", "threat", "count")
  ) %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      section_key = factor(
        .data$section_key,
        levels = c("circulation", "pressing", "threat")
      )
    )
}

#' Goal markers for the PPDA interval chart (colored ball icons)
compute_ppda_goal_markers <- function(events_df,
                                      meta,
                                      ppda_df,
                                      home_color = SDC_PALETTE[["green"]],
                                      away_color = SDC_PALETTE[["red"]],
                                      root = get_project_root()) {
  data <- ensure_viz_aliases(events_df)
  home <- meta$home_team[[1]]

  goals <- data %>%
    dplyr::filter(
      .data$`type.name` == "Shot",
      .data$`shot.outcome.name` == "Goal",
      !is.na(.data$minute)
    ) %>%
    dplyr::transmute(
      minute = .data$minute,
      team_name = .data$`team.name`,
      interval = factor(
        match_minute_interval(.data$minute),
        levels = MATCH_INTERVAL_LABELS
      ),
      team_color = dplyr::if_else(.data$team_name == home, home_color, away_color)
    )

  if (nrow(goals) == 0) {
    return(goals %>%
      dplyr::mutate(
        x_num = numeric(),
        y = numeric(),
        icon = character(),
        minute_label = character(),
        label_x = numeric(),
        label_y = numeric(),
        label_hjust = numeric()
      ))
  }

  goals %>%
    dplyr::arrange(.data$minute) %>%
    dplyr::mutate(
      position = purrr::pmap(
        list(.data$minute, .data$team_name),
        function(goal_minute, goal_team) {
          interpolate_goal_on_ppda_line(goal_minute, goal_team, ppda_df)
        }
      ),
      x_num = purrr::map_dbl(.data$position, "x_num"),
      y = purrr::map_dbl(.data$position, "y"),
      interval = factor(
        purrr::map_chr(.data$position, "interval"),
        levels = MATCH_INTERVAL_LABELS
      )
    ) %>%
    dplyr::select(-"position") %>%
    dplyr::group_by(.data$interval, .data$team_name) %>%
    dplyr::mutate(
      stack = dplyr::row_number(),
      y = .data$y + (.data$stack - 1) * 0.06
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      icon = purrr::map_chr(.data$team_color, colored_ball_icon_path, root = root),
      minute_label = purrr::map_chr(.data$minute, format_ppda_goal_minute_label),
      label_x = .data$x_num,
      label_hjust = 0.5
    ) %>%
    assign_ppda_goal_label_offsets(home_team = home)
}

#' Panel A — PPDA by 10-minute interval
viz_ppda_interval_line <- function(ppda_df,
                                   events_df = NULL,
                                   meta = NULL,
                                   home_color = SDC_PALETTE[["green"]],
                                   away_color = SDC_PALETTE[["red"]],
                                   root = get_project_root(),
                                   title = "Pressing never really settled",
                                   subtitle = "Average PPDA by 10-minute period") {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  color_map <- stats::setNames(
    c(home_color, away_color),
    c(meta$home_team[[1]], meta$away_team[[1]])
  )

  plot_df <- ppda_df %>%
    dplyr::arrange(.data$team_name, .data$interval) %>%
    dplyr::mutate(x_num = as.numeric(.data$interval))

  goal_markers <- if (!is.null(events_df)) {
    compute_ppda_goal_markers(
      events_df,
      meta,
      ppda_df,
      home_color = home_color,
      away_color = away_color,
      root = root
    )
  } else {
    NULL
  }

  p <- ggplot(plot_df, aes(
    x = .data$x_num,
    y = .data$ppda,
    colour = .data$team_name,
    group = .data$team_name
  )) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.8) +
    scale_colour_manual(
      values = color_map,
      labels = c(meta$display_home[[1]], meta$display_away[[1]]),
      name = NULL
    ) +
    scale_x_continuous(
      breaks = seq_along(MATCH_INTERVAL_LABELS),
      labels = MATCH_INTERVAL_LABELS,
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "PPDA",
      caption = "Lower values = more intense press. Ball icons mark goals on each team's PPDA line."
    ) +
    theme_sdc(base_size = 10) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0, colour = "#555555"),
      plot.caption = element_text(size = 8, hjust = 0, colour = "#666666"),
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5)
    ) +
    coord_cartesian(clip = "off")

  if (!is.null(goal_markers) && nrow(goal_markers) > 0) {
    p <- p +
      ggimage::geom_image(
        data = goal_markers,
        ggplot2::aes(
          x = .data$x_num,
          y = .data$y,
          image = .data$icon
        ),
        inherit.aes = FALSE,
        size = 0.045
      ) +
      ggplot2::geom_text(
        data = goal_markers,
        ggplot2::aes(
          x = .data$label_x,
          y = .data$label_y,
          label = .data$minute_label,
          hjust = .data$label_hjust
        ),
        inherit.aes = FALSE,
        colour = goal_markers$team_color,
        family = SDC_FONTS$body,
        size = 2.4,
        fontface = "bold",
        vjust = 0
      )
  }

  p
}

#' Compact colour-scale legend for game-quality team heatmaps
plot_game_quality_heatmap_legend <- function(home_colors,
                                             away_colors,
                                             limits = c(0, 0.2)) {
  title_style <- legend_title_ggpar()
  label_style <- legend_label_ggpar()
  breaks <- c(0, 0.05, 0.1, 0.15, 0.2)
  breaks <- breaks[breaks <= limits[[2]] + 1e-9]

  cx <- 5
  bar_half <- 4.25
  bar_height <- 0.15
  top_bar_y <- 0.56
  bottom_bar_y <- 0.34
  tick_y <- 0.12

  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(0.04, 0.98), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 6, 2, 6))

  p <- p +
    ggplot2::annotate(
      "text",
      x = cx,
      y = 0.80,
      label = "Share of attacking actions",
      family = title_style$family,
      size = title_style$size * 0.82,
      colour = title_style$colour,
      fontface = title_style$fontface,
      vjust = 0
    )

  p <- heatmap_share_legend_bar(
    p,
    cx = cx,
    heat_colors = home_colors,
    marker_y = top_bar_y,
    bar_half = bar_half,
    bar_height = bar_height
  )
  p <- heatmap_share_legend_bar(
    p,
    cx = cx,
    heat_colors = away_colors,
    marker_y = bottom_bar_y,
    bar_half = bar_half,
    bar_height = bar_height
  )

  tick_df <- tibble::tibble(
    x = cx - bar_half + (breaks - limits[[1]]) / diff(limits) * (2 * bar_half),
    label = scales::percent(breaks, accuracy = 1)
  )

  p +
    ggplot2::geom_text(
      data = tick_df,
      ggplot2::aes(x = .data$x, y = tick_y, label = .data$label),
      family = label_style$family,
      size = label_style$size * 0.86,
      colour = label_style$colour,
      fontface = label_style$fontface,
      vjust = 1
    )
}

#' Compact team attacking heatmap pitch for the game-quality grid
prepare_game_quality_team_heatmap <- function(events_df,
                                              team_name,
                                              heat_color,
                                              match_id = NULL,
                                              n_x_bins = 5,
                                              n_y_bins = 4,
                                              legend_limits = NULL,
                                              show_direction_arrow = TRUE) {
  heatmap_df <- compute_team_attacking_heatmap(
    events_df,
    team_name = team_name,
    match_id = match_id,
    n_x_bins = n_x_bins,
    n_y_bins = n_y_bins,
    normalize_direction = TRUE
  )

  heat_colors <- palette_binned_heatmap(color = heat_color, n = 9)
  legend_max <- max(heatmap_df$share_of_actions, na.rm = TRUE)
  default_limit <- min(0.25, max(0.12, ceiling(legend_max * 100 / 5) * 5 / 100))
  fill_limits <- legend_limits %||% c(0, default_limit)

  pitch_plot <- ggplot(heatmap_df) +
    geom_rect(
      aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$share_of_actions
      ),
      colour = NA,
      alpha = 0.92
    ) +
    draw_pitch_markings(colour = "black", linewidth = 0.4) +
    draw_pitch_outer_border(colour = "black", linewidth = 0.75)

  if (isTRUE(show_direction_arrow)) {
    pitch_plot <- pitch_plot +
      geom_segment(
        data = tibble::tibble(x = 16, xend = 104, y = 83.5, yend = 83.5),
        aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
        arrow = arrow(
          length = unit(0.08, "inches"),
          ends = "last",
          type = "closed"
        ),
        linewidth = 0.35,
        colour = "black",
        inherit.aes = FALSE
      )
  }

  pitch_plot <- pitch_plot +
    scale_fill_gradientn(
      colours = heat_colors,
      limits = fill_limits,
      oob = scales::squish,
      guide = "none"
    ) +
    scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_y_reverse(limits = c(86, 0), expand = c(0, 0)) +
    coord_fixed(ratio = 80 / 120) +
    labs(x = NULL, y = NULL) +
    theme_sdc(base_size = 9) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 0, r = 0, b = 0, l = 6)
    )

  list(
    plot = pitch_plot,
    heat_colors = heat_colors,
    legend_limits = fill_limits
  )
}

#' Panel B — stacked team attacking-action heatmaps
viz_game_quality_attacking_heatmaps <- function(events_df,
                                                meta,
                                                match_id = NULL,
                                                home_color = SDC_PALETTE[["green"]],
                                                away_color = SDC_PALETTE[["red"]],
                                                title = "Where attacks were built",
                                                subtitle = paste(
                                                  "Darker zones = larger share of that team's on-ball",
                                                  "attacking actions (0–20% of total per team)"
                                                )) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  legend_limits <- c(0, 0.2)

  home_prep <- prepare_game_quality_team_heatmap(
    events_df,
    team_name = meta$home_team[[1]],
    heat_color = home_color,
    match_id = match_id,
    legend_limits = legend_limits
  )
  away_prep <- prepare_game_quality_team_heatmap(
    events_df,
    team_name = meta$away_team[[1]],
    heat_color = away_color,
    match_id = match_id,
    legend_limits = legend_limits,
    show_direction_arrow = FALSE
  )
  away_prep$plot <- away_prep$plot +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 6, l = 6))

  legend_plot <- plot_game_quality_heatmap_legend(
    home_colors = home_prep$heat_colors,
    away_colors = away_prep$heat_colors,
    limits = legend_limits
  )

  patchwork::wrap_plots(
    list(home_prep$plot, away_prep$plot, legend_plot),
    ncol = 1,
    heights = c(1, 1, 0.22)
  ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = "Includes passes, carries, ball receipts, dribbles and shots. Arrow = direction of attack.",
      theme = theme(
        plot.title = element_text(
          family = SDC_FONTS$body,
          face = "bold",
          size = 13,
          hjust = 0,
          colour = "#111111"
        ),
        plot.subtitle = element_text(
          family = SDC_FONTS$body,
          size = 10,
          hjust = 0,
          colour = "#555555"
        ),
        plot.caption = element_text(
          family = SDC_FONTS$body,
          size = 7.5,
          hjust = 0,
          colour = "#666666"
        ),
        plot.margin = margin(t = 4, r = 4, b = 2, l = 0)
      )
    )
}

#' Panel B — goals vs non-penalty xG comparison
viz_goals_vs_xg_card <- function(summary_df,
                                 title = "The score outran the chances",
                                 subtitle = "Goals vs non-penalty expected goals",
                                 show_caption = TRUE) {
  plot_df <- summary_df %>%
    tidyr::pivot_longer(
      cols = c("goals", "np_xg"),
      names_to = "stat",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      stat_label = dplyr::if_else(.data$stat == "goals", "Goals", "Non-penalty xG"),
      row_label = paste0(.data$display_team, " — ", .data$stat_label),
      row_label = factor(
        .data$row_label,
        levels = rev(c(
          paste0(summary_df$display_team[[1]], " — Non-penalty xG"),
          paste0(summary_df$display_team[[1]], " — Goals"),
          paste0(summary_df$display_team[[2]], " — Non-penalty xG"),
          paste0(summary_df$display_team[[2]], " — Goals")
        ))
      ),
      bar_alpha = dplyr::if_else(.data$stat == "goals", 1, 0.38)
    )

  delta_labels <- summary_df %>%
    dplyr::mutate(
      label = paste0(
        .data$display_team, ": +",
        format(round(.data$delta, 2), nsmall = 2),
        " above expected"
      )
    )

  caption_text <- if (isTRUE(show_caption)) {
    paste(
      paste(delta_labels$label, collapse = " · "),
      "\nEach row compares goals scored with non-penalty xG."
    )
  } else {
    NULL
  }

  ggplot(plot_df, aes(y = .data$row_label, x = .data$value)) +
    geom_col(
      aes(fill = .data$team_color, alpha = .data$bar_alpha),
      width = 0.62
    ) +
    geom_text(
      aes(
        x = .data$value + 0.06,
        label = format(round(.data$value, 2), nsmall = 2)
      ),
      hjust = 0,
      family = SDC_FONTS$body,
      size = 3.1,
      colour = "#222222",
      fontface = "bold"
    ) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = caption_text
    ) +
    theme_sdc(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 8.5, hjust = 0, colour = "#555555"),
      plot.caption = element_text(size = 7.8, hjust = 0, colour = "#555555"),
      legend.position = "none",
      axis.text.y = element_text(size = 8.5)
    )
}

#' One vertical share panel (100% stacked bars for a single theme)
viz_match_share_section <- function(share_df_section,
                                    section_title,
                                    home_color = SDC_PALETTE[["green"]],
                                    away_color = SDC_PALETTE[["red"]],
                                    display_home = NULL,
                                    display_away = NULL,
                                    bar_half = 0.20,
                                    row_step = 0.72,
                                    value_label_size = 2.5,
                                    metric_label_size = 2.35) {
  n_rows <- nrow(share_df_section)

  share_df_section <- share_df_section %>%
    dplyr::mutate(y = (.env$n_rows - seq_along(.data$metric)) * .env$row_step + 1)

  y_bottom <- 0.45
  y_top <- max(share_df_section$y) + bar_half + 0.55

  ggplot(share_df_section) +
    geom_rect(
      aes(
        xmin = 0,
        xmax = .data$home_share,
        ymin = .data$y - .env$bar_half,
        ymax = .data$y + .env$bar_half
      ),
      fill = home_color,
      colour = NA
    ) +
    geom_rect(
      aes(
        xmin = .data$home_share,
        xmax = 1,
        ymin = .data$y - .env$bar_half,
        ymax = .data$y + .env$bar_half
      ),
      fill = away_color,
      colour = NA
    ) +
    geom_text(
      aes(x = 0.03, y = .data$y, label = .data$home_label),
      hjust = 0,
      family = SDC_FONTS$body,
      fontface = "bold",
      size = value_label_size,
      colour = "white"
    ) +
    geom_text(
      aes(x = 0.97, y = .data$y, label = .data$away_label),
      hjust = 1,
      family = SDC_FONTS$body,
      fontface = "bold",
      size = value_label_size,
      colour = "white"
    ) +
    geom_text(
      aes(x = 0.5, y = .data$y + bar_half + 0.14, label = .data$metric),
      family = SDC_FONTS$body,
      size = metric_label_size,
      colour = "#222222"
    ) +
    annotate(
      "segment",
      x = 0.06,
      xend = 0.94,
      y = y_top - 0.08,
      yend = y_top - 0.08,
      colour = SDC_PALETTE[["blue"]],
      linewidth = 0.35
    ) +
    annotate(
      "text",
      x = 0.5,
      y = y_top + 0.08,
      label = section_title,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 3,
      colour = SDC_PALETTE[["blue"]]
    ) +
    annotate(
      "text",
      x = 0.08,
      y = y_top + 0.34,
      label = display_home,
      hjust = 0,
      family = SDC_FONTS$body,
      fontface = "bold",
      size = 2.4,
      colour = home_color
    ) +
    annotate(
      "text",
      x = 0.92,
      y = y_top + 0.34,
      label = display_away,
      hjust = 1,
      family = SDC_FONTS$body,
      fontface = "bold",
      size = 2.4,
      colour = away_color
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(y_bottom, y_top + 0.42), clip = "off") +
    theme_void() +
    theme(plot.margin = margin(2, 4, 2, 4))
}

#' Three side-by-side share panels for the bottom grid row
viz_match_share_sections_row <- function(share_df,
                                       meta,
                                       home_color = SDC_PALETTE[["green"]],
                                       away_color = SDC_PALETTE[["red"]]) {
  section_keys <- c("circulation", "pressing", "threat")
  panels <- lapply(section_keys, function(key) {
    section_df <- share_df %>%
      dplyr::filter(.data$section_key == key)
    viz_match_share_section(
      section_df,
      section_title = SHARE_SECTION_LABELS[[key]],
      home_color = home_color,
      away_color = away_color,
      display_home = meta$display_home[[1]],
      display_away = meta$display_away[[1]]
    )
  })

  patchwork::wrap_plots(panels, ncol = 3) +
    patchwork::plot_annotation(
      title = "How the game was shared",
      subtitle = "Each row is a 100% share of the combined match total",
      theme = theme(
        plot.title = element_text(
          family = SDC_FONTS$title,
          face = "bold",
          size = 11,
          hjust = 0,
          colour = "#111111"
        ),
        plot.subtitle = element_text(
          family = SDC_FONTS$body,
          size = 8,
          hjust = 0,
          colour = "#555555"
        )
      )
    )
}

#' Assemble Chart 1 game-quality grid
viz_game_quality_grid <- function(events_df,
                                  meta,
                                  team_match_stats_df = NULL,
                                  player_match_stats_df = NULL,
                                  match_id = NULL,
                                  home_color = SDC_PALETTE[["green"]],
                                  away_color = SDC_PALETTE[["red"]],
                                  title = "Six goals, little spark",
                                  subtitle = NULL,
                                  root = get_project_root()) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  match_id <- match_id %||% meta$match_id[[1]]
  if (is.null(subtitle)) {
    subtitle <- match_chart_subtitle(meta)
  }

  quality <- load_match_quality_stats(
    match_id = match_id,
    events = events_df,
    meta = meta,
    team_match_stats_df = team_match_stats_df,
    player_match_stats_df = player_match_stats_df,
    root = root
  )

  tms <- quality$team_match_stats
  pms <- quality$player_match_stats

  ppda_df <- compute_ppda_by_interval(events_df, meta)

  share_df <- compute_match_share_metrics(tms, pms, events_df, meta)

  panel_a <- viz_ppda_interval_line(
    ppda_df,
    events_df = events_df,
    meta = meta,
    home_color = home_color,
    away_color = away_color,
    root = root
  )
  panel_b <- viz_game_quality_attacking_heatmaps(
    events_df,
    meta = meta,
    match_id = match_id,
    home_color = home_color,
    away_color = away_color
  )
  panel_c <- viz_match_share_sections_row(
    share_df,
    meta = meta,
    home_color = home_color,
    away_color = away_color
  )

  grid_body <- patchwork::wrap_plots(
    A = panel_a,
    B = panel_b,
    C = panel_c,
    design = "AAB\nCCC",
    heights = c(0.58, 0.42)
  )

  grid_body +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme_sdc_grid()
    )
}
