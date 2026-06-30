#' Parse player_match_stats JSON
parse_player_match_stats_json <- function(stats, match_id) {
  if (length(stats) == 0) {
    return(tibble::tibble())
  }

  df <- dplyr::bind_rows(stats)
  df$match_id <- as.integer(match_id)
  df
}

#' Parse team_match_stats JSON
parse_team_match_stats_json <- function(stats, match_id) {
  if (length(stats) == 0) {
    return(tibble::tibble())
  }

  df <- dplyr::bind_rows(stats)
  df$match_id <- as.integer(match_id)
  df
}

#' Minimal team_match_stats when StatsBomb stats JSON is not yet available
synthesize_team_match_stats_from_events <- function(events_df,
                                                    lineups_df,
                                                    match_id,
                                                    display_row = NULL) {
  teams <- lineups_df %>%
    dplyr::distinct(.data$team_id, .data$team_name)

  if (nrow(teams) < 2) {
    teams <- events_df %>%
      dplyr::filter(!is.na(.data$team_id)) %>%
      dplyr::distinct(.data$team_id, .data$team_name)
  }

  if (nrow(teams) < 2) {
    stop("Cannot synthesize team stats: fewer than two teams found.", call. = FALSE)
  }

  if (!is.null(display_row) && !is.null(display_row$pais_1)) {
    home_idx <- match(as.character(display_row$pais_1), teams$team_name)
    if (!is.na(home_idx) && home_idx > 1) {
      teams <- teams[c(home_idx, setdiff(seq_len(nrow(teams)), home_idx)), , drop = FALSE]
    }
  }

  goals <- events_df %>%
    dplyr::filter(
      .data$type_name == "Shot",
      .data$shot_outcome_name == "Goal"
    ) %>%
    dplyr::count(.data$team_id, .data$team_name, name = "team_match_goals")

  team_rows <- teams %>%
    dplyr::left_join(goals, by = c("team_id", "team_name")) %>%
    dplyr::mutate(team_match_goals = dplyr::coalesce(.data$team_match_goals, 0L))

  dplyr::bind_rows(lapply(seq_len(nrow(team_rows)), function(i) {
    opp <- team_rows[if (i == 1) 2 else 1, , drop = FALSE]
    tibble::tibble(
      match_id = as.integer(match_id),
      team_id = team_rows$team_id[i],
      team_name = team_rows$team_name[i],
      opposition_id = opp$team_id,
      opposition_name = opp$team_name,
      competition_name = "FIFA World Cup",
      season_name = "2026",
      team_match_goals = team_rows$team_match_goals[i],
      team_match_goals_conceded = opp$team_match_goals,
      team_match_gd = team_rows$team_match_goals[i] - opp$team_match_goals
    )
  }))
}

#' Minimal player_match_stats when StatsBomb stats JSON is not yet available
synthesize_player_match_stats_from_lineups <- function(lineups_df,
                                                     events_df,
                                                     match_id) {
  minutes <- events_df %>%
    dplyr::filter(!is.na(.data$player_id)) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      player_match_minutes = max(.data$minute, na.rm = TRUE),
      .groups = "drop"
    )

  lineups_df %>%
    dplyr::left_join(minutes, by = "player_id") %>%
    dplyr::transmute(
      match_id = as.integer(match_id),
      team_id = .data$team_id,
      team_name = .data$team_name,
      player_id = .data$player_id,
      player_name = .data$player_name,
      player_match_minutes = dplyr::coalesce(.data$player_match_minutes, 0),
      player_match_goals = dplyr::coalesce(.data$goals, 0L)
    )
}
english_stadium_name <- function(display_row) {
  if (is.null(display_row)) {
    return(NA_character_)
  }

  ciudad <- display_row$Ciudad %||% NA_character_
  if (!is.na(ciudad) && nzchar(trimws(ciudad))) {
    return(paste(trimws(ciudad), "Stadium"))
  }

  estadio <- display_row$Estadio %||% NA_character_
  if (!is.na(estadio) && nzchar(estadio)) {
    venue <- sub("^Estadio\\s+", "", estadio, ignore.case = TRUE)
    return(paste(trimws(venue), "Stadium"))
  }

  NA_character_
}

#' Build match metadata from team stats and optional game_ids row
build_match_meta <- function(match_id,
                           team_match_stats_df,
                           display_row = NULL) {
  winner <- team_match_stats_df %>%
    dplyr::filter(.data$team_match_gd > 0) %>%
    dplyr::slice(1)

  loser <- team_match_stats_df %>%
    dplyr::filter(.data$team_match_gd < 0) %>%
    dplyr::slice(1)

  if (nrow(winner) == 0) {
    home_team <- team_match_stats_df$team_name[1]
    away_team <- team_match_stats_df$opposition_name[1]
    home_score <- team_match_stats_df$team_match_goals[1]
    away_score <- team_match_stats_df$team_match_goals_conceded[1]
  } else {
    home_team <- winner$team_name
    away_team <- loser$team_name
    home_score <- as.integer(winner$team_match_goals)
    away_score <- as.integer(loser$team_match_goals)
  }

  tibble::tibble(
    match_id = as.integer(match_id),
    home_team = home_team,
    away_team = away_team,
    home_score = home_score,
    away_score = away_score,
    competition_name = team_match_stats_df$competition_name[1],
    season_name = team_match_stats_df$season_name[1],
    display_home = if (!is.null(display_row)) display_row$pais_1 else home_team,
    display_away = if (!is.null(display_row)) display_row$pais_2 else away_team,
    stadium = english_stadium_name(display_row),
    match_label = paste(home_team, "vs", away_team)
  )
}
