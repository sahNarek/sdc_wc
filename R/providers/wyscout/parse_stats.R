#' Parse Wyscout gold match.csv into player_match_stats
parse_wyscout_player_match_stats <- function(gold_df, provider_match_id) {
  if (nrow(gold_df) == 0) {
    return(tibble::tibble())
  }

  gold_df %>%
    dplyr::transmute(
      match_id = as.integer(provider_match_id),
      player_id = as.integer(.data$player_id),
      player_name = dplyr::coalesce(
        as.character(.data$player_shortname),
        paste0("Player ", .data$player_id)
      ),
      team_id = as.integer(.data$team_id),
      team_name = as.character(.data$team_name),
      player_match_minutes = as.numeric(.data$minutes_played),
      player_match_goals = as.integer(.data$goals),
      player_match_assists = as.integer(.data$assists),
      is_starter = as.logical(.data$is_starter)
    )
}

#' Build two-row team_match_stats from Wyscout gold match.csv
parse_wyscout_team_match_stats <- function(gold_df, provider_match_id) {
  if (nrow(gold_df) == 0) {
    return(tibble::tibble())
  }

  row <- gold_df[1, ]
  home_id <- as.integer(row$home_team_id)
  away_id <- as.integer(row$away_team_id)
  home_name <- as.character(row$home_team_name)
  away_name <- as.character(row$away_team_name)
  home_goals <- as.integer(row$home_score_ft)
  away_goals <- as.integer(row$away_score_ft)
  competition_name <- "FIFA World Cup"
  season_name <- as.character(row$season_id)

  dplyr::bind_rows(
    tibble::tibble(
      match_id = as.integer(provider_match_id),
      team_id = home_id,
      team_name = home_name,
      opposition_id = away_id,
      opposition_name = away_name,
      team_match_goals = home_goals,
      team_match_goals_conceded = away_goals,
      team_match_gd = home_goals - away_goals,
      competition_name = competition_name,
      season_name = season_name
    ),
    tibble::tibble(
      match_id = as.integer(provider_match_id),
      team_id = away_id,
      team_name = away_name,
      opposition_id = home_id,
      opposition_name = home_name,
      team_match_goals = away_goals,
      team_match_goals_conceded = home_goals,
      team_match_gd = away_goals - home_goals,
      competition_name = competition_name,
      season_name = season_name
    )
  )
}
