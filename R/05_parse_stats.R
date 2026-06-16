#' Parse player_match_stats JSON
parse_player_match_stats_json <- function(stats, match_id) {
  if (length(stats) == 0) {
    return(tibble())
  }

  df <- bind_rows(stats)
  df$match_id <- as.integer(match_id)
  df
}

#' Parse team_match_stats JSON
parse_team_match_stats_json <- function(stats, match_id) {
  if (length(stats) == 0) {
    return(tibble())
  }

  df <- bind_rows(stats)
  df$match_id <- as.integer(match_id)
  df
}

#' Build match metadata from team stats and optional game_ids row
build_match_meta <- function(match_id,
                           team_match_stats_df,
                           display_row = NULL) {
  winner <- team_match_stats_df %>%
    filter(team_match_gd > 0) %>%
    slice(1)

  loser <- team_match_stats_df %>%
    filter(team_match_gd < 0) %>%
    slice(1)

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

  tibble(
    match_id = as.integer(match_id),
    home_team = home_team,
    away_team = away_team,
    home_score = home_score,
    away_score = away_score,
    competition_name = team_match_stats_df$competition_name[1],
    season_name = team_match_stats_df$season_name[1],
    display_home = if (!is.null(display_row)) display_row$pais_1 else home_team,
    display_away = if (!is.null(display_row)) display_row$pais_2 else away_team,
    stadium = if (!is.null(display_row)) display_row$Estadio else NA_character_,
    match_label = paste(home_team, "vs", away_team)
  )
}
