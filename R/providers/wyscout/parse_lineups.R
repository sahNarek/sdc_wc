#' Parse Wyscout gold match.csv into lineups (one row per player)
parse_wyscout_lineups <- function(gold_df, provider_match_id) {
  if (nrow(gold_df) == 0) {
    return(tibble::tibble())
  }

  gold_df %>%
    dplyr::transmute(
      match_id = as.integer(provider_match_id),
      team_id = as.integer(.data$team_id),
      team_name = as.character(.data$team_name),
      player_id = as.integer(.data$player_id),
      player_name = dplyr::coalesce(
        as.character(.data$player_shortname),
        paste0("Player ", .data$player_id)
      ),
      player_nickname = as.character(.data$player_shortname),
      jersey_number = as.integer(.data$shirt_number),
      primary_position = as.character(.data$tactical_position),
      goals = as.integer(.data$goals),
      assists = as.integer(.data$assists)
    )
}

#' Build player lookup from Wyscout lineups
build_wyscout_player_lookup <- function(lineups_df) {
  lineups_df %>%
    dplyr::transmute(
      player_id,
      player_name,
      player_nickname,
      player_display_name = dplyr::coalesce(
        dplyr::if_else(
          !is.na(.data$player_nickname) & nzchar(.data$player_nickname),
          .data$player_nickname,
          NA_character_
        ),
        .data$player_name
      ),
      team_id,
      team_name
    ) %>%
    dplyr::distinct(.data$player_id, .keep_all = TRUE)
}

#' Build team lookup from Wyscout lineups
build_wyscout_team_lookup <- function(lineups_df) {
  lineups_df %>%
    dplyr::distinct(.data$team_id, .data$team_name)
}
