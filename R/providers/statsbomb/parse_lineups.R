#' Parse lineups JSON into long-format dataframe
parse_lineups_json <- function(lineups, match_id) {
  if (length(lineups) == 0) {
    return(tibble::tibble())
  }

  rows <- lapply(lineups, function(team_block) {
    team_id <- as.integer(team_block$team_id)
    team_name <- as.character(team_block$team_name)

    lapply(team_block$lineup, function(player_row) {
      positions <- player_row$positions
      primary_position <- if (length(positions) > 0) {
        as.character(positions[[1]]$position)
      } else {
        NA_character_
      }

      tibble::tibble(
        match_id = as.integer(match_id),
        team_id = team_id,
        team_name = team_name,
        player_id = as.integer(player_row$player_id),
        player_name = as.character(player_row$player_name),
        player_nickname = as.character(player_row$player_nickname),
        jersey_number = as.integer(player_row$jersey_number),
        primary_position = primary_position,
        goals = as.integer(player_row$stats$goals),
        assists = as.integer(player_row$stats$assists)
      )
    })
  })

  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

#' Build deduplicated player lookup table
build_player_lookup <- function(lineups_df, events_df = NULL) {
  players <- lineups_df %>%
    dplyr::transmute(
      player_id,
      player_name,
      player_nickname,
      team_id,
      team_name
    ) %>%
    dplyr::distinct(.data$player_id, .keep_all = TRUE) %>%
    dplyr::mutate(
      player_display_name = dplyr::if_else(
        !is.na(.data$player_nickname) &
          .data$player_nickname != "NULL" &
          nzchar(.data$player_nickname),
        .data$player_nickname,
        .data$player_name
      )
    )

  if (!is.null(events_df) && nrow(events_df) > 0) {
    event_players <- events_df %>%
      dplyr::filter(!is.na(.data$player_id)) %>%
      dplyr::transmute(
        player_id = as.integer(.data$player_id),
        player_name = .data$player_name,
        team_id = as.integer(.data$team_id),
        team_name = .data$team_name
      ) %>%
      dplyr::distinct(.data$player_id, .keep_all = TRUE)

    players <- players %>%
      dplyr::full_join(event_players, by = "player_id", suffix = c("", "_event")) %>%
      dplyr::mutate(
        player_name = dplyr::coalesce(.data$player_name, .data$player_name_event),
        team_id = dplyr::coalesce(.data$team_id, .data$team_id_event),
        team_name = dplyr::coalesce(.data$team_name, .data$team_name_event),
        player_nickname = dplyr::if_else(
          is.na(.data$player_nickname) | .data$player_nickname == "NULL",
          NA_character_,
          .data$player_nickname
        ),
        player_display_name = dplyr::coalesce(
          dplyr::if_else(
            !is.na(.data$player_nickname) & nzchar(.data$player_nickname),
            .data$player_nickname,
            NA_character_
          ),
          .data$player_name
        )
      ) %>%
      dplyr::select(
        .data$player_id,
        .data$player_name,
        .data$player_nickname,
        .data$player_display_name,
        .data$team_id,
        .data$team_name
      )
  }

  players
}

#' Build team lookup table
build_team_lookup <- function(lineups_df, events_df = NULL) {
  teams <- lineups_df %>%
    dplyr::transmute(team_id, team_name) %>%
    dplyr::distinct()

  if (!is.null(events_df) && nrow(events_df) > 0) {
    event_teams <- events_df %>%
      dplyr::filter(!is.na(.data$team_id)) %>%
      dplyr::transmute(
        team_id = as.integer(.data$team_id),
        team_name = .data$team_name
      ) %>%
      dplyr::distinct()

    teams <- dplyr::bind_rows(teams, event_teams) %>%
      dplyr::distinct(.data$team_id, .keep_all = TRUE)
  }

  teams
}

#' Apply consistent display names to events
apply_name_mapping <- function(events_df, players, teams) {
  events_df %>%
    dplyr::left_join(
      players %>% dplyr::select(.data$player_id, .data$player_display_name),
      by = "player_id"
    ) %>%
    dplyr::left_join(
      teams %>% dplyr::rename(team_name_mapped = .data$team_name),
      by = "team_id"
    ) %>%
    dplyr::mutate(
      player_display_name = dplyr::coalesce(.data$player_display_name, .data$player_name),
      team_name = dplyr::coalesce(.data$team_name_mapped, .data$team_name)
    ) %>%
    dplyr::select(-.data$team_name_mapped)
}
