#' Build all data objects for a single StatsBomb match
build_match_statsbomb <- function(match_id,
                                  data_dir = NULL,
                                  root = get_project_root()) {
  provider_match_id <- as.integer(match_id)

  if (is.null(data_dir) || !match_data_available(provider_match_id, data_dir = data_dir, root = root)) {
    data_dir <- resolve_statsbomb_data_dir(provider_match_id, root = root)
  }

  if (is.null(data_dir) || !match_data_available(provider_match_id, data_dir = data_dir, root = root)) {
    stop(
      "No event data found for match ", provider_match_id,
      ". If this is an assigned match, data may not be downloaded yet.",
      call. = FALSE
    )
  }

  events_raw <- read_match_json(provider_match_id, "events.json", data_dir = data_dir, root = root)
  lineups_raw <- read_match_json_optional(provider_match_id, "lineups.json", data_dir = data_dir, root = root)
  pms_raw <- read_match_json_optional(provider_match_id, "player_match_stats.json", data_dir = data_dir, root = root)
  tms_raw <- read_match_json_optional(provider_match_id, "team_match_stats.json", data_dir = data_dir, root = root)

  events <- parse_events_json(events_raw, provider_match_id) %>%
    add_statsbomb_aliases()

  lineups <- if (length(lineups_raw) > 0) {
    parse_lineups_json(lineups_raw, provider_match_id)
  } else {
    tibble::tibble()
  }
  players <- build_player_lookup(lineups, events)
  teams <- build_team_lookup(lineups, events)
  events <- apply_name_mapping(events, players, teams)

  display_row <- get_match_display_meta(provider_match_id, root = root)

  player_match_stats <- if (length(pms_raw) > 0) {
    parse_player_match_stats_json(pms_raw, provider_match_id)
  } else {
    message(
      "Synthesizing player_match_stats from lineups/events for match ",
      provider_match_id
    )
    synthesize_player_match_stats_from_lineups(lineups, events, provider_match_id)
  }

  team_match_stats <- if (length(tms_raw) > 0) {
    parse_team_match_stats_json(tms_raw, provider_match_id)
  } else {
    message(
      "Synthesizing team_match_stats from events for match ",
      provider_match_id
    )
    synthesize_team_match_stats_from_events(
      events,
      lineups,
      provider_match_id,
      display_row = display_row
    )
  }

  meta <- build_match_meta(provider_match_id, team_match_stats, display_row)

  list(
    provider_match_id = provider_match_id,
    meta = meta,
    events = events,
    lineups = lineups,
    players = players,
    teams = teams,
    player_match_stats = player_match_stats,
    team_match_stats = team_match_stats
  )
}
