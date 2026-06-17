#' Build all data objects for a single StatsBomb match
build_match_statsbomb <- function(match_id,
                                  data_dir = get_provider_raw_dir("statsbomb"),
                                  root = get_project_root()) {
  provider_match_id <- as.integer(match_id)

  if (!match_data_available(provider_match_id, data_dir = data_dir)) {
    stop(
      "No event data found for match ", provider_match_id,
      ". If this is an assigned match, data may not be downloaded yet.",
      call. = FALSE
    )
  }

  events_raw <- read_match_json(provider_match_id, "events.json", data_dir = data_dir)
  lineups_raw <- read_match_json(provider_match_id, "lineups.json", data_dir = data_dir)
  pms_raw <- read_match_json(provider_match_id, "player_match_stats.json", data_dir = data_dir)
  tms_raw <- read_match_json(provider_match_id, "team_match_stats.json", data_dir = data_dir)

  events <- parse_events_json(events_raw, provider_match_id) %>%
    add_statsbomb_aliases()

  lineups <- parse_lineups_json(lineups_raw, provider_match_id)
  players <- build_player_lookup(lineups, events)
  teams <- build_team_lookup(lineups, events)
  events <- apply_name_mapping(events, players, teams)

  player_match_stats <- parse_player_match_stats_json(pms_raw, provider_match_id)
  team_match_stats <- parse_team_match_stats_json(tms_raw, provider_match_id)

  display_row <- get_match_display_meta(provider_match_id, root = root)
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
