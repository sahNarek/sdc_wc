#' Build all data objects for a single match
build_match <- function(match_id, data_dir = get_data_sample_dir(), root = get_project_root()) {
  if (!match_data_available(match_id, data_dir = data_dir)) {
    stop(
      "No event data found for match ", match_id,
      ". If this is an assigned match, data may not be downloaded yet.",
      call. = FALSE
    )
  }

  events_raw <- read_match_json(match_id, "events.json", data_dir = data_dir)
  lineups_raw <- read_match_json(match_id, "lineups.json", data_dir = data_dir)
  pms_raw <- read_match_json(match_id, "player_match_stats.json", data_dir = data_dir)
  tms_raw <- read_match_json(match_id, "team_match_stats.json", data_dir = data_dir)

  events <- parse_events_json(events_raw, match_id) %>%
    add_statsbomb_aliases()

  lineups <- parse_lineups_json(lineups_raw, match_id)
  players <- build_player_lookup(lineups, events)
  teams <- build_team_lookup(lineups, events)
  events <- apply_name_mapping(events, players, teams)

  player_match_stats <- parse_player_match_stats_json(pms_raw, match_id)
  team_match_stats <- parse_team_match_stats_json(tms_raw, match_id)

  display_row <- get_match_display_meta(match_id, root = root)
  meta <- build_match_meta(match_id, team_match_stats, display_row)

  list(
    meta = meta,
    events = events,
    lineups = lineups,
    players = players,
    teams = teams,
    player_match_stats = player_match_stats,
    team_match_stats = team_match_stats
  )
}
