#' Build all canonical tables for a Wyscout match (gold CSV + raw JSON)
build_match_wyscout <- function(match_id,
                                data_dir = get_provider_raw_dir("wyscout"),
                                root = get_project_root()) {
  canonical_match_id <- as.integer(match_id)
  provider_match_id <- get_provider_match_id(canonical_match_id, "wyscout", root = root)

  if (is.na(provider_match_id)) {
    stop(
      "No Wyscout ID mapping for canonical match ", canonical_match_id,
      " in game_ids.csv.",
      call. = FALSE
    )
  }

  if (!wyscout_match_data_available(provider_match_id, data_dir = data_dir)) {
    stop(
      "Wyscout gold data not found for match ", provider_match_id,
      " under ", data_dir,
      call. = FALSE
    )
  }

  gold_df <- read_wyscout_gold_csv(provider_match_id, data_dir = data_dir)
  match_json <- read_wyscout_match_json(provider_match_id, data_dir = data_dir)

  lineups <- parse_wyscout_lineups(gold_df, provider_match_id)
  players <- build_wyscout_player_lookup(lineups)
  teams <- build_wyscout_team_lookup(lineups)
  player_match_stats <- parse_wyscout_player_match_stats(gold_df, provider_match_id)
  team_match_stats <- parse_wyscout_team_match_stats(gold_df, provider_match_id)
  events <- empty_wyscout_events()

  display_row <- get_match_display_meta(canonical_match_id, root = root)
  meta <- build_match_meta(canonical_match_id, team_match_stats, display_row)

  if (!is.null(match_json) && !is.null(match_json$label)) {
    meta$match_label <- as.character(match_json$label)
  }

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
