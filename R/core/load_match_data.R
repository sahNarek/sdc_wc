#' Check whether raw data exists for a provider/match combination
provider_data_available <- function(match_id,
                                    provider = "statsbomb",
                                    root = get_project_root()) {
  registry <- get_provider_registry()
  if (!provider %in% names(registry)) {
    return(FALSE)
  }

  canonical_id <- as.integer(match_id)
  provider_match_id <- get_provider_match_id(canonical_id, provider, root = root)
  if (is.na(provider_match_id)) {
    return(FALSE)
  }

  data_dir <- get_provider_raw_dir(provider, root = root)

  switch(
    provider,
    statsbomb = !is.null(
      resolve_match_file(provider_match_id, "events.json", data_dir = data_dir)
    ),
    wyscout = wyscout_match_data_available(provider_match_id, data_dir = data_dir),
    FALSE
  )
}

#' Load processed wc_matches.rda for a provider (with legacy path fallback)
load_processed_matches <- function(provider = "statsbomb", root = get_project_root()) {
  path <- get_provider_processed_path(provider, root)
  if (!file.exists(path) && provider == "statsbomb") {
    legacy <- get_legacy_processed_path(root)
    if (file.exists(legacy)) {
      path <- legacy
    }
  }

  if (!file.exists(path)) {
    stop(
      "Processed data not found for provider ", provider, ": ", path,
      ". Run Rscript scripts/run_build.R ", provider,
      call. = FALSE
    )
  }

  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  wc_matches <- env$wc_matches
  if (is.null(wc_matches$provider)) {
    wc_matches$provider <- provider
  }
  wc_matches
}

#' Load canonical match tables for one provider (viz/report adapter)
load_match_data <- function(provider,
                            match_id,
                            root = get_project_root(),
                            build_if_missing = FALSE) {
  match_id <- as.integer(match_id)
  path <- get_provider_processed_path(provider, root)

  if (!file.exists(path) && build_if_missing) {
    build_all_matches(match_ids = match_id, provider = provider, root = root)
  }

  wc_matches <- load_processed_matches(provider, root = root)

  mid <- match_id
  list(
    provider = provider,
    match_id = match_id,
    meta = wc_matches$meta %>% dplyr::filter(.data$match_id == .env$mid),
    events = wc_matches$events %>% dplyr::filter(.data$match_id == .env$mid),
    lineups = wc_matches$lineups %>% dplyr::filter(.data$match_id == .env$mid),
    players = wc_matches$players,
    teams = wc_matches$teams,
    player_match_stats = wc_matches$player_match_stats %>%
      dplyr::filter(.data$match_id == .env$mid),
    team_match_stats = wc_matches$team_match_stats %>%
      dplyr::filter(.data$match_id == .env$mid)
  )
}
