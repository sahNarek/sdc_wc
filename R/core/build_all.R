#' Build combined dataset for one provider and save to data/processed/{provider}/
build_all_matches <- function(match_ids = NULL,
                              provider = "statsbomb",
                              data_dir = NULL,
                              root = get_project_root(),
                              save_path = NULL,
                              include_frames = FALSE) {
  registry <- get_provider_registry()
  if (!provider %in% names(registry)) {
    stop("Unknown provider: ", provider, call. = FALSE)
  }

  build_fn <- registry[[provider]]
  config <- load_match_config(root)

  if (is.null(match_ids)) {
    match_ids <- config$development$sample_match_ids
  }

  if (is.null(data_dir)) {
    data_dir <- get_provider_raw_dir(provider, root = root)
  }

  if (is.null(save_path)) {
    save_path <- get_provider_processed_path(provider, root)
  }

  available <- vapply(
    match_ids,
    provider_data_available,
    FUN.VALUE = logical(1),
    provider = provider,
    root = root
  )

  pending <- match_ids[!available]
  if (length(pending) > 0) {
    message(
      "Skipping unavailable match IDs for ", provider, ": ",
      paste(pending, collapse = ", ")
    )
  }

  match_ids <- match_ids[available]
  if (length(match_ids) == 0) {
    stop(
      "No match data available for provider ", provider,
      " and the requested IDs.",
      call. = FALSE
    )
  }

  built <- lapply(match_ids, function(mid) {
    bundle <- build_fn(
      match_id = mid,
      data_dir = data_dir,
      root = root
    )
    normalize_match_bundle(
      bundle,
      provider = provider,
      canonical_match_id = mid
    )
  })

  wc_matches <- list(
    meta = dplyr::bind_rows(lapply(built, `[[`, "meta")),
    events = dplyr::bind_rows(lapply(built, `[[`, "events")),
    lineups = dplyr::bind_rows(lapply(built, `[[`, "lineups")),
    players = dplyr::bind_rows(lapply(built, `[[`, "players")) %>%
      dplyr::distinct(.data$player_id, .keep_all = TRUE),
    teams = dplyr::bind_rows(lapply(built, `[[`, "teams")) %>%
      dplyr::distinct(.data$team_id, .keep_all = TRUE),
    player_match_stats = dplyr::bind_rows(lapply(built, `[[`, "player_match_stats")),
    team_match_stats = dplyr::bind_rows(lapply(built, `[[`, "team_match_stats")),
    provider = provider,
    config = config,
    built_at = Sys.time()
  )

  if (include_frames && provider == "statsbomb") {
    wc_matches$frames_360 <- dplyr::bind_rows(lapply(match_ids, function(mid) {
      frames_path <- resolve_match_file(
        mid,
        "360_frames.json",
        data_dir = data_dir
      )
      if (is.null(frames_path)) {
        return(tibble::tibble())
      }
      frames <- jsonlite::fromJSON(frames_path, simplifyVector = FALSE)
      tibble::tibble(match_id = mid, n_frames = length(frames))
    }))
  }

  save(wc_matches, file = save_path)
  message("Saved ", save_path)
  wc_matches
}
