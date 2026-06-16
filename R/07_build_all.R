#' Build combined dataset for multiple matches and save to .rda
build_all_matches <- function(match_ids = NULL,
                              data_dir = get_data_sample_dir(),
                              root = get_project_root(),
                              save_path = file.path(get_processed_dir(root), "wc_matches.rda"),
                              include_frames = FALSE) {
  config <- load_match_config(root)

  if (is.null(match_ids)) {
    match_ids <- config$development$sample_match_ids
  }

  available <- vapply(
    match_ids,
    match_data_available,
    FUN.VALUE = logical(1),
    data_dir = data_dir
  )

  pending <- match_ids[!available]
  if (length(pending) > 0) {
    message(
      "Skipping unavailable match IDs: ",
      paste(pending, collapse = ", ")
    )
  }

  match_ids <- match_ids[available]
  if (length(match_ids) == 0) {
    stop("No match data available for the requested IDs.", call. = FALSE)
  }

  built <- lapply(match_ids, build_match, data_dir = data_dir, root = root)

  wc_matches <- list(
    meta = bind_rows(lapply(built, `[[`, "meta")),
    events = bind_rows(lapply(built, `[[`, "events")),
    lineups = bind_rows(lapply(built, `[[`, "lineups")),
    players = bind_rows(lapply(built, `[[`, "players")) %>%
      distinct(player_id, .keep_all = TRUE),
    teams = bind_rows(lapply(built, `[[`, "teams")) %>%
      distinct(team_id, .keep_all = TRUE),
    player_match_stats = bind_rows(lapply(built, `[[`, "player_match_stats")),
    team_match_stats = bind_rows(lapply(built, `[[`, "team_match_stats")),
    config = config,
    built_at = Sys.time()
  )

  if (include_frames) {
    wc_matches$frames_360 <- bind_rows(lapply(match_ids, function(mid) {
      frames_path <- resolve_match_file(mid, "360_frames.json", data_dir = data_dir)
      if (is.null(frames_path)) {
        return(tibble())
      }
      frames <- jsonlite::fromJSON(frames_path, simplifyVector = FALSE)
      tibble(match_id = mid, n_frames = length(frames))
    }))
  }

  save(wc_matches, file = save_path)
  message("Saved ", save_path)
  wc_matches
}
