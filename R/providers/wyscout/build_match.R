#' Check whether Wyscout raw data exists for a match (scaffold)
wyscout_match_data_available <- function(match_id, data_dir = get_provider_raw_dir("wyscout")) {
  match_dir <- file.path(data_dir, "matches", as.character(match_id))
  dir.exists(match_dir) &&
    length(list.files(match_dir, recursive = TRUE)) > 0
}

#' Build all data objects for a single Wyscout match (not implemented)
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
      "Wyscout raw data not found for match ", provider_match_id,
      " under ", data_dir,
      call. = FALSE
    )
  }

  stop("Wyscout ingestion is not implemented yet.", call. = FALSE)
}
