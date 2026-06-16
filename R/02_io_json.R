#' List available match IDs under data_sample/matches/
list_available_match_ids <- function(data_dir = get_data_sample_dir()) {
  match_root <- file.path(data_dir, "matches")
  if (!dir.exists(match_root)) {
    return(integer(0))
  }

  ids <- list.dirs(match_root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  sort(as.integer(ids))
}

#' Resolve highest version folder for a match file
resolve_match_file <- function(match_id,
                               file_name,
                               data_dir = get_data_sample_dir()) {
  match_dir <- file.path(data_dir, "matches", as.character(match_id))
  if (!dir.exists(match_dir)) {
    return(NULL)
  }

  versions <- list.dirs(match_dir, full.names = FALSE, recursive = FALSE)
  versions <- versions[grepl("^v[0-9]+$", versions)]
  if (length(versions) == 0) {
    return(NULL)
  }

  version_nums <- as.integer(sub("^v", "", versions))
  for (v in sort(version_nums, decreasing = TRUE)) {
    candidate <- file.path(match_dir, paste0("v", v), file_name)
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  NULL
}

#' Read a match JSON file (events, lineups, stats, etc.)
read_match_json <- function(match_id,
                            file_name,
                            data_dir = get_data_sample_dir()) {
  path <- resolve_match_file(match_id, file_name, data_dir = data_dir)
  if (is.null(path)) {
    stop(
      "Missing ", file_name, " for match ", match_id,
      " under ", data_dir, call. = FALSE
    )
  }

  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

match_data_available <- function(match_id, data_dir = get_data_sample_dir()) {
  !is.null(resolve_match_file(match_id, "events.json", data_dir = data_dir))
}

read_game_ids <- function(root = get_project_root()) {
  path <- file.path(root, "game_ids.csv")
  if (!file.exists(path)) {
    return(NULL)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

get_match_display_meta <- function(match_id, root = get_project_root()) {
  game_ids <- read_game_ids(root)
  if (is.null(game_ids)) {
    return(NULL)
  }

  row <- game_ids %>%
    filter(as.character(`Statsbomb ID`) == as.character(match_id))

  if (nrow(row) == 0) {
    return(NULL)
  }

  row[1, ]
}
