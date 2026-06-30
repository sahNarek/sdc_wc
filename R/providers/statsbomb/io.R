#' List available match IDs under provider raw data
list_available_match_ids <- function(provider = "statsbomb",
                                     data_dir = NULL,
                                     root = get_project_root()) {
  if (!is.null(data_dir)) {
    data_dirs <- data_dir
  } else if (provider == "statsbomb") {
    data_dirs <- statsbomb_raw_dirs(root)
  } else {
    data_dirs <- get_provider_raw_dir(provider, root = root)
  }

  ids <- integer(0)
  for (dir in data_dirs) {
    match_root <- file.path(dir, "matches")
    if (!dir.exists(match_root)) {
      next
    }
    found <- list.dirs(match_root, full.names = FALSE, recursive = FALSE)
    found <- found[nzchar(found)]
    ids <- c(ids, as.integer(found))
  }

  sort(unique(ids[!is.na(ids)]))
}

#' StatsBomb raw root that contains a match's event data
resolve_statsbomb_data_dir <- function(match_id, root = get_project_root()) {
  match_id <- as.integer(match_id)
  for (dir in statsbomb_raw_dirs(root)) {
    if (!is.null(resolve_match_file(match_id, "events.json", data_dir = dir))) {
      return(dir)
    }
  }
  NULL
}

#' Resolve highest version folder for a match file
resolve_match_file <- function(match_id,
                               file_name,
                               data_dir = NULL,
                               root = get_project_root()) {
  data_dirs <- if (is.null(data_dir)) {
    statsbomb_raw_dirs(root)
  } else {
    data_dir
  }

  for (dir in data_dirs) {
    match_dir <- file.path(dir, "matches", as.character(match_id))
    if (!dir.exists(match_dir)) {
      next
    }

    versions <- list.dirs(match_dir, full.names = FALSE, recursive = FALSE)
    versions <- versions[grepl("^v[0-9]+$", versions)]
    if (length(versions) == 0) {
      next
    }

    version_nums <- as.integer(sub("^v", "", versions))
    for (v in sort(version_nums, decreasing = TRUE)) {
      candidate <- file.path(match_dir, paste0("v", v), file_name)
      if (file.exists(candidate)) {
        return(candidate)
      }
    }
  }

  NULL
}

#' Read a match JSON file (events, lineups, stats, etc.)
read_match_json <- function(match_id,
                            file_name,
                            data_dir = NULL,
                            root = get_project_root()) {
  if (is.null(data_dir)) {
    data_dir <- resolve_statsbomb_data_dir(match_id, root = root)
  }
  path <- resolve_match_file(match_id, file_name, data_dir = data_dir, root = root)
  if (is.null(path)) {
    stop(
      "Missing ", file_name, " for match ", match_id,
      if (!is.null(data_dir)) paste0(" under ", data_dir) else "",
      call. = FALSE
    )
  }

  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Read a match JSON file when present; return empty list if missing
read_match_json_optional <- function(match_id,
                                   file_name,
                                   data_dir = NULL,
                                   root = get_project_root()) {
  if (is.null(data_dir)) {
    data_dir <- resolve_statsbomb_data_dir(match_id, root = root)
  }
  path <- resolve_match_file(match_id, file_name, data_dir = data_dir, root = root)
  if (is.null(path)) {
    return(list())
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

match_data_available <- function(match_id,
                                 data_dir = NULL,
                                 root = get_project_root()) {
  !is.null(resolve_match_file(match_id, "events.json", data_dir = data_dir, root = root))
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
    dplyr::filter(as.character(.data[["Statsbomb ID"]]) == as.character(match_id))

  if (nrow(row) == 0) {
    return(NULL)
  }

  row[1, ]
}
