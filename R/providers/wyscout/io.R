#' Resolve highest-version Wyscout gold match.csv
#' Ported from all_data/WYSCOUT/gold layout (no Python ETL in repo).
resolve_wyscout_gold_csv <- function(match_id,
                                     data_dir = get_provider_raw_dir("wyscout")) {
  match_dir <- file.path(data_dir, "gold", "matches", as.character(match_id))
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
    candidate <- file.path(match_dir, paste0("v", v), "match.csv")
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  NULL
}

#' Resolve highest-version Wyscout raw JSON file
resolve_wyscout_raw_json <- function(match_id,
                                     file_name,
                                     data_dir = get_provider_raw_dir("wyscout")) {
  match_dir <- file.path(data_dir, "raw", "matches", as.character(match_id))
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

read_wyscout_gold_csv <- function(match_id,
                                  data_dir = get_provider_raw_dir("wyscout")) {
  path <- resolve_wyscout_gold_csv(match_id, data_dir = data_dir)
  if (is.null(path)) {
    stop(
      "Missing Wyscout gold match.csv for match ", match_id,
      " under ", data_dir, call. = FALSE
    )
  }

  readr::read_csv(path, show_col_types = FALSE)
}

read_wyscout_match_json <- function(match_id,
                                    data_dir = get_provider_raw_dir("wyscout")) {
  path <- resolve_wyscout_raw_json(match_id, "match.json", data_dir = data_dir)
  if (is.null(path)) {
    return(NULL)
  }

  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

wyscout_match_data_available <- function(match_id,
                                         data_dir = get_provider_raw_dir("wyscout")) {
  !is.null(resolve_wyscout_gold_csv(match_id, data_dir = data_dir))
}

list_wyscout_match_ids <- function(data_dir = get_provider_raw_dir("wyscout")) {
  match_root <- file.path(data_dir, "gold", "matches")
  if (!dir.exists(match_root)) {
    return(integer(0))
  }

  ids <- list.dirs(match_root, full.names = FALSE, recursive = FALSE)
  ids <- ids[nzchar(ids)]
  sort(as.integer(ids))
}
