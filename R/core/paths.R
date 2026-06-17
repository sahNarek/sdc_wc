`%||%` <- function(x, y) if (is.null(x)) y else x

#' Project root (directory containing config/ and sdc_wc.Rproj)
get_project_root <- function() {
  env_root <- Sys.getenv("SDC_WC_ROOT")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, mustWork = FALSE))
  }

  cwd <- normalizePath(getwd(), mustWork = FALSE)
  candidates <- unique(c(
    cwd,
    dirname(cwd),
    file.path(cwd, ".."),
    file.path(dirname(cwd), "..")
  ))

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "sdc_wc.Rproj"))) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }

  cwd
}

load_match_config <- function(root = get_project_root()) {
  yaml::read_yaml(file.path(root, "config", "matches.yml"))
}

load_providers_config <- function(root = get_project_root()) {
  yaml::read_yaml(file.path(root, "config", "providers.yml"))
}

#' Local multi-provider bundle (all_data/ or all_data with trailing space)
get_all_data_dir <- function(root = get_project_root()) {
  for (name in c("all_data", "all_data ")) {
    path <- file.path(root, name)
    if (dir.exists(path)) {
      return(path)
    }
  }
  file.path(root, "all_data")
}

#' All StatsBomb raw roots, highest priority first (all_data bundle, then legacy)
statsbomb_raw_dirs <- function(root = get_project_root()) {
  dirs <- character(0)

  bundled <- file.path(get_all_data_dir(root), "STATSBOMB", "raw")
  if (dir.exists(file.path(bundled, "matches"))) {
    dirs <- c(dirs, bundled)
  }

  legacy <- file.path(root, "data_sample")
  if (dir.exists(file.path(legacy, "matches"))) {
    dirs <- c(dirs, legacy)
  }

  fallback <- file.path(root, "data", "raw", "statsbomb")
  if (dir.exists(file.path(fallback, "matches"))) {
    dirs <- c(dirs, fallback)
  }

  unique(dirs)
}

#' Primary StatsBomb raw directory (first root that exists)
get_provider_raw_dir <- function(provider, root = get_project_root()) {
  if (provider == "statsbomb") {
    dirs <- statsbomb_raw_dirs(root)
    if (length(dirs) > 0) {
      return(dirs[[1]])
    }
  }

  if (provider == "wyscout") {
    bundled <- file.path(get_all_data_dir(root), "WYSCOUT")
    if (dir.exists(file.path(bundled, "gold", "matches"))) {
      return(bundled)
    }
  }

  file.path(root, "data", "raw", provider)
}

#' @deprecated Use get_provider_raw_dir("statsbomb") — kept for compatibility
get_data_sample_dir <- function(root = get_project_root()) {
  get_provider_raw_dir("statsbomb", root = root)
}

get_processed_dir <- function(root = get_project_root()) {
  path <- file.path(root, "data", "processed")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

get_provider_processed_dir <- function(provider, root = get_project_root()) {
  path <- file.path(get_processed_dir(root), provider)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

get_provider_processed_path <- function(provider, root = get_project_root()) {
  file.path(get_provider_processed_dir(provider, root), "wc_matches.rda")
}

get_legacy_processed_path <- function(root = get_project_root()) {
  file.path(get_processed_dir(root), "wc_matches.rda")
}

get_figures_dir <- function(match_id,
                            provider = NULL,
                            root = get_project_root()) {
  base <- file.path(root, "output", "figures", as.character(match_id))
  path <- if (!is.null(provider) && nzchar(provider)) {
    file.path(base, provider)
  } else {
    base
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

enabled_providers <- function(root = get_project_root()) {
  cfg <- load_providers_config(root)
  names(cfg$providers)[vapply(cfg$providers, function(x) isTRUE(x$enabled), logical(1))]
}

provider_section_label <- function(provider, root = get_project_root()) {
  cfg <- load_providers_config(root)
  entry <- cfg$providers[[provider]]
  if (!is.null(entry) && !is.null(entry$display_label) && nzchar(entry$display_label)) {
    return(entry$display_label)
  }
  provider
}

parse_providers_arg <- function(providers) {
  if (is.null(providers) || length(providers) == 0) {
    return("statsbomb")
  }

  if (length(providers) == 1 && is.character(providers)) {
    if (tolower(providers) == "all") {
      return("all")
    }
    if (grepl(",", providers, fixed = TRUE)) {
      providers <- strsplit(providers, ",", fixed = TRUE)[[1]]
    }
  }

  unique(trimws(as.character(providers)))
}

resolve_providers_for_match <- function(providers,
                                        match_id,
                                        root = get_project_root()) {
  parsed <- parse_providers_arg(providers)
  if (length(parsed) == 1 && parsed == "all") {
    parsed <- enabled_providers(root)
  }

  unknown <- setdiff(parsed, enabled_providers(root))
  if (length(unknown) > 0) {
    stop(
      "Unknown provider(s): ", paste(unknown, collapse = ", "),
      ". See config/providers.yml.",
      call. = FALSE
    )
  }

  available <- parsed[vapply(
    parsed,
    provider_data_available,
    logical(1),
    match_id = match_id,
    root = root
  )]

  missing <- setdiff(parsed, available)
  if (length(missing) > 0) {
    message(
      "Skipping provider(s) with no data for match ", match_id, ": ",
      paste(missing, collapse = ", ")
    )
  }

  available
}

get_provider_match_id <- function(canonical_match_id,
                                  provider,
                                  root = get_project_root()) {
  canonical_match_id <- as.integer(canonical_match_id)
  if (provider == "statsbomb") {
    return(canonical_match_id)
  }

  game_ids <- read_game_ids(root)
  if (is.null(game_ids)) {
    return(NA_integer_)
  }

  row <- game_ids %>%
    dplyr::filter(as.character(.data[["Statsbomb ID"]]) == as.character(canonical_match_id))

  if (nrow(row) == 0) {
    return(NA_integer_)
  }

  col <- switch(
    provider,
    wyscout = "Wyscout ID",
    stop("No ID mapping for provider: ", provider, call. = FALSE)
  )

  as.integer(row[[col]][1])
}
