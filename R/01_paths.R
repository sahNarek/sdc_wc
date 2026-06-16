#' Project root (directory containing config/ and data_sample/)
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

get_data_sample_dir <- function(root = get_project_root()) {
  file.path(root, "data_sample")
}

get_processed_dir <- function(root = get_project_root()) {
  path <- file.path(root, "data", "processed")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

get_figures_dir <- function(match_id, root = get_project_root()) {
  path <- file.path(root, "output", "figures", as.character(match_id))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
