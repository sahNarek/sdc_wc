#' Source data-layer and visualization-layer R scripts
source_project_r <- function(root) {
  data_dir <- file.path(root, "R")
  viz_dir <- file.path(root, "R", "viz")

  data_files <- sort(list.files(data_dir, pattern = "\\.R$", full.names = TRUE))
  viz_files <- sort(list.files(viz_dir, pattern = "\\.R$", full.names = TRUE))
  viz_files <- viz_files[!grepl("00_packages\\.R$", viz_files)]

  paths_first <- data_files[grepl("01_paths\\.R$", data_files)]
  other_data <- data_files[!grepl("01_paths\\.R$", data_files)]

  invisible(lapply(c(paths_first, other_data, viz_files), source, local = globalenv()))
}

load_project <- function(install_packages = TRUE, root = NULL) {
  if (!is.null(root)) {
    Sys.setenv(SDC_WC_ROOT = normalizePath(root, mustWork = FALSE))
  }

  pkg_root <- Sys.getenv("SDC_WC_ROOT")
  if (!nzchar(pkg_root)) {
    pkg_root <- if (file.exists("sdc_wc.Rproj")) {
      normalizePath(getwd(), mustWork = FALSE)
    } else {
      normalizePath("..", mustWork = FALSE)
    }
    Sys.setenv(SDC_WC_ROOT = pkg_root)
  }

  source(file.path(pkg_root, "R", "01_paths.R"), local = globalenv())

  if (install_packages) {
    source(file.path(pkg_root, "R", "viz", "00_packages.R"), local = globalenv())
  }

  source_project_r(pkg_root)
  invisible(pkg_root)
}
