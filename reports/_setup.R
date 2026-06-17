#' Source core, provider, and visualization R scripts
source_project_r <- function(root) {
  core_dir <- file.path(root, "R", "core")
  provider_root <- file.path(root, "R", "providers")

  core_order <- c(
    "paths.R",
    "schema.R",
    "capabilities.R",
    "registry.R",
    "build_all.R",
    "load_match_data.R"
  )
  core_files <- file.path(core_dir, core_order)
  core_files <- core_files[file.exists(core_files)]

  provider_dirs <- sort(list.dirs(provider_root, recursive = FALSE, full.names = TRUE))
  provider_files <- unlist(lapply(provider_dirs, function(dir) {
    sort(list.files(dir, pattern = "\\.R$", full.names = TRUE))
  }))

  # Registry and build_all depend on provider build functions
  pre_registry <- c(
    file.path(core_dir, "paths.R"),
    file.path(core_dir, "schema.R"),
    file.path(core_dir, "capabilities.R"),
    provider_files,
    file.path(core_dir, "registry.R"),
    file.path(core_dir, "build_all.R"),
    file.path(core_dir, "load_match_data.R"),
    file.path(root, "R", "render_report.R")
  )
  pre_registry <- pre_registry[file.exists(pre_registry)]

  viz_dir <- file.path(root, "R", "viz")
  viz_files <- sort(list.files(viz_dir, pattern = "\\.R$", full.names = TRUE))
  viz_files <- viz_files[!grepl("00_packages\\.R$", viz_files)]

  invisible(lapply(c(pre_registry, viz_files), source, local = globalenv()))
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

  if (install_packages) {
    source(file.path(pkg_root, "R", "viz", "00_packages.R"), local = globalenv())
  }

  source_project_r(pkg_root)
  invisible(pkg_root)
}
