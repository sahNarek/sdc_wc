#!/usr/bin/env Rscript
# One-time / repeatable local environment setup for sdc_wc.
#
# Usage (from project root):
#   Rscript scripts/setup_local.R
#   Rscript scripts/setup_local.R --pdf    # also install TinyTeX for PDF reports
#   Rscript scripts/setup_local.R --check  # verify only, do not install

args <- commandArgs(trailingOnly = TRUE)
install_pdf <- "--pdf" %in% args
check_only <- "--check" %in% args

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

message("=== SDC World Cup 2026 — local setup ===")
message("Project root: ", root)

# --- R version ---
r_version <- getRversion()
message("\n[1/6] R version: ", as.character(r_version))
if (r_version < "4.2.0") {
  stop(
    "R >= 4.2.0 is required. Install from https://cran.r-project.org/",
    call. = FALSE
  )
}

# --- System tools (informational) ---
message("\n[2/6] System dependencies (install via OS package manager if charts fail):")
sys_checks <- list(
  "ImageMagick (magick icons)" = nzchar(Sys.which("magick")),
  "rsvg-convert (optional)" = nzchar(Sys.which("rsvg-convert")),
  "pandoc (rmarkdown)" = nzchar(Sys.which("pandoc"))
)
for (label in names(sys_checks)) {
  status <- if (sys_checks[[label]]) "found" else "not found (may still work via R packages)"
  message("  - ", label, ": ", status)
}

# --- Project directories ---
message("\n[3/6] Project directories")
dirs <- c(
  file.path(root, "data", "processed"),
  file.path(root, "output", "reports"),
  file.path(root, "output", "figures"),
  file.path(root, "assets", "icons")
)
for (d in dirs) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  message("  - ", d)
}

# --- Match data ---
message("\n[4/6] Match data")
legacy_sample <- file.path(root, "data_sample", "matches")
raw_statsbomb <- file.path(root, "data", "raw", "statsbomb", "matches")
data_roots <- c(legacy_sample, raw_statsbomb)
data_roots <- data_roots[dir.exists(data_roots)]

if (length(data_roots) == 0) {
  message("  WARNING: No raw StatsBomb data found.")
  message("  Copy JSON to data/raw/statsbomb/matches/{match_id}/v1/")
  message("  or legacy data_sample/matches/{match_id}/v1/")
  message("  See README.md → Adding new match data")
} else {
  match_ids <- unique(unlist(lapply(data_roots, function(p) {
    ids <- list.dirs(p, recursive = FALSE, full.names = FALSE)
    ids[grepl("^[0-9]+$", ids)]
  })))
  message("  Found ", length(match_ids), " match folder(s): ",
          paste(head(match_ids, 5), collapse = ", "),
          if (length(match_ids) > 5) " ..." else "")
}

# --- R packages ---
message("\n[5/6] R packages")
cran_packages <- c(
  "tidyverse",
  "jsonlite",
  "yaml",
  "scales",
  "grid",
  "showtext",
  "sysfonts",
  "rmarkdown",
  "ggimage",
  "rsvg",
  "patchwork",
  "magick"
)

if (check_only) {
  missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) {
    message("  All required packages are installed.")
  } else {
    message("  Missing: ", paste(missing, collapse = ", "))
    quit(status = 1)
  }
} else {
  missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("  Installing: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  } else {
    message("  All CRAN packages already installed.")
  }
}

# --- PDF engine (optional) ---
message("\n[6/6] PDF export")
if (install_pdf) {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    install.packages("tinytex", repos = "https://cloud.r-project.org")
  }
  if (!tinytex::is_tinytex()) {
    message("  Installing TinyTeX (one-time, requires internet)...")
    tinytex::install_tinytex()
  } else {
    message("  TinyTeX already installed.")
  }
} else {
  message("  Skipped (pass --pdf to install TinyTeX for PDF reports).")
}

# --- Load project & rasterise icons ---
if (!check_only) {
  source(file.path(root, "reports", "_setup.R"))
  load_project(root = root, install_packages = FALSE)
  if (exists("ensure_shot_icons", mode = "function")) {
    ensure_shot_icons(icon_set = "footprint")
    message("\nShot-map icons rasterised to assets/icons/*.png")
  }
}

message("\n=== Setup complete ===")
message("Next steps:")
message("  1. Ensure raw match JSON is available (data/raw/ or data_sample/)")
message("  2. Rscript scripts/run_build.R")
message("  3. Rscript scripts/run_report.R 4036731 html")
message("     Rscript scripts/run_report.R 4036731 both   # HTML + PDF")
message("")
message("Or run the full pipeline:")
message("  Rscript scripts/run_all.R")
