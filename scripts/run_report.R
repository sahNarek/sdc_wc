#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

cli_args <- commandArgs(trailingOnly = TRUE)

match_id <- if (length(cli_args) >= 1) {
  as.integer(cli_args[[1]])
} else {
  4036731L
}

format <- if (length(cli_args) >= 2) {
  tolower(cli_args[[2]])
} else {
  "html"
}

providers <- if (length(cli_args) >= 3) {
  cli_args[[3]]
} else {
  "statsbomb"
}

if (!format %in% c("html", "pdf", "both")) {
  stop("Format must be one of: html, pdf, both", call. = FALSE)
}

source(file.path(root, "reports", "_setup.R"))
load_project(root = root)

outputs <- render_match_report(
  match_id = match_id,
  format = format,
  providers = providers,
  root = root
)

message("Report written to: ", file.path(root, "output", "reports"))
for (out in outputs) {
  message("  - ", out)
}
