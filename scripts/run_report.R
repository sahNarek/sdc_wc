#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

match_id <- if (length(commandArgs(trailingOnly = TRUE))) {
  as.integer(commandArgs(trailingOnly = TRUE)[1])
} else {
  4036731L
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown", repos = "https://cloud.r-project.org")
}

rmarkdown::render(
  file.path(root, "reports", "02_match_report_template.Rmd"),
  output_dir = file.path(root, "output", "reports"),
  params = list(match_id = match_id),
  quiet = FALSE
)

message("Report written to output/reports/")
