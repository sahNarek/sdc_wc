#!/usr/bin/env Rscript
# Full local pipeline: setup → build data → render report.
#
# Usage:
#   Rscript scripts/run_all.R
#   Rscript scripts/run_all.R 4036731 both statsbomb
#   Rscript scripts/run_all.R 4036731 html all --skip-setup

cli_args <- commandArgs(trailingOnly = TRUE)
skip_setup <- "--skip-setup" %in% cli_args
cli_args <- cli_args[!cli_args %in% c("--skip-setup")]

match_id <- if (length(cli_args) >= 1) cli_args[[1]] else "4036731"
format <- if (length(cli_args) >= 2) cli_args[[2]] else "html"
providers <- if (length(cli_args) >= 3) cli_args[[3]] else "statsbomb"

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

run_step <- function(script, extra_args = character()) {
  cmd <- sprintf(
    "%s --vanilla %s",
    shQuote(file.path(R.home("bin"), "R")),
    shQuote(file.path(root, "scripts", script))
  )
  if (length(extra_args) > 0) {
    cmd <- paste(cmd, paste(extra_args, collapse = " "))
  }
  message("\n>>> ", script, if (length(extra_args)) paste("", paste(extra_args, collapse = " ")) else "")
  status <- system(cmd, intern = FALSE)
  if (status != 0) {
    stop(script, " failed with exit code ", status, call. = FALSE)
  }
  invisible(TRUE)
}

message("=== SDC World Cup 2026 — full pipeline ===")

if (!skip_setup) {
  pdf_flag <- if (format %in% c("pdf", "both")) "--pdf" else character()
  run_step("setup_local.R", pdf_flag)
}

run_step("run_build.R", providers)
run_step("run_report.R", c(match_id, format, providers))

message("\n=== Pipeline complete ===")
