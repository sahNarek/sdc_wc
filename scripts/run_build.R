#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

source(file.path(root, "reports", "_setup.R"))
load_project(root = root)

config <- load_match_config()
message("Primary match: ", config$development$primary_match_id, " (", config$development$primary_label, ")")

wc_matches <- build_all_matches(
  match_ids = config$development$sample_match_ids
)

meta <- wc_matches$meta
events <- wc_matches$events %>% dplyr::filter(match_id == config$development$primary_match_id)

sg <- compute_team_shots_goals(events, match_id = config$development$primary_match_id)
print(meta)
print(sg)

message("Done.")
