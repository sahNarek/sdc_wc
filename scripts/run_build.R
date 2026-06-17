#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

cli_args <- commandArgs(trailingOnly = TRUE)

providers_arg <- if (length(cli_args) >= 1) {
  cli_args[[1]]
} else {
  "statsbomb"
}

source(file.path(root, "reports", "_setup.R"))
load_project(root = root)

config <- load_match_config()
message("Primary match: ", config$development$primary_match_id, " (", config$development$primary_label, ")")

providers <- parse_providers_arg(providers_arg)
if (length(providers) == 1 && providers == "all") {
  providers <- enabled_providers(root)
}

for (provider in providers) {
  message("Building provider: ", provider)
  wc_matches <- build_all_matches(
    match_ids = config$development$sample_match_ids,
    provider = provider
  )
}

primary_id <- config$development$primary_match_id
primary_provider <- if ("statsbomb" %in% providers) "statsbomb" else providers[1]
events <- load_match_data(primary_provider, primary_id)$events

sg <- compute_team_shots_goals(events, match_id = primary_id)
print(wc_matches$meta)
print(sg)

message("Done.")
