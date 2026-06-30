#!/usr/bin/env Rscript

source("reports/_setup.R", local = TRUE)
load_project(root = getwd())

match_id <- 4036772L
data <- load_match_data("statsbomb", match_id, build_if_missing = TRUE)

plot <- viz_structure_momentum_grid(
  events_df = data$events,
  meta = data$meta,
  team_match_stats_df = data$team_match_stats,
  player_match_stats_df = data$player_match_stats,
  lineups_df = data$lineups,
  match_id = match_id,
  home_color = "#006233",
  away_color = "#ED2939"
)

out_dir <- get_figures_dir(match_id, provider = "statsbomb")
out_path <- file.path(out_dir, "Algeria_vs_Austria_Structure_Momentum_Grid.png")
save_figure(plot, out_path, format = "16_9")

message("Saved: ", out_path)
