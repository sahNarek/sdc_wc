source("reports/_setup.R", local = TRUE)
load_project(root = getwd())

data <- load_match_data("statsbomb", 4036758)
plot <- viz_dangerous_set_pieces_grid(
  data$events,
  team_name = "Portugal",
  match_id = 4036758,
  meta = data$meta,
  lineups = data$lineups,
  opponent_name = "Uzbekistan",
  team_color = "#D62728",
  opponent_color = "#FFFFFF",
  attacking_zone_only = TRUE,
  max_goal_distance_m = 35,
  top_n = 6,
  ncol = 3,
  title = "PORTUGAL'S MOST DANGEROUS SET PIECES",
  subtitle = "Ranked by shot quality and penalty-box threat · Portugal 4–0 Uzbekistan"
)

out <- "output/figures/4036758/statsbomb/Portugal_Dangerous_Set_Pieces.png"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out,
  plot = plot,
  width = 12,
  height = 14,
  units = "in",
  dpi = 96,
  bg = SDC_ARTICLE_COLORS$offwhite
)
cat("Saved:", out, "\n")

gif_out <- "output/figures/4036758/statsbomb/Portugal_Dangerous_Set_Piece_Animation_Mendes.gif"
save_dangerous_set_piece_gif(
  data$events,
  team_name = "Portugal",
  match_id = 4036758,
  meta = data$meta,
  lineups = data$lineups,
  opponent_name = "Uzbekistan",
  possession_id = 29L,
  path = gif_out,
  team_color = "#D62728",
  opponent_color = "#FFFFFF",
  frames_per_pass = 11L,
  frames_per_shot = 14L,
  hold_at_end = 6L,
  max_keyframes = 12,
  fps = 10,
  width_px = 864,
  height_px = 1080,
  defending_asset = "white",
  eyebrow = "Set piece goal",
  headline_prefix = "Nuno Mendes"
)
cat("Saved:", gif_out, "\n")
