source("reports/_setup.R", local = TRUE)
load_project(root = getwd())

data <- load_match_data("statsbomb", 4036758)

plot <- viz_portugal_set_piece_networks(
  data$events,
  team_name = "Portugal",
  match_id = 4036758,
  meta = data$meta,
  lineups = data$lineups,
  opponent_name = "Uzbekistan",
  team_color = "#D62728",
  opponent_color = "#17BECF",
  attacking_zone_only = TRUE,
  max_goal_distance_m = 35,
  eyebrow = "Set piece sequences",
  title = "Portugal · three dangerous set-piece passing networks",
  subtitle = "Portugal 4–0 Uzbekistan"
)

out <- "output/figures/4036758/statsbomb/Portugal_Set_Piece_Networks.png"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out,
  plot = plot,
  width = 864 / 96,
  height = 1080 / 96,
  units = "in",
  dpi = 120,
  bg = SDC_ARTICLE_COLORS$offwhite
)
cat("Saved:", out, "\n")
