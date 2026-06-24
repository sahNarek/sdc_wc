source("reports/_setup.R", local = TRUE)
load_project(root = getwd())

data <- load_match_data("statsbomb", 4036758)

gif_out <- "output/figures/4036758/statsbomb/Portugal_Dangerous_Set_Pieces_Faceted.gif"
dir.create(dirname(gif_out), recursive = TRUE, showWarnings = FALSE)

save_faceted_set_pieces_gif(
  data$events,
  team_name = "Portugal",
  match_id = 4036758,
  meta = data$meta,
  lineups = data$lineups,
  opponent_name = "Uzbekistan",
  path = gif_out,
  panels = list(
    mendes = list(
      possession_id = 29L,
      panel_title = "Nuno Mendes · FK · 16' · Goal"
    ),
    nematov_og = list(
      possession_id = 136L,
      panel_title = "Nematov own goal · Corner · 59'"
    ),
    ronaldo_saved = list(
      possession_id = 135L,
      panel_title = "Ronaldo · FK · 57' · Saved · 0.36 xG"
    )
  ),
  team_color = "#D62728",
  opponent_color = "#1EB53A",
  frames_per_pass = 18L,
  frames_per_shot = 22L,
  hold_at_end = 10L,
  max_keyframes = 14,
  fps = 5,
  width_px = 864,
  height_px = 1080,
  dpi = 96,
  defending_asset = "uzbek",
  facet_ncol = 3L,
  eyebrow = "Set piece sequences",
  headline = "Portugal · three dangerous set pieces",
  subtitle = "Portugal 4–0 Uzbekistan"
)

cat("Saved:", gif_out, "\n")
