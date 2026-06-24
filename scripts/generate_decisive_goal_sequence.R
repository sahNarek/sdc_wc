source("reports/_setup.R", local = TRUE)
load_project(root = getwd())

data <- load_match_data("statsbomb", 4036758)
plot <- viz_decisive_goal_sequence(
  data$events,
  team_name = "Portugal",
  match_id = 4036758,
  meta = data$meta,
  goal_minute = 5,
  scorer_name = "Cristiano Ronaldo",
  team_color = "#D62728",
  headline = "11 SECONDS. TWO PASSES.\nONE DECISIVE GOAL.",
  subtitle = "A right-wing release found Cancelo in space, then Ronaldo finished from close range.",
  chain_text = "Neto -> Cancelo -> Ronaldo goal",
  detail_text = "The 0.20 xG finish opened the scoring and set the tone for Portugal's 4-0 win.",
  highlight_carry_players = c("Pedro Lomba Neto", "João Pedro Cavaco Cancelo"),
  format = "4_5"
)
out <- "output/figures/4036758/statsbomb/Decisive_Goal_Sequence_Ronaldo_5.png"
save_figure(plot, out, format = "4_5")
cat("Saved:", out, "\n")
