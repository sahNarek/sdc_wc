#' Empty events table — Wyscout bundle in all_data has no event stream
#'
#' UC1–UC8 require StatsBomb-style event coordinates. The Wyscout gold CSV
#' supplies lineups and player aggregates only.
empty_wyscout_events <- function() {
  tibble::tibble(
    match_id = integer(),
    id = character(),
    type_name = character(),
    type_id = integer(),
    team_id = integer(),
    team_name = character(),
    player_id = integer(),
    player_name = character(),
    location_x = double(),
    location_y = double(),
    shot_statsbomb_xg = double(),
    shot_outcome_name = character(),
    shot_type_name = character(),
    shot_body_part_name = character(),
    shot_end_location_x = double(),
    shot_end_location_y = double(),
    shot_end_location_z = double(),
    minute = integer()
  )
}
