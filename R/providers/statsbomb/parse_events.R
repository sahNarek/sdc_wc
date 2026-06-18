#' Extract a coordinate from a length-2 list/array
coord_x <- function(loc) {
  if (is.null(loc) || length(loc) < 1) {
    return(NA_real_)
  }
  as.numeric(loc[[1]])
}

coord_y <- function(loc) {
  if (is.null(loc) || length(loc) < 2) {
    return(NA_real_)
  }
  as.numeric(loc[[2]])
}

coord_z <- function(loc) {
  if (is.null(loc) || length(loc) < 3) {
    return(NA_real_)
  }
  as.numeric(loc[[3]])
}

nested_name <- function(obj, field = "name") {
  if (is.null(obj) || is.null(obj[[field]])) {
    return(NA_character_)
  }
  as.character(obj[[field]])
}

nested_id <- function(obj, field = "id") {
  if (is.null(obj) || is.null(obj[[field]])) {
    return(NA_real_)
  }
  as.numeric(obj[[field]])
}

#' Flatten one StatsBomb event into a single row
flatten_event <- function(event, match_id) {
  pass <- event$pass
  shot <- event$shot
  duel <- event$duel

  tibble::tibble(
    match_id = as.integer(match_id),
    id = as.character(event$id),
    index = as.integer(event$index),
    period = as.integer(event$period),
    timestamp = as.character(event$timestamp),
    minute = as.integer(event$minute),
    second = as.integer(event$second),
    type_id = nested_id(event$type),
    type_name = nested_name(event$type),
    team_id = nested_id(event$team),
    team_name = nested_name(event$team),
    player_id = nested_id(event$player),
    player_name = nested_name(event$player),
    possession = as.integer(event$possession),
    possession_team_id = nested_id(event$possession_team),
    possession_team_name = nested_name(event$possession_team),
    play_pattern_name = nested_name(event$play_pattern),
    location_x = coord_x(event$location),
    location_y = coord_y(event$location),
    pass_end_location_x = coord_x(if (!is.null(pass)) pass$end_location else NULL),
    pass_end_location_y = coord_y(if (!is.null(pass)) pass$end_location else NULL),
    pass_recipient_id = nested_id(if (!is.null(pass)) pass$recipient else NULL),
    pass_recipient_name = nested_name(if (!is.null(pass)) pass$recipient else NULL),
    pass_outcome_name = nested_name(if (!is.null(pass)) pass$outcome else NULL),
    pass_shot_assist = if (!is.null(pass) && !is.null(pass$shot_assist)) {
      as.logical(pass$shot_assist)
    } else {
      NA
    },
    pass_goal_assist = if (!is.null(pass) && !is.null(pass$goal_assist)) {
      as.logical(pass$goal_assist)
    } else {
      NA
    },
    shot_statsbomb_xg = if (!is.null(shot) && !is.null(shot$statsbomb_xg)) {
      as.numeric(shot$statsbomb_xg)
    } else {
      NA_real_
    },
    shot_outcome_name = nested_name(if (!is.null(shot)) shot$outcome else NULL),
    shot_type_name = nested_name(if (!is.null(shot)) shot$type else NULL),
    shot_body_part_name = nested_name(if (!is.null(shot)) shot$body_part else NULL),
    shot_key_pass_id = if (!is.null(shot) && !is.null(shot$key_pass_id)) {
      as.character(shot$key_pass_id)
    } else {
      NA_character_
    },
    shot_end_location_x = coord_x(if (!is.null(shot)) shot$end_location else NULL),
    shot_end_location_y = coord_y(if (!is.null(shot)) shot$end_location else NULL),
    shot_end_location_z = coord_z(if (!is.null(shot)) shot$end_location else NULL),
    duel_type_name = nested_name(if (!is.null(duel)) duel$type else NULL)
  )
}

#' Parse events JSON into a flat dataframe (allclean-style columns)
parse_events_json <- function(events, match_id) {
  if (length(events) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(events, flatten_event, match_id = match_id))
}

#' StatsBombR-compatible aliases for viz functions
add_statsbomb_aliases <- function(events_df) {
  events_df %>%
    dplyr::mutate(
      `type.name` = .data$type_name,
      `team.name` = .data$team_name,
      `team.id` = .data$team_id,
      `player.name` = .data$player_name,
      `player.id` = .data$player_id,
      `location.x` = .data$location_x,
      `location.y` = .data$location_y,
      `pass.end_location.x` = .data$pass_end_location_x,
      `pass.end_location.y` = .data$pass_end_location_y,
      `pass.recipient.id` = .data$pass_recipient_id,
      `pass.recipient.name` = .data$pass_recipient_name,
      `pass.outcome.name` = .data$pass_outcome_name,
      `pass.shot_assist` = .data$pass_shot_assist,
      `pass.goal_assist` = .data$pass_goal_assist,
      `shot.statsbomb_xg` = .data$shot_statsbomb_xg,
      `shot.outcome.name` = .data$shot_outcome_name,
      `shot.type.name` = .data$shot_type_name,
      `shot.body_part.name` = .data$shot_body_part_name,
      `shot.key_pass_id` = .data$shot_key_pass_id,
      `shot.end_location.x` = .data$shot_end_location_x,
      `shot.end_location.y` = .data$shot_end_location_y,
      `shot.end_location.z` = .data$shot_end_location_z,
      `duel.type.name` = .data$duel_type_name
    )
}
