CANONICAL_TABLES <- c(
  "meta",
  "events",
  "lineups",
  "players",
  "teams",
  "player_match_stats",
  "team_match_stats"
)

#' Ensure viz-layer column aliases exist on events (provider-agnostic boundary)
ensure_viz_aliases <- function(events_df) {
  if (nrow(events_df) == 0) {
    return(events_df)
  }

  out <- events_df %>%
    dplyr::mutate(
      shot_statsbomb_xg = dplyr::coalesce(
        .data$shot_statsbomb_xg,
        .data$shot_xg
      ),
      `type.name` = dplyr::coalesce(.data$`type.name`, .data$type_name),
      `team.name` = dplyr::coalesce(.data$`team.name`, .data$team_name),
      `team.id` = dplyr::coalesce(.data$`team.id`, .data$team_id),
      `player.name` = dplyr::coalesce(.data$`player.name`, .data$player_name),
      `player.id` = dplyr::coalesce(.data$`player.id`, .data$player_id),
      `location.x` = dplyr::coalesce(.data$`location.x`, .data$location_x),
      `location.y` = dplyr::coalesce(.data$`location.y`, .data$location_y),
      `pass.end_location.x` = dplyr::coalesce(
        .data$`pass.end_location.x`,
        .data$pass_end_location_x
      ),
      `pass.end_location.y` = dplyr::coalesce(
        .data$`pass.end_location.y`,
        .data$pass_end_location_y
      ),
      `pass.outcome.name` = dplyr::coalesce(
        .data$`pass.outcome.name`,
        .data$pass_outcome_name
      ),
      `pass.shot_assist` = dplyr::coalesce(
        .data$`pass.shot_assist`,
        .data$pass_shot_assist
      ),
      `pass.goal_assist` = dplyr::coalesce(
        .data$`pass.goal_assist`,
        .data$pass_goal_assist
      ),
      `shot.statsbomb_xg` = dplyr::coalesce(
        .data$`shot.statsbomb_xg`,
        .data$shot_statsbomb_xg,
        .data$shot_xg
      ),
      `shot.outcome.name` = dplyr::coalesce(
        .data$`shot.outcome.name`,
        .data$shot_outcome_name
      ),
      `shot.type.name` = dplyr::coalesce(
        .data$`shot.type.name`,
        .data$shot_type_name
      ),
      `shot.body_part.name` = dplyr::coalesce(
        .data$`shot.body_part.name`,
        .data$shot_body_part_name
      ),
      `shot.key_pass_id` = dplyr::coalesce(
        .data$`shot.key_pass_id`,
        .data$shot_key_pass_id
      ),
      `shot.end_location.x` = dplyr::coalesce(
        .data$`shot.end_location.x`,
        .data$shot_end_location_x
      ),
      `shot.end_location.y` = dplyr::coalesce(
        .data$`shot.end_location.y`,
        .data$shot_end_location_y
      ),
      `shot.end_location.z` = dplyr::coalesce(
        .data$`shot.end_location.z`,
        .data$shot_end_location_z
      ),
      `duel.type.name` = dplyr::coalesce(
        .data$`duel.type.name`,
        .data$duel_type_name
      )
    )

  if ("obv_total_net" %in% names(out) && "obv.total.net" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        obv_total_net = dplyr::coalesce(.data$obv_total_net, .data$`obv.total.net`)
      )
  } else if ("obv.total.net" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(obv_total_net = .data$`obv.total.net`)
  }

  out
}

add_canonical_event_columns <- function(events_df) {
  if (nrow(events_df) == 0) {
    return(events_df)
  }

  if ("shot_xg" %in% names(events_df)) {
    events_df <- events_df %>%
      dplyr::mutate(
        shot_xg = dplyr::coalesce(.data$shot_xg, .data$shot_statsbomb_xg)
      )
  } else {
    events_df <- events_df %>%
      dplyr::mutate(shot_xg = .data$shot_statsbomb_xg)
  }

  events_df
}

normalize_match_bundle <- function(bundle,
                                   provider,
                                   canonical_match_id) {
  canonical_match_id <- as.integer(canonical_match_id)
  provider_match_id <- as.integer(bundle$provider_match_id %||% canonical_match_id)

  bundle$meta <- bundle$meta %>%
    dplyr::mutate(
      match_id = canonical_match_id,
      provider = provider,
      provider_match_id = provider_match_id
    )

  for (table in setdiff(CANONICAL_TABLES, "meta")) {
    if (!is.null(bundle[[table]]) && nrow(bundle[[table]]) > 0) {
      bundle[[table]] <- bundle[[table]] %>%
        dplyr::mutate(
          match_id = canonical_match_id,
          provider = provider,
          provider_match_id = provider_match_id
        )
    }
  }

  if (!is.null(bundle$events)) {
    bundle$events <- bundle$events %>%
      add_canonical_event_columns() %>%
      ensure_viz_aliases()
  }

  bundle
}
