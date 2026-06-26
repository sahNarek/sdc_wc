#' Load StatsBomb 360 freeze frames for one match, indexed by event UUID
load_statsbomb_360_frames <- function(match_id, data_dir = NULL) {
  frames_path <- resolve_match_file(match_id, "360_frames.json", data_dir = data_dir)
  if (is.null(frames_path) || !file.exists(frames_path)) {
    return(list(frames = list(), by_event_id = list()))
  }

  frames <- jsonlite::fromJSON(frames_path, simplifyVector = FALSE)
  by_event_id <- stats::setNames(
    frames,
    vapply(frames, function(x) x$event_uuid, character(1))
  )
  list(frames = frames, by_event_id = by_event_id)
}

#' Event id used for freeze-frame lookup (shot or last completed pass)
set_piece_key_event_id <- function(sequence_df) {
  data <- ensure_viz_aliases(sequence_df)
  shot <- data %>% dplyr::filter(.data$`type.name` == "Shot")
  if (nrow(shot) > 0) {
    return(shot$id[1])
  }

  passes <- data %>%
    dplyr::filter(
      .data$`type.name` == "Pass",
      is.na(.data$`pass.outcome.name`)
    )
  if (nrow(passes) > 0) {
    return(passes$id[nrow(passes)])
  }

  data$id[nrow(data)]
}

#' Danger score for ranking set pieces (xG-led, with box-entry tie-breakers)
score_set_piece_danger <- function(sequence_df, events_df) {
  sequence_df <- normalize_sequence_coords(sequence_df, events_df)
  shot <- sequence_df %>% dplyr::filter(.data$type_name == "Shot")
  xg <- if (nrow(shot) > 0) {
    max(shot$`shot.statsbomb_xg`, na.rm = TRUE)
  } else {
    0
  }

  passes <- sequence_df %>%
    dplyr::filter(
      .data$type_name == "Pass",
      is.na(.data$`pass.outcome.name`)
    )
  box_passes <- sum(
    passes$pass_end_x >= 102 &
      passes$pass_end_y >= 18 &
      passes$pass_end_y <= 62,
    na.rm = TRUE
  )

  xg * 10 + box_passes * 0.05 + nrow(passes) * 0.01
}

#' Rank filtered set-piece possessions by danger score
rank_dangerous_set_pieces <- function(events_df,
                                      team_name,
                                      match_id = NULL,
                                      opponent_name = NULL,
                                      patterns = c("From Corner", "From Free Kick"),
                                      attacking_zone_only = TRUE,
                                      max_goal_distance_m = 35,
                                      top_n = 6) {
  set_pieces <- identify_team_set_piece_possessions(
    events_df,
    team_name = team_name,
    match_id = match_id,
    patterns = patterns
  )

  if (isTRUE(attacking_zone_only)) {
    set_pieces <- filter_attacking_zone_set_pieces(
      set_pieces,
      events_df = events_df,
      max_goal_distance_m = max_goal_distance_m
    )
  }

  if (nrow(set_pieces) == 0) {
    stop("No set-piece possessions found for ", team_name, call. = FALSE)
  }

  ranked <- purrr::map_dfr(seq_len(nrow(set_pieces)), function(i) {
    sp <- set_pieces[i, , drop = FALSE]
    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = sp$possession,
      team_name = team_name
    )
    if (!keep_set_piece_sequence_for_network(sequence)) {
      return(NULL)
    }

    shot <- sequence %>% dplyr::filter(.data$type_name == "Shot")
    shot_xg <- if (nrow(shot) > 0) {
      max(shot$`shot.statsbomb_xg`, na.rm = TRUE)
    } else {
      0
    }
    shot_outcome <- if (nrow(shot) > 0) shot$`shot.outcome.name`[1] else NA_character_

    tibble::tibble(
      possession = sp$possession,
      minute = sp$minute,
      second = sp$second,
      period = sp$period,
      set_piece_type = sp$set_piece_type,
      danger_score = score_set_piece_danger(sequence, events_df),
      shot_xg = shot_xg,
      shot_outcome = shot_outcome,
      key_event_id = set_piece_key_event_id(sequence)
    )
  })

  if (nrow(ranked) == 0) {
    stop("No qualifying set-piece sequences found for ", team_name, call. = FALSE)
  }

  ranked %>%
    dplyr::arrange(dplyr::desc(.data$danger_score), .data$minute) %>%
    dplyr::slice_head(n = top_n)
}

#' Panel title for one dangerous set piece
dangerous_set_piece_panel_title <- function(set_piece_row) {
  if (!is.null(set_piece_row$panel_headline) &&
      !is.na(set_piece_row$panel_headline) &&
      nzchar(set_piece_row$panel_headline)) {
    return(set_piece_row$panel_headline)
  }

  suffix <- if (!is.na(set_piece_row$shot_outcome) && set_piece_row$shot_xg > 0) {
    paste0(
      " · ",
      set_piece_row$shot_outcome,
      " · ",
      format(round(set_piece_row$shot_xg, 2), nsmall = 2),
      " xG"
    )
  } else if (!is.na(set_piece_row$shot_outcome)) {
    paste0(" · ", set_piece_row$shot_outcome)
  } else {
    ""
  }

  paste0(
    set_piece_row$set_piece_type,
    " · ",
    set_piece_row$minute,
    "'",
    suffix
  )
}

#' Parse freeze-frame player locations relative to the attacking team
parse_freeze_frame_positions <- function(frame_entry,
                                         events_df,
                                         attacking_team_name,
                                         period,
                                         frame_index = 1L) {
  if (is.null(frame_entry) || is.null(frame_entry$freeze_frame)) {
    return(tibble::tibble())
  }

  direction <- infer_team_attacking_high_x(events_df)
  attacks_high_x <- direction %>%
    dplyr::filter(.data$`team.name` == !!attacking_team_name, .data$period == !!period) %>%
    dplyr::pull(.data$attacks_high_x) %>%
    dplyr::first()
  attacks_high_x <- dplyr::coalesce(attacks_high_x, TRUE)

  purrr::map_dfr(seq_along(frame_entry$freeze_frame), function(i) {
    player <- frame_entry$freeze_frame[[i]]
    loc_x <- player$location[[1]]
    loc_y <- player$location[[2]]
    norm <- normalize_opponent_half_coords(loc_x, loc_y, attacks_high_x)
    tibble::tibble(
      frame_index = frame_index,
      player_index = i,
      pitch_x = norm$x,
      pitch_y = norm$y,
      is_attacking_team = isTRUE(player$teammate),
      is_actor = isTRUE(player$actor),
      is_keeper = isTRUE(player$keeper)
    )
  }) %>%
    dplyr::mutate(
      team_side = dplyr::if_else(
        .data$is_attacking_team,
        "attacking",
        "defending"
      )
    )
}

#' Jersey lookup from possession events and lineups
build_team_jersey_lookup <- function(possession_events,
                                     lineups,
                                     team_name,
                                     events_df,
                                     period,
                                     normalize_team_name = NULL) {
  data <- ensure_viz_aliases(possession_events)
  norm_team <- normalize_team_name %||% team_name
  direction <- infer_team_attacking_high_x(events_df)
  attacks_high_x <- direction %>%
    dplyr::filter(.data$`team.name` == !!norm_team, .data$period == !!period) %>%
    dplyr::pull(.data$attacks_high_x) %>%
    dplyr::first()
  attacks_high_x <- dplyr::coalesce(attacks_high_x, TRUE)

  data %>%
    dplyr::filter(
      .data$`team.name` == !!team_name,
      !is.na(.data$`player.id`),
      !is.na(.data$`location.x`)
    ) %>%
    dplyr::left_join(
      lineups %>%
        dplyr::select(
          player_id,
          team_name,
          jersey_number
        ),
      by = c("player.id" = "player_id", "team.name" = "team_name")
    ) %>%
    dplyr::mutate(
      pitch_x = purrr::map2_dbl(
        .data$`location.x`,
        .data$`location.y`,
        ~normalize_opponent_half_coords(.x, .y, attacks_high_x)$x
      ),
      pitch_y = purrr::map2_dbl(
        .data$`location.x`,
        .data$`location.y`,
        ~normalize_opponent_half_coords(.x, .y, attacks_high_x)$y
      )
    ) %>%
    dplyr::group_by(.data$`player.id`) %>%
    dplyr::summarise(
      jersey_number = dplyr::first(.data$jersey_number[!is.na(.data$jersey_number)]),
      pitch_x = mean(.data$pitch_x, na.rm = TRUE),
      pitch_y = mean(.data$pitch_y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data$jersey_number))
}

#' Assign jersey labels to shirt positions by nearest known player location
assign_jersey_labels_to_positions <- function(positions_df,
                                              attacking_lookup,
                                              defending_lookup,
                                              max_dist = 5) {
  if (nrow(positions_df) == 0) {
    return(positions_df)
  }

  positions_df$jersey_label <- NA_character_
  for (i in seq_len(nrow(positions_df))) {
    lookup <- if (positions_df$team_side[i] == "attacking") {
      attacking_lookup
    } else {
      defending_lookup
    }
    if (nrow(lookup) == 0) {
      next
    }
    dist <- sqrt(
      (lookup$pitch_x - positions_df$pitch_x[i])^2 +
        (lookup$pitch_y - positions_df$pitch_y[i])^2
    )
    best <- which.min(dist)
    if (dist[best] <= max_dist) {
      positions_df$jersey_label[i] <- as.character(lookup$jersey_number[best])
    }
  }

  positions_df
}

#' Greedy nearest-neighbour track matching between two freeze frames
match_player_tracks <- function(from_df, to_df) {
  to_df$track_id <- NA_integer_
  if (nrow(from_df) == 0 || nrow(to_df) == 0) {
    to_df$track_id <- seq_len(nrow(to_df))
    return(to_df)
  }

  used_from <- rep(FALSE, nrow(from_df))
  for (i in seq_len(nrow(to_df))) {
    same_side <- which(from_df$team_side == to_df$team_side[i])
    if (length(same_side) == 0) {
      next
    }
    available <- same_side[!used_from[same_side]]
    if (length(available) == 0) {
      next
    }
    dist <- sqrt(
      (from_df$pitch_x[available] - to_df$pitch_x[i])^2 +
        (from_df$pitch_y[available] - to_df$pitch_y[i])^2
    )
    pick <- available[which.min(dist)]
    to_df$track_id[i] <- from_df$track_id[pick]
    used_from[pick] <- TRUE
  }

  new_ids <- seq_len(nrow(to_df))[is.na(to_df$track_id)]
  if (length(new_ids) > 0) {
    max_id <- max(c(from_df$track_id, 0), na.rm = TRUE)
    to_df$track_id[new_ids] <- max_id + seq_along(new_ids)
  }

  to_df
}

#' Build keyframe player positions from possession events with 360 data
build_set_piece_keyframes <- function(events_df,
                                      possession_id,
                                      team_name,
                                      period,
                                      frames_360,
                                      max_keyframes = 18) {
  poss_events <- ensure_viz_aliases(events_df) %>%
    dplyr::filter(.data$possession == !!possession_id) %>%
    dplyr::arrange(.data$index)

  keyed <- poss_events %>%
    dplyr::filter(.data$id %in% names(frames_360$by_event_id)) %>%
    dplyr::mutate(
      event_sec = .data$minute * 60L + .data$second,
      keep = .data$`type.name` %in% c(
        "Pass", "Carry", "Shot", "Ball Receipt*", "Pressure", "Duel"
      )
    ) %>%
    dplyr::filter(.data$keep) %>%
    dplyr::group_by(.data$event_sec) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$event_sec, .data$index)

  if (nrow(keyed) > max_keyframes) {
    idx <- unique(round(seq(1, nrow(keyed), length.out = max_keyframes)))
    keyed <- keyed[idx, , drop = FALSE]
  }

  frames <- purrr::imap(
    keyed$id,
    function(event_id, i) {
      frame_entry <- frames_360$by_event_id[[event_id]]
      parse_freeze_frame_positions(
        frame_entry,
        events_df = events_df,
        attacking_team_name = team_name,
        period = period,
        frame_index = i
      ) %>%
        dplyr::mutate(
          key_event_id = event_id,
          event_sec = keyed$event_sec[i]
        )
    }
  )

  if (length(frames) == 0) {
    return(tibble::tibble())
  }

  frames[[1]]$track_id <- seq_len(nrow(frames[[1]]))
  if (length(frames) > 1) {
    for (i in 2:length(frames)) {
      frames[[i]] <- match_player_tracks(frames[[i - 1]], frames[[i]])
    }
  }

  dplyr::bind_rows(frames)
}

#' Interpolate matched tracks between consecutive keyframes
interpolate_set_piece_keyframes <- function(keyframes_df,
                                            tween_steps = 8,
                                            hold_frames = 3) {
  if (nrow(keyframes_df) == 0) {
    return(keyframes_df)
  }

  key_ids <- unique(keyframes_df$frame_index)
  if (length(key_ids) < 2) {
    return(keyframes_df %>% dplyr::mutate(anim_frame = .data$frame_index))
  }

  duplicate_keyframe <- function(df, anim_id) {
    out <- df
    out$anim_frame <- anim_id
    out$is_actor <- FALSE
    out
  }

  row_for_track <- function(df, track_id) {
    df %>%
      dplyr::filter(.data$track_id == !!track_id) %>%
      dplyr::slice(1)
  }

  tween_between_keyframes <- function(a_full, b_full, anim_id, alpha) {
    all_ids <- union(a_full$track_id, b_full$track_id)
    if (length(all_ids) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(all_ids, function(track_id) {
      in_a <- track_id %in% a_full$track_id
      in_b <- track_id %in% b_full$track_id
      row_a <- if (in_a) row_for_track(a_full, track_id) else NULL
      row_b <- if (in_b) row_for_track(b_full, track_id) else NULL
      meta <- if (in_a) row_a else row_b

      if (in_a && in_b) {
        pitch_x <- row_a$pitch_x + alpha * (row_b$pitch_x - row_a$pitch_x)
        pitch_y <- row_a$pitch_y + alpha * (row_b$pitch_y - row_a$pitch_y)
      } else if (in_a) {
        pitch_x <- row_a$pitch_x
        pitch_y <- row_a$pitch_y
      } else {
        pitch_x <- row_b$pitch_x
        pitch_y <- row_b$pitch_y
      }

      tibble::tibble(
        track_id = track_id,
        pitch_x = pitch_x,
        pitch_y = pitch_y,
        team_side = meta$team_side,
        is_attacking_team = meta$is_attacking_team,
        is_keeper = meta$is_keeper,
        is_actor = FALSE,
        anim_frame = anim_id
      )
    })
  }

  out <- list()
  anim_id <- 1L
  for (i in seq_len(length(key_ids) - 1L)) {
    a_full <- keyframes_df %>% dplyr::filter(.data$frame_index == key_ids[i])
    b_full <- keyframes_df %>% dplyr::filter(.data$frame_index == key_ids[i + 1L])

    for (hold in seq_len(hold_frames)) {
      out[[length(out) + 1]] <- duplicate_keyframe(a_full, anim_id)
      anim_id <- anim_id + 1L
    }

    for (step in seq_len(tween_steps)) {
      alpha <- step / (tween_steps + 1)
      out[[length(out) + 1]] <- tween_between_keyframes(a_full, b_full, anim_id, alpha)
      anim_id <- anim_id + 1L
    }
  }

  b_last <- keyframes_df %>%
    dplyr::filter(.data$frame_index == key_ids[length(key_ids)])
  for (hold in seq_len(hold_frames)) {
    out[[length(out) + 1]] <- duplicate_keyframe(b_last, anim_id)
    anim_id <- anim_id + 1L
  }

  dplyr::bind_rows(out)
}

#' Ball movement segments along passes and the shot trajectory
build_set_piece_ball_segments <- function(network) {
  edges <- network$edges
  shots <- network$shots
  segments <- list()

  if (nrow(edges) > 0) {
    for (i in seq_len(nrow(edges))) {
      segments[[length(segments) + 1L]] <- tibble::tibble(
        segment_type = "pass",
        segment_index = i,
        x_start = edges$x_from[i],
        y_start = edges$y_from[i],
        x_end = edges$x_to[i],
        y_end = edges$y_to[i]
      )
    }
  }

  if (nrow(shots) > 0) {
    shot_traj <- add_shot_trajectory_endpoints(
      shots %>%
        dplyr::mutate(
          `location.x` = .data$pitch_x,
          `location.y` = .data$pitch_y
        )
    )
    segments[[length(segments) + 1L]] <- tibble::tibble(
      segment_type = "shot",
      segment_index = nrow(edges) + 1L,
      x_start = shot_traj$`location.x`[1],
      y_start = shot_traj$`location.y`[1],
      x_end = shot_traj$traj_xend[1],
      y_end = shot_traj$traj_yend[1],
      shot_outcome = shot_traj$`shot.outcome.name`[1]
    )
  }

  if (length(segments) == 0) {
    return(tibble::tibble(
      segment_type = character(),
      segment_index = integer(),
      x_start = numeric(),
      y_start = numeric(),
      x_end = numeric(),
      y_end = numeric(),
      shot_outcome = character()
    ))
  }

  dplyr::bind_rows(segments) %>%
    dplyr::mutate(
      segment_length = sqrt(
        (.data$x_end - .data$x_start)^2 + (.data$y_end - .data$y_start)^2
      )
    )
}

#' Per-frame ball position and progressive pass / shot reveal state
compute_ball_states_for_animation <- function(segments_df,
                                              frames_per_pass = 14L,
                                              frames_per_shot = 18L,
                                              hold_at_end = 8L) {
  if (nrow(segments_df) == 0) {
    return(tibble::tibble(
      anim_frame = 1L,
      ball_x = NA_real_,
      ball_y = NA_real_,
      completed_passes = 0L,
      active_pass_index = NA_integer_,
      pass_progress = 0,
      show_shot = FALSE,
      shot_progress = 0,
      shot_end_x = NA_real_,
      shot_end_y = NA_real_
    ))
  }

  shot_seg <- segments_df %>% dplyr::filter(.data$segment_type == "shot")
  shot_end_x <- if (nrow(shot_seg) > 0) shot_seg$x_end[1] else NA_real_
  shot_end_y <- if (nrow(shot_seg) > 0) shot_seg$y_end[1] else NA_real_
  n_passes <- sum(segments_df$segment_type == "pass")

  states <- list()
  anim_id <- 1L
  for (seg_i in seq_len(nrow(segments_df))) {
    seg <- segments_df[seg_i, , drop = FALSE]
    n_seg_frames <- if (identical(seg$segment_type, "pass")) {
      frames_per_pass
    } else {
      frames_per_shot
    }
    is_pass <- identical(seg$segment_type, "pass")
    for (local_i in seq_len(n_seg_frames)) {
      alpha <- local_i / n_seg_frames
      states[[length(states) + 1L]] <- tibble::tibble(
        anim_frame = anim_id,
        ball_x = seg$x_start + alpha * (seg$x_end - seg$x_start),
        ball_y = seg$y_start + alpha * (seg$y_end - seg$y_start),
        completed_passes = if (is_pass) {
          as.integer(seg$segment_index - 1L)
        } else {
          as.integer(n_passes)
        },
        active_pass_index = if (is_pass) as.integer(seg$segment_index) else NA_integer_,
        pass_progress = if (is_pass) alpha else 0,
        show_shot = identical(seg$segment_type, "shot"),
        shot_progress = if (identical(seg$segment_type, "shot")) alpha else 0,
        shot_end_x = shot_end_x,
        shot_end_y = shot_end_y
      )
      anim_id <- anim_id + 1L
    }
  }

  last_seg <- segments_df[nrow(segments_df), , drop = FALSE]
  for (hold in seq_len(hold_at_end)) {
    states[[length(states) + 1L]] <- tibble::tibble(
      anim_frame = anim_id,
      ball_x = last_seg$x_end,
      ball_y = last_seg$y_end,
      completed_passes = as.integer(n_passes),
      active_pass_index = NA_integer_,
      pass_progress = 0,
      show_shot = any(segments_df$segment_type == "shot"),
      shot_progress = 1,
      shot_end_x = shot_end_x,
      shot_end_y = shot_end_y
    )
    anim_id <- anim_id + 1L
  }

  dplyr::bind_rows(states)
}

#' Map 360 keyframe positions onto the ball-driven animation timeline
resample_player_positions_for_animation <- function(keyframes_df, anim_ids) {
  if (nrow(keyframes_df) == 0 || length(anim_ids) == 0) {
    return(tibble::tibble())
  }

  key_ids <- unique(keyframes_df$frame_index)
  if (length(key_ids) < 2L) {
    base <- keyframes_df %>% dplyr::filter(.data$frame_index == key_ids[1])
    return(purrr::map_dfr(anim_ids, function(af) {
      out <- base
      out$anim_frame <- af
      out$is_actor <- FALSE
      out
    }))
  }

  dense <- interpolate_set_piece_keyframes(
    keyframes_df,
    tween_steps = 14L,
    hold_frames = 4L
  )
  dense_ids <- sort(unique(dense$anim_frame))
  n_dense <- length(dense_ids)
  max_anim <- max(anim_ids)

  purrr::map_dfr(anim_ids, function(af) {
    progress <- (af - 1) / max(1L, max_anim - 1L)
    dense_idx <- max(1L, min(n_dense, round(progress * (n_dense - 1L)) + 1L))
    dense %>%
      dplyr::filter(.data$anim_frame == dense_ids[dense_idx]) %>%
      dplyr::mutate(anim_frame = af)
  })
}

#' Pin sequence actors (passer, recipient, shooter) to event coordinates
build_sequence_actor_pins <- function(network, attacking_lookup) {
  pins <- list()
  edges <- network$edges
  shots <- network$shots

  nearest_jersey <- function(x, y) {
    if (nrow(attacking_lookup) == 0) {
      return(NA_character_)
    }
    dist <- sqrt(
      (attacking_lookup$pitch_x - x)^2 + (attacking_lookup$pitch_y - y)^2
    )
    as.character(attacking_lookup$jersey_number[which.min(dist)])
  }

  if (nrow(edges) > 0) {
    for (i in seq_len(nrow(edges))) {
      pins[[length(pins) + 1L]] <- tibble::tibble(
        pitch_x = edges$x_from[i],
        pitch_y = edges$y_from[i],
        team_side = "attacking",
        is_keeper = FALSE,
        is_actor = TRUE,
        is_attacking_team = TRUE,
        pin_role = "passer",
        pass_index = i,
        jersey_label = nearest_jersey(edges$x_from[i], edges$y_from[i])
      )
      pins[[length(pins) + 1L]] <- tibble::tibble(
        pitch_x = edges$x_to[i],
        pitch_y = edges$y_to[i],
        team_side = "attacking",
        is_keeper = FALSE,
        is_actor = TRUE,
        is_attacking_team = TRUE,
        pin_role = "recipient",
        pass_index = i,
        jersey_label = nearest_jersey(edges$x_to[i], edges$y_to[i])
      )
    }
  }

  if (nrow(shots) > 0) {
    pins[[length(pins) + 1L]] <- tibble::tibble(
      pitch_x = shots$pitch_x[1],
      pitch_y = shots$pitch_y[1],
      team_side = "attacking",
      is_keeper = FALSE,
      is_actor = TRUE,
      is_attacking_team = TRUE,
      pin_role = "shooter",
      pass_index = NA_integer_,
      jersey_label = nearest_jersey(shots$pitch_x[1], shots$pitch_y[1])
    )
  }

  if (length(pins) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(pins)
}

#' Add sequence actor pins to freeze-frame positions for static network maps
merge_static_sequence_actor_pins <- function(positions_df,
                                             pins_df,
                                             radius = 3) {
  if (nrow(pins_df) == 0) {
    return(positions_df)
  }

  keep_pins <- purrr::compact(purrr::map(seq_len(nrow(pins_df)), function(i) {
    pin <- pins_df[i, , drop = FALSE]
    if (nrow(positions_df) == 0) {
      return(pin)
    }
    dist <- sqrt(
      (positions_df$pitch_x - pin$pitch_x)^2 +
        (positions_df$pitch_y - pin$pitch_y)^2
    )
    if (any(dist < radius, na.rm = TRUE)) {
      return(NULL)
    }
    pin
  }))

  if (length(keep_pins) == 0) {
    return(positions_df)
  }

  dplyr::bind_rows(positions_df, dplyr::bind_rows(keep_pins))
}

#' Merge visible sequence actor pins into one animation frame
merge_sequence_actor_pins <- function(positions_df, pins_df, ball_state) {
  if (nrow(pins_df) == 0) {
    return(positions_df)
  }

  completed <- if (is.null(ball_state) || nrow(ball_state) == 0) {
    0L
  } else {
    ball_state$completed_passes[1] %||% 0L
  }
  active_pass <- if (is.null(ball_state) || nrow(ball_state) == 0) {
    NA_integer_
  } else {
    ball_state$active_pass_index[1]
  }
  show_shot <- !is.null(ball_state) &&
    nrow(ball_state) > 0 &&
    isTRUE(ball_state$show_shot[1])

  visible <- pins_df %>%
    dplyr::filter(
      (.data$pin_role == "passer" &
        !is.na(active_pass) &
        .data$pass_index == active_pass) |
        (.data$pin_role == "recipient" &
          !is.na(.data$pass_index) &
          .data$pass_index <= completed) |
        (.data$pin_role == "shooter" & show_shot)
    )

  if (nrow(visible) == 0) {
    return(positions_df)
  }

  keep_pins <- purrr::compact(purrr::map(seq_len(nrow(visible)), function(i) {
    pin <- visible[i, , drop = FALSE]
    if (nrow(positions_df) == 0) {
      return(pin)
    }
    dist <- sqrt(
      (positions_df$pitch_x - pin$pitch_x)^2 +
        (positions_df$pitch_y - pin$pitch_y)^2
    )
    if (any(dist < 3, na.rm = TRUE)) {
      return(NULL)
    }
    pin
  }))

  if (length(keep_pins) == 0) {
    return(positions_df)
  }

  dplyr::bind_rows(keep_pins)
}

#' Grass-green pitch styling for set-piece animations
set_piece_pitch_layers <- function(pitch_fill = "#3A7D44",
                                   line_colour = "#E8F2E8") {
  draw_decisive_sequence_pitch_layers(
    line_colour = line_colour,
    pitch_fill = pitch_fill
  )
}

#' Pitch layers for set-piece animations (article white or green grass)
get_set_piece_pitch_layers <- function(pitch_style = c("green", "article")) {
  pitch_style <- match.arg(pitch_style)
  if (identical(pitch_style, "article")) {
    return(draw_decisive_sequence_pitch_layers(
      line_colour = SDC_ARTICLE_COLORS$grid,
      pitch_fill = SDC_ARTICLE_COLORS$pitch
    ))
  }
  set_piece_pitch_layers()
}

#' Draw completed and in-progress pass arrows for one animation frame
draw_animation_pass_layers <- function(edges,
                                       ball_state,
                                       team_color) {
  layers <- list()
  if (nrow(edges) == 0 || is.null(ball_state) || nrow(ball_state) == 0) {
    return(layers)
  }

  completed <- ball_state$completed_passes[1] %||% 0L
  active_idx <- ball_state$active_pass_index[1]
  pass_progress <- ball_state$pass_progress[1] %||% 0

  if (completed > 0) {
    done <- edges %>% dplyr::slice_head(n = completed)
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = done,
      ggplot2::aes(
        x = .data$x_from,
        y = .data$y_from,
        xend = .data$x_to,
        yend = .data$y_to
      ),
      colour = team_color,
      alpha = 0.9,
      linewidth = 0.85,
      lineend = "round",
      arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed")
    )
  }

  if (!is.na(active_idx) && active_idx >= 1L && active_idx <= nrow(edges)) {
    edge <- edges[active_idx, , drop = FALSE]
  active <- tibble::tibble(
      x_from = edge$x_from,
      y_from = edge$y_from,
      x_to = edge$x_from + pass_progress * (edge$x_to - edge$x_from),
      y_to = edge$y_from + pass_progress * (edge$y_to - edge$y_from)
    )
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = active,
      ggplot2::aes(
        x = .data$x_from,
        y = .data$y_from,
        xend = .data$x_to,
        yend = .data$y_to
      ),
      colour = team_color,
      alpha = 0.9,
      linewidth = 0.85,
      lineend = "round",
      arrow = if (pass_progress >= 0.85) {
        grid::arrow(length = grid::unit(0.07, "inches"), type = "closed")
      } else {
        NULL
      }
    )
  }

  layers
}

#' Draw progressive shot trajectory with end-direction marker
draw_animation_shot_layers <- function(shots,
                                       ball_state,
                                       pitch_style = c("green", "article")) {
  pitch_style <- match.arg(pitch_style)
  shot_color_fn <- function(outcome) {
    if (identical(pitch_style, "article")) {
      if (outcome %in% c("Goal", "Own Goal")) {
        return(SDC_PALETTE[["orange"]])
      }
      if (outcome %in% c("Saved", "Blocked")) {
        return(SDC_PALETTE[["purple"]])
      }
      return(SDC_PALETTE[["red"]])
    }
    shot_trajectory_line_color(outcome)
  }

  layers <- list()
  if (nrow(shots) == 0 || is.null(ball_state) || nrow(ball_state) == 0) {
    return(layers)
  }
  if (!isTRUE(ball_state$show_shot[1])) {
    return(layers)
  }

  shot_progress <- ball_state$shot_progress[1] %||% 0
  shot_traj <- add_shot_trajectory_endpoints(
    shots %>%
      dplyr::mutate(
        `location.x` = .data$pitch_x,
        `location.y` = .data$pitch_y
      )
  ) %>%
    dplyr::mutate(
      line_colour = vapply(
        .data$`shot.outcome.name`,
        shot_color_fn,
        character(1)
      ),
      full_xend = .data$traj_xend,
      full_yend = .data$traj_yend,
      traj_xend = .data$`location.x` +
        shot_progress * (.data$traj_xend - .data$`location.x`),
      traj_yend = .data$`location.y` +
        shot_progress * (.data$traj_yend - .data$`location.y`)
    )

  layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
    data = shot_traj,
    ggplot2::aes(
      x = .data$`location.x`,
      y = .data$`location.y`,
      xend = .data$traj_xend,
      yend = .data$traj_yend,
      colour = I(.data$line_colour)
    ),
    linetype = "dashed",
    linewidth = 0.95,
    alpha = 0.95,
    arrow = if (shot_progress >= 0.2) {
      grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")
    } else {
      NULL
    }
  )

  if (shot_progress >= 0.9) {
    end_arrow <- shot_traj %>%
      dplyr::mutate(
        seg_x = .data$full_xend - 0.08 * (.data$full_xend - .data$`location.x`),
        seg_y = .data$full_yend - 0.08 * (.data$full_yend - .data$`location.y`)
      )
    layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
      data = end_arrow,
      ggplot2::aes(
        x = .data$seg_x,
        y = .data$seg_y,
        xend = .data$full_xend,
        yend = .data$full_yend,
        colour = I(.data$line_colour)
      ),
      linewidth = 1.1,
      alpha = 1,
      lineend = "round",
      arrow = grid::arrow(length = grid::unit(0.1, "inches"), type = "closed")
    )
  }

  layers
}

#' Resolve one set-piece row by rank, possession, or goal scorer
resolve_set_piece_row <- function(events_df,
                                 team_name,
                                 match_id = NULL,
                                 opponent_name = NULL,
                                 rank_index = 1L,
                                 possession_id = NULL,
                                 goal_minute = NULL,
                                 goal_second = NULL,
                                 scorer_name = NULL,
                                 attacking_zone_only = TRUE,
                                 max_goal_distance_m = 35) {
  if (!is.null(possession_id) || !is.null(goal_minute)) {
    set_pieces <- identify_team_set_piece_possessions(
      events_df,
      team_name = team_name,
      match_id = match_id
    )
    if (isTRUE(attacking_zone_only)) {
      set_pieces <- filter_attacking_zone_set_pieces(
        set_pieces,
        events_df = events_df,
        max_goal_distance_m = max_goal_distance_m
      )
    }

    if (!is.null(possession_id)) {
      row <- set_pieces %>%
        dplyr::filter(.data$possession == !!possession_id)
    } else {
      goals <- ensure_viz_aliases(events_df) %>%
        dplyr::filter(
          .data$`type.name` == "Shot",
          .data$`shot.outcome.name` == "Goal",
          .data$`team.name` == !!team_name,
          .data$minute == !!goal_minute
        )
      if (!is.null(goal_second)) {
        goals <- goals %>% dplyr::filter(.data$second == !!goal_second)
      }
      if (!is.null(scorer_name)) {
        goals <- goals %>%
          dplyr::filter(
            .data$`player.name` == !!scorer_name |
              .data$player_display_name == !!scorer_name
          )
      }
      if (nrow(goals) == 0) {
        stop("No matching goal found for set-piece animation.", call. = FALSE)
      }
      row <- set_pieces %>%
        dplyr::filter(.data$possession == goals$possession[1])
    }

    if (nrow(row) == 0) {
      stop("No set-piece possession found for animation.", call. = FALSE)
    }

    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = row$possession[1],
      team_name = team_name
    )
    poss_events <- events_df %>%
      dplyr::filter(.data$possession == row$possession[1])
    shot <- sequence %>% dplyr::filter(.data$type_name == "Shot")
    og_for <- poss_events %>%
      dplyr::filter(
        .data$`type.name` == "Own Goal For",
        .data$`team.name` == !!team_name
      )
    og_against <- poss_events %>%
      dplyr::filter(.data$`type.name` == "Own Goal Against")
    shot_outcome <- if (nrow(og_for) > 0) {
      "Own Goal"
    } else if (nrow(shot) > 0) {
      shot$`shot.outcome.name`[1]
    } else {
      NA_character_
    }
    shot_xg <- if (nrow(shot) > 0) {
      max(shot$`shot.statsbomb_xg`, na.rm = TRUE)
    } else {
      0
    }
    own_goal_scorer <- if (nrow(og_against) > 0) {
      og_against$`player.name`[1]
    } else {
      NA_character_
    }
    return(tibble::tibble(
      possession = row$possession[1],
      minute = row$minute[1],
      second = row$second[1],
      period = row$period[1],
      set_piece_type = row$set_piece_type[1],
      danger_score = score_set_piece_danger(sequence, events_df),
      shot_xg = shot_xg,
      shot_outcome = shot_outcome,
      own_goal_scorer = own_goal_scorer,
      key_event_id = set_piece_key_event_id(sequence)
    ))
  }

  ranked <- rank_dangerous_set_pieces(
    events_df,
    team_name = team_name,
    match_id = match_id,
    opponent_name = opponent_name,
    attacking_zone_only = attacking_zone_only,
    max_goal_distance_m = max_goal_distance_m,
    top_n = max(rank_index, 1L)
  )
  ranked[rank_index, , drop = FALSE]
}

#' Prepare one animation frame of shirt positions with numbers
prepare_set_piece_shirt_frame <- function(frame_df,
                                          attacking_lookup,
                                          defending_lookup,
                                          team_color,
                                          opponent_color,
                                          defending_asset = "white") {
  frame_df %>%
    assign_jersey_labels_to_positions(
      attacking_lookup = attacking_lookup,
      defending_lookup = defending_lookup
    ) %>%
    add_colored_shirt_icons(
      team_color = team_color,
      opponent_color = opponent_color,
      defending_asset = defending_asset
    )
}

#' Build one dangerous set-piece panel with freeze-frame player positions
build_dangerous_set_piece_panel <- function(sequence_df,
                                            network,
                                            freeze_positions,
                                            panel_title,
                                            team_color = SDC_PALETTE[["red"]],
                                            opponent_color = SDC_PALETTE[["green"]],
                                            attacking_lookup = NULL,
                                            defending_lookup = NULL,
                                            shirt_size = 0.034,
                                            gk_size = 0.042,
                                            x_min = 52,
                                            x_max = 122) {
  edges <- network$edges
  shots <- network$shots
  nodes <- network$nodes

  if (nrow(freeze_positions) > 0) {
    freeze_positions <- prepare_set_piece_shirt_frame(
      freeze_positions,
      attacking_lookup = attacking_lookup %||% tibble::tibble(),
      defending_lookup = defending_lookup %||% tibble::tibble(),
      team_color = team_color,
      opponent_color = opponent_color
    )
  }

  p <- ggplot2::ggplot()

  for (layer in set_piece_pitch_layers()) {
    p <- p + layer
  }

  if (nrow(freeze_positions) > 0) {
    if (!requireNamespace("ggimage", quietly = TRUE)) {
      install.packages("ggimage", repos = "https://cloud.r-project.org")
    }
    p <- p +
      ggimage::geom_image(
        data = freeze_positions %>%
          dplyr::filter(!.data$is_actor, !.data$is_keeper),
        ggplot2::aes(
          x = .data$pitch_x,
          y = .data$pitch_y,
          image = .data$shirt_image
        ),
        size = shirt_size,
        alpha = 0.88
      ) +
      ggimage::geom_image(
        data = freeze_positions %>%
          dplyr::filter(.data$is_keeper),
        ggplot2::aes(
          x = .data$pitch_x,
          y = .data$pitch_y,
          image = .data$shirt_image
        ),
        size = gk_size,
        alpha = 0.95
      ) +
      ggimage::geom_image(
        data = freeze_positions %>% dplyr::filter(.data$is_actor),
        ggplot2::aes(
          x = .data$pitch_x,
          y = .data$pitch_y,
          image = .data$shirt_image
        ),
        size = shirt_size * 1.1,
        alpha = 1
      )
  }

  if (nrow(edges) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(
          x = .data$x_from,
          y = .data$y_from,
          xend = .data$x_to,
          yend = .data$y_to
        ),
        colour = team_color,
        alpha = 0.9,
        linewidth = 0.7,
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.06, "inches"), type = "closed")
      )
  }

  if (nrow(nodes) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = nodes,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$player_label),
        family = SDC_FONTS$title,
        fontface = "bold",
        size = 2,
        colour = SDC_ARTICLE_COLORS$ink,
        vjust = -2.2
      )
  }

  if (nrow(shots) > 0) {
    shot_traj <- add_shot_trajectory_endpoints(
      shots %>%
        dplyr::mutate(
          `location.x` = .data$pitch_x,
          `location.y` = .data$pitch_y
        )
    ) %>%
      dplyr::mutate(
        line_colour = vapply(
          .data$`shot.outcome.name`,
          shot_trajectory_line_color,
          character(1)
        )
      )

    p <- p +
      ggplot2::geom_segment(
        data = shot_traj,
        ggplot2::aes(
          x = .data$`location.x`,
          y = .data$`location.y`,
          xend = .data$traj_xend,
          yend = .data$traj_yend,
          colour = I(.data$line_colour)
        ),
        linetype = "dashed",
        linewidth = 0.85,
        alpha = 0.95
      )
  }

  p +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / (x_max - x_min)) +
    ggplot2::labs(title = panel_title, x = NULL, y = NULL) +
    theme_sdc(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 8,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0.5,
        margin = ggplot2::margin(b = 2)
      ),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#3A7D44", colour = NA),
      plot.background = ggplot2::element_rect(fill = "#3A7D44", colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}

#' Compact legend for freeze-frame team colours
build_set_piece_position_legend <- function(team_label,
                                          team_color,
                                          opponent_label,
                                          opponent_color) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "point",
      x = c(0.04, 0.28),
      y = c(0.5, 0.5),
      colour = c(team_color, opponent_color),
      size = 3,
      alpha = 0.75
    ) +
    ggplot2::annotate(
      "text",
      x = c(0.07, 0.31),
      y = c(0.5, 0.5),
      label = c(team_label, opponent_label),
      family = SDC_FONTS$body,
      size = 3.2,
      colour = SDC_ARTICLE_COLORS$ink,
      hjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = 0.52,
      y = 0.5,
      label = "Portugal shirts · White away shirts · GK icon · Solid = passes · Dashed = shots",
      family = SDC_FONTS$body,
      size = 3,
      colour = SDC_ARTICLE_COLORS$muted,
      hjust = 0
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
    ggplot2::theme_void()
}

#' UC13: Grid of the most dangerous set pieces with player positioning
viz_dangerous_set_pieces_grid <- function(events_df,
                                          team_name,
                                          match_id = NULL,
                                          meta = NULL,
                                          lineups = NULL,
                                          opponent_name = NULL,
                                          frames_360 = NULL,
                                          team_color = SDC_PALETTE[["red"]],
                                          opponent_color = NULL,
                                          patterns = c("From Corner", "From Free Kick"),
                                          attacking_zone_only = TRUE,
                                          max_goal_distance_m = 35,
                                          top_n = 6,
                                          ncol = 3,
                                          eyebrow = "Most dangerous set pieces",
                                          title = NULL,
                                          subtitle = NULL,
                                          caption = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(opponent_name) && !is.null(meta)) {
    opponent_name <- if (identical(team_name, meta$home_team)) {
      meta$away_team
    } else {
      meta$home_team
    }
  }

  if (is.null(opponent_color)) {
    opponent_color <- SDC_PALETTE[["green"]]
  }

  if (is.null(frames_360) && !is.null(match_id)) {
    frames_360 <- load_statsbomb_360_frames(match_id)
  }
  frames_360 <- frames_360 %||% list(by_event_id = list())

  ranked <- rank_dangerous_set_pieces(
    events_df,
    team_name = team_name,
    match_id = match_id,
    opponent_name = opponent_name,
    patterns = patterns,
    attacking_zone_only = attacking_zone_only,
    max_goal_distance_m = max_goal_distance_m,
    top_n = top_n
  )

  plots <- purrr::map(seq_len(nrow(ranked)), function(i) {
    row <- ranked[i, , drop = FALSE]
    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = row$possession,
      team_name = team_name
    )
    network <- compute_set_piece_sequence_network(sequence, events_df)
    frame_entry <- frames_360$by_event_id[[row$key_event_id]]
    freeze_positions <- parse_freeze_frame_positions(
      frame_entry,
      events_df = events_df,
      attacking_team_name = team_name,
      period = row$period
    )
    poss_events <- events_df %>%
      dplyr::filter(.data$possession == row$possession)
    attacking_lookup = if (!is.null(lineups)) {
      build_team_jersey_lookup(
        poss_events,
        lineups,
        team_name,
        events_df,
        row$period,
        normalize_team_name = team_name
      )
    } else {
      NULL
    }
    defending_lookup <- if (!is.null(lineups) && !is.null(opponent_name)) {
      build_team_jersey_lookup(
        poss_events,
        lineups,
        opponent_name,
        events_df,
        row$period,
        normalize_team_name = team_name
      )
    } else {
      NULL
    }

    build_dangerous_set_piece_panel(
      sequence_df = sequence,
      network = network,
      freeze_positions = freeze_positions,
      panel_title = dangerous_set_piece_panel_title(row),
      team_color = team_color,
      opponent_color = opponent_color,
      attacking_lookup = attacking_lookup,
      defending_lookup = defending_lookup
    )
  })

  n_panels <- length(plots)
  ncol_eff <- min(ncol, n_panels)
  grid <- patchwork::wrap_plots(plots, ncol = ncol_eff)

  team_label <- if (!is.null(meta)) {
    if (identical(team_name, meta$home_team)) meta$display_home else meta$display_away
  } else {
    team_name
  }
  opponent_label <- if (!is.null(meta) && !is.null(opponent_name)) {
    if (identical(opponent_name, meta$home_team)) meta$display_home else meta$display_away
  } else {
    opponent_name %||% "Opponent"
  }

  if (is.null(title)) {
    title <- paste0(team_label, " set pieces that created the best chances")
  }
  if (is.null(subtitle) && !is.null(meta)) {
    subtitle <- match_score_line(meta)
  }
  if (is.null(caption)) {
    has_frames <- nrow(ranked) > 0 && any(
      ranked$key_event_id %in% names(frames_360$by_event_id)
    )
    frame_note <- if (has_frames) {
      paste0(
        "Player positions from match freeze frames at the shot or final pass. ",
        team_label,
        " (",
        team_color,
        ") vs ",
        opponent_label,
        " (",
        opponent_color,
        "). "
      )
    } else {
      ""
    }
    caption <- paste0(
      frame_note,
      "Ranked by shot xG, then penalty-box entries. ",
      "Shirts show all visible players; numbers matched from lineups where possible. ",
      "Portugal and Uzbekistan kit icons; goalkeeper shown with gloves."
    )
  }

  header <- build_decisive_sequence_header(
    eyebrow = eyebrow,
    headline = title,
    subtitle = subtitle %||% ""
  )

  legend_row <- build_set_piece_position_legend(
    team_label = team_label,
    team_color = team_color,
    opponent_label = opponent_label,
    opponent_color = opponent_color
  )

  patchwork::wrap_plots(
    header,
    legend_row,
    grid,
    ncol = 1,
    heights = c(0.11, 0.03, 0.86)
  ) +
    patchwork::plot_annotation(
      caption = caption,
      theme = theme_sdc_article(base_size = 10) +
        ggplot2::theme(
          plot.caption = ggplot2::element_text(
            family = SDC_FONTS$body,
            size = 8.5,
            colour = SDC_ARTICLE_COLORS$muted,
            hjust = 0,
            margin = ggplot2::margin(t = 6)
          ),
          plot.background = ggplot2::element_rect(
            fill = SDC_ARTICLE_COLORS$offwhite,
            colour = NA
          ),
          plot.margin = ggplot2::margin(12, 14, 8, 14)
        )
    )
}

#' Build animation data for one ranked set piece
prepare_set_piece_animation_data <- function(events_df,
                                             set_piece_row,
                                             team_name,
                                             opponent_name,
                                             lineups,
                                             frames_360,
                                             frames_per_pass = 11L,
                                             frames_per_shot = 14L,
                                             hold_at_end = 6L,
                                             max_keyframes = 16) {
  keyframes <- build_set_piece_keyframes(
    events_df,
    possession_id = set_piece_row$possession,
    team_name = team_name,
    period = set_piece_row$period,
    frames_360 = frames_360,
    max_keyframes = max_keyframes
  )
  poss_events <- events_df %>%
    dplyr::filter(.data$possession == set_piece_row$possession)
  sequence <- extract_set_piece_sequence(
    events_df,
    possession_id = set_piece_row$possession,
    team_name = team_name
  )
  network <- compute_set_piece_sequence_network(sequence, events_df)
  if (!is.na(set_piece_row$shot_outcome) &&
      identical(set_piece_row$shot_outcome, "Own Goal") &&
      nrow(network$shots) > 0) {
    network$shots <- network$shots %>%
      dplyr::mutate(`shot.outcome.name` = "Own Goal")
  }
  segments <- build_set_piece_ball_segments(network)
  ball_states <- compute_ball_states_for_animation(
    segments,
    frames_per_pass = frames_per_pass,
    frames_per_shot = frames_per_shot,
    hold_at_end = hold_at_end
  )
  anim_ids <- ball_states$anim_frame
  animation <- resample_player_positions_for_animation(keyframes, anim_ids)
  attacking_lookup <- build_team_jersey_lookup(
    poss_events,
    lineups,
    team_name,
    events_df,
    set_piece_row$period,
    normalize_team_name = team_name
  )
  actor_pins <- build_sequence_actor_pins(network, attacking_lookup)

  list(
    animation = animation,
    ball_states = ball_states,
    actor_pins = actor_pins,
    ball_icon = colored_ball_icon_path("#000000"),
    attacking_lookup = attacking_lookup,
    defending_lookup = build_team_jersey_lookup(
      poss_events,
      lineups,
      opponent_name,
      events_df,
      set_piece_row$period,
      normalize_team_name = team_name
    ),
    sequence = sequence,
    network = network
  )
}

#' One animation frame for a dangerous set piece
build_set_piece_animation_frame <- function(animation_df,
                                            anim_frame,
                                            network,
                                            panel_title,
                                            team_color,
                                            opponent_color,
                                            attacking_lookup,
                                            defending_lookup,
                                            ball_state = NULL,
                                            ball_icon = NULL,
                                            actor_pins = NULL,
                                            shirt_size = 0.036,
                                            gk_size = 0.052,
                                            ball_size = 0.022,
                                            defending_asset = "white",
                                            pitch_style = c("green", "article"),
                                            compact = FALSE,
                                            x_min = 52,
                                            x_max = 122) {
  pitch_style <- match.arg(pitch_style)
  if (isTRUE(compact)) {
    shirt_size <- 0.032
    gk_size <- 0.045
    ball_size <- 0.018
  }
  panel_fill <- if (identical(pitch_style, "article")) {
    SDC_ARTICLE_COLORS$pitch
  } else {
    "#3A7D44"
  }
  title_size <- if (isTRUE(compact)) 5.2 else 14
  frame_positions <- animation_df %>%
    dplyr::filter(.data$anim_frame == !!anim_frame)
  frame_positions <- merge_sequence_actor_pins(
    frame_positions,
    actor_pins %||% tibble::tibble(),
    ball_state
  )
  shirts <- prepare_set_piece_shirt_frame(
    frame_positions,
    attacking_lookup = attacking_lookup,
    defending_lookup = defending_lookup,
    team_color = team_color,
    opponent_color = opponent_color,
    defending_asset = defending_asset
  )

  p <- ggplot2::ggplot()
  for (layer in get_set_piece_pitch_layers(pitch_style)) {
    p <- p + layer
  }

  edges <- network$edges
  pass_color <- if (identical(pitch_style, "article")) {
    SDC_PALETTE[["blue"]]
  } else {
    team_color
  }
  for (layer in draw_animation_pass_layers(edges, ball_state, pass_color)) {
    p <- p + layer
  }

  for (layer in draw_animation_shot_layers(network$shots, ball_state, pitch_style)) {
    p <- p + layer
  }

  if (nrow(shirts) > 0) {
    if (!requireNamespace("ggimage", quietly = TRUE)) {
      install.packages("ggimage", repos = "https://cloud.r-project.org")
    }
    p <- p +
      ggimage::geom_image(
        data = shirts %>%
          dplyr::filter(!.data$is_actor, !.data$is_keeper),
        ggplot2::aes(
          x = .data$pitch_x,
          y = .data$pitch_y,
          image = .data$shirt_image
        ),
        size = shirt_size,
        alpha = 0.88
      ) +
      ggimage::geom_image(
        data = shirts %>% dplyr::filter(.data$is_keeper),
        ggplot2::aes(
          x = .data$pitch_x,
          y = .data$pitch_y,
          image = .data$shirt_image
        ),
        size = gk_size,
        alpha = 1
      ) +
      ggimage::geom_image(
        data = shirts %>% dplyr::filter(.data$is_actor),
        ggplot2::aes(
          x = .data$pitch_x,
          y = .data$pitch_y,
          image = .data$shirt_image
        ),
        size = shirt_size * 1.1,
        alpha = 1
      )
  }

  if (!is.null(ball_state) && nrow(ball_state) > 0 &&
      !is.na(ball_state$ball_x[1]) && !is.null(ball_icon)) {
    ensure_ball_icon()
    ball_df <- tibble::tibble(
      ball_x = ball_state$ball_x,
      ball_y = ball_state$ball_y,
      ball_image = ball_icon
    )
    p <- p +
      ggimage::geom_image(
        data = ball_df,
        ggplot2::aes(
          x = .data$ball_x,
          y = .data$ball_y,
          image = .data$ball_image
        ),
        size = ball_size,
        alpha = 1
      )
  }

  p +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / (x_max - x_min)) +
    ggplot2::labs(title = panel_title, x = NULL, y = NULL) +
    theme_sdc(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = title_size,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0.5,
        margin = ggplot2::margin(b = if (isTRUE(compact)) 1 else 4)
      ),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = panel_fill, colour = NA),
      plot.background = ggplot2::element_rect(fill = panel_fill, colour = NA),
      plot.margin = ggplot2::margin(
        if (isTRUE(compact)) 2 else 8,
        if (isTRUE(compact)) 2 else 8,
        if (isTRUE(compact)) 2 else 8,
        if (isTRUE(compact)) 2 else 8
      )
    )
}

#' Save animated set-piece GIF (4:5 article format by default)
save_dangerous_set_piece_gif <- function(events_df,
                                         team_name,
                                         match_id,
                                         meta = NULL,
                                         lineups = NULL,
                                         opponent_name = NULL,
                                         rank_index = 1L,
                                         possession_id = NULL,
                                         goal_minute = NULL,
                                         goal_second = NULL,
                                         scorer_name = NULL,
                                         path,
                                         frames_360 = NULL,
                                         team_color = SDC_PALETTE[["red"]],
                                         opponent_color = "#1EB53A",
                                         frames_per_pass = 11L,
                                         frames_per_shot = 14L,
                                         hold_at_end = 6L,
                                         max_keyframes = 14,
                                         fps = 10,
                                         defending_asset = "white",
                                         width_px = 864,
                                         height_px = 1080,
                                         dpi = 96,
                                         attacking_zone_only = TRUE,
                                         max_goal_distance_m = 35,
                                         eyebrow = "Set piece sequence",
                                         headline_prefix = NULL) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    install.packages("magick", repos = "https://cloud.r-project.org")
  }

  if (is.null(opponent_name) && !is.null(meta)) {
    opponent_name <- if (identical(team_name, meta$home_team)) {
      meta$away_team
    } else {
      meta$home_team
    }
  }
  if (is.null(frames_360) && !is.null(match_id)) {
    frames_360 <- load_statsbomb_360_frames(match_id)
  }
  if (is.null(lineups)) {
    stop("lineups are required for shirt numbers in animation.", call. = FALSE)
  }

  row <- resolve_set_piece_row(
    events_df,
    team_name = team_name,
    match_id = match_id,
    opponent_name = opponent_name,
    rank_index = rank_index,
    possession_id = possession_id,
    goal_minute = goal_minute,
    goal_second = goal_second,
    scorer_name = scorer_name,
    attacking_zone_only = attacking_zone_only,
    max_goal_distance_m = max_goal_distance_m
  )
  prep <- prepare_set_piece_animation_data(
    events_df,
    set_piece_row = row,
    team_name = team_name,
    opponent_name = opponent_name,
    lineups = lineups,
    frames_360 = frames_360,
    frames_per_pass = frames_per_pass,
    frames_per_shot = frames_per_shot,
    hold_at_end = hold_at_end,
    max_keyframes = max_keyframes
  )

  anim_ids <- sort(unique(prep$animation$anim_frame))
  panel_title <- dangerous_set_piece_panel_title(row)
  team_label <- if (!is.null(meta)) {
    if (identical(team_name, meta$home_team)) meta$display_home else meta$display_away
  } else {
    team_name
  }
  subtitle <- if (!is.null(meta)) match_score_line(meta) else ""

  tmp_dir <- tempfile("set_piece_anim_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  width_in <- width_px / dpi
  height_in <- height_px / dpi

  for (i in seq_along(anim_ids)) {
    ball_state <- prep$ball_states %>%
      dplyr::filter(.data$anim_frame == anim_ids[i]) %>%
      dplyr::slice(1)
    pitch_plot <- build_set_piece_animation_frame(
      animation_df = prep$animation,
      anim_frame = anim_ids[i],
      network = prep$network,
      panel_title = panel_title,
      team_color = team_color,
      opponent_color = opponent_color,
      attacking_lookup = prep$attacking_lookup,
      defending_lookup = prep$defending_lookup,
      ball_state = if (nrow(ball_state) > 0) ball_state else NULL,
      ball_icon = prep$ball_icon,
      actor_pins = prep$actor_pins,
      defending_asset = defending_asset
    )
    frame_plot <- patchwork::wrap_plots(
      build_decisive_sequence_header(
        eyebrow = eyebrow,
        headline = paste0(
          headline_prefix %||% team_label,
          " · ",
          panel_title
        ),
        subtitle = subtitle
      ),
      pitch_plot,
      ncol = 1,
      heights = c(0.14, 0.86)
    ) +
      patchwork::plot_annotation(
        caption = paste0(
          "Player movement · Green pitch · ",
          "Portugal shirts · White away shirts · GK icon (gk.svg)"
        ),
        theme = theme_sdc_article(base_size = 10) +
          ggplot2::theme(
            plot.caption = ggplot2::element_text(
              family = SDC_FONTS$body,
              size = 9,
              colour = SDC_ARTICLE_COLORS$muted,
              hjust = 0.5
            ),
            plot.background = ggplot2::element_rect(
              fill = SDC_ARTICLE_COLORS$offwhite,
              colour = NA
            ),
            plot.margin = ggplot2::margin(10, 12, 8, 12)
          )
      )

    ggplot2::ggsave(
      filename = file.path(tmp_dir, sprintf("frame_%03d.png", i)),
      plot = frame_plot,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = dpi,
      bg = SDC_ARTICLE_COLORS$offwhite
    )
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  frame_paths <- file.path(tmp_dir, sprintf("frame_%03d.png", seq_along(anim_ids)))
  frames <- purrr::reduce(
    purrr::map(frame_paths, magick::image_read),
    c
  )
  gif <- magick::image_animate(frames, fps = fps)
  magick::image_write(gif, path)
  invisible(path)
}

#' Build panel title for faceted set-piece animation
format_faceted_set_piece_title <- function(set_piece_row, headline = NULL) {
  if (!is.null(headline) && nzchar(headline)) {
    type_label <- dplyr::case_when(
      identical(set_piece_row$set_piece_type, "From Free Kick") ~ "FK",
      identical(set_piece_row$set_piece_type, "From Corner") ~ "Corner",
      TRUE ~ set_piece_row$set_piece_type
    )
    minute_label <- paste0(set_piece_row$minute, "'")
    outcome_bits <- character()
    if (!is.na(set_piece_row$shot_outcome)) {
      if (identical(set_piece_row$shot_outcome, "Own Goal") &&
          !is.na(set_piece_row$own_goal_scorer)) {
        outcome_bits <- c(outcome_bits, paste0(set_piece_row$own_goal_scorer, " own goal"))
      } else {
        outcome_bits <- c(outcome_bits, set_piece_row$shot_outcome)
      }
    }
    if (!is.na(set_piece_row$shot_xg) && set_piece_row$shot_xg > 0) {
      outcome_bits <- c(
        outcome_bits,
        paste0(format(round(set_piece_row$shot_xg, 2), nsmall = 2), " xG")
      )
    }
    suffix <- if (length(outcome_bits) > 0) {
      paste0(" · ", paste(outcome_bits, collapse = " · "))
    } else {
      ""
    }
    return(paste(headline, type_label, minute_label, suffix, sep = " · "))
  }
  dangerous_set_piece_panel_title(set_piece_row)
}

#' Prepare synced animation data for multiple set-piece panels
prepare_faceted_set_piece_animations <- function(events_df,
                                                 team_name,
                                                 panels,
                                                 opponent_name,
                                                 lineups,
                                                 frames_360,
                                                 frames_per_pass = 11L,
                                                 frames_per_shot = 14L,
                                                 hold_at_end = 6L,
                                                 max_keyframes = 16,
                                                 attacking_zone_only = TRUE,
                                                 max_goal_distance_m = 35) {
  purrr::imap(panels, function(panel, panel_id) {
    row <- resolve_set_piece_row(
      events_df,
      team_name = team_name,
      possession_id = panel$possession_id,
      attacking_zone_only = attacking_zone_only,
      max_goal_distance_m = max_goal_distance_m
    )
    if (!is.null(panel$panel_title)) {
      row$panel_headline <- panel$panel_title
    } else if (!is.null(panel$headline)) {
      row$panel_headline <- format_faceted_set_piece_title(row, headline = panel$headline)
    }
    prep <- prepare_set_piece_animation_data(
      events_df,
      set_piece_row = row,
      team_name = team_name,
      opponent_name = opponent_name,
      lineups = lineups,
      frames_360 = frames_360,
      frames_per_pass = frames_per_pass,
      frames_per_shot = frames_per_shot,
      hold_at_end = hold_at_end,
      max_keyframes = max_keyframes
    )
    list(
      panel_id = panel_id,
      row = row,
      prep = prep,
      panel_title = dangerous_set_piece_panel_title(row),
      n_frames = max(prep$ball_states$anim_frame)
    )
  })
}

#' Save one faceted animated GIF with multiple set-piece panels
save_faceted_set_pieces_gif <- function(events_df,
                                        team_name,
                                        match_id,
                                        panels,
                                        path,
                                        meta = NULL,
                                        lineups = NULL,
                                        opponent_name = NULL,
                                        frames_360 = NULL,
                                        team_color = SDC_PALETTE[["red"]],
                                        opponent_color = "#1EB53A",
                                        frames_per_pass = 11L,
                                        frames_per_shot = 14L,
                                        hold_at_end = 6L,
                                        max_keyframes = 16,
                                        fps = 10,
                                        defending_asset = "uzbek",
                                        width_px = 864,
                                        height_px = 1080,
                                        dpi = 96,
                                        facet_ncol = 3L,
                                        attacking_zone_only = TRUE,
                                        max_goal_distance_m = 35,
                                        eyebrow = "Dangerous set pieces",
                                        headline = NULL,
                                        subtitle = NULL) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    install.packages("magick", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(opponent_name) && !is.null(meta)) {
    opponent_name <- if (identical(team_name, meta$home_team)) {
      meta$away_team
    } else {
      meta$home_team
    }
  }
  if (is.null(frames_360) && !is.null(match_id)) {
    frames_360 <- load_statsbomb_360_frames(match_id)
  }
  if (is.null(lineups)) {
    stop("lineups are required for shirt numbers in animation.", call. = FALSE)
  }

  panel_data <- prepare_faceted_set_piece_animations(
    events_df = events_df,
    team_name = team_name,
    panels = panels,
    opponent_name = opponent_name,
    lineups = lineups,
    frames_360 = frames_360,
    frames_per_pass = frames_per_pass,
    frames_per_shot = frames_per_shot,
    hold_at_end = hold_at_end,
    max_keyframes = max_keyframes,
    attacking_zone_only = attacking_zone_only,
    max_goal_distance_m = max_goal_distance_m
  )

  max_frames <- max(vapply(panel_data, function(x) x$n_frames, integer(1)))
  team_label <- if (!is.null(meta)) {
    if (identical(team_name, meta$home_team)) meta$display_home else meta$display_away
  } else {
    team_name
  }
  headline <- headline %||% paste0(team_label, " · dangerous set pieces")
  subtitle <- subtitle %||% if (!is.null(meta)) match_score_line(meta) else ""

  tmp_dir <- tempfile("faceted_set_piece_anim_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  width_in <- width_px / dpi
  height_in <- height_px / dpi
  n_panels <- length(panel_data)

  for (i in seq_len(max_frames)) {
    pitch_plots <- purrr::map(panel_data, function(panel) {
      af <- min(i, panel$n_frames)
      ball_state <- panel$prep$ball_states %>%
        dplyr::filter(.data$anim_frame == af) %>%
        dplyr::slice(1)
      build_set_piece_animation_frame(
        animation_df = panel$prep$animation,
        anim_frame = af,
        network = panel$prep$network,
        panel_title = panel$panel_title,
        team_color = team_color,
        opponent_color = opponent_color,
        attacking_lookup = panel$prep$attacking_lookup,
        defending_lookup = panel$prep$defending_lookup,
        ball_state = if (nrow(ball_state) > 0) ball_state else NULL,
        ball_icon = panel$prep$ball_icon,
        actor_pins = panel$prep$actor_pins,
        defending_asset = defending_asset,
        pitch_style = "article",
        compact = TRUE
      )
    })

    frame_plot <- patchwork::wrap_plots(
      build_decisive_sequence_header(
        eyebrow = eyebrow,
        headline = headline,
        subtitle = subtitle
      ),
      patchwork::wrap_plots(pitch_plots, ncol = facet_ncol),
      ncol = 1,
      heights = c(0.14, 0.86)
    ) +
      patchwork::plot_annotation(
        caption = paste0(
          "Player movement · White pitch · ",
          "Blue pass arrows · Orange goals · Purple saved shots · ",
          "Uzbekistan in light blue"
        ),
        theme = theme_sdc_article(base_size = 10) +
          ggplot2::theme(
            plot.caption = ggplot2::element_text(
              family = SDC_FONTS$body,
              size = 8.5,
              colour = SDC_ARTICLE_COLORS$muted,
              hjust = 0.5
            ),
            plot.background = ggplot2::element_rect(
              fill = SDC_ARTICLE_COLORS$offwhite,
              colour = NA
            ),
            plot.margin = ggplot2::margin(10, 12, 8, 12)
          )
      )

    ggplot2::ggsave(
      filename = file.path(tmp_dir, sprintf("frame_%03d.png", i)),
      plot = frame_plot,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = dpi,
      bg = SDC_ARTICLE_COLORS$offwhite
    )
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  frame_paths <- file.path(tmp_dir, sprintf("frame_%03d.png", seq_len(max_frames)))
  frames <- purrr::reduce(
    purrr::map(frame_paths, magick::image_read),
    c
  )
  gif <- magick::image_animate(frames, fps = fps)
  magick::image_write(gif, path)
  invisible(path)
}

#' Classify corner flag side from set-piece starting coordinates
classify_corner_side <- function(x, y, attacks_high_x = TRUE) {
  if (is.na(x) || is.na(y)) {
    return(NA_character_)
  }
  norm <- normalize_opponent_half_coords(x, y, attacks_high_x)
  dplyr::if_else(norm$y <= 40, "Left", "Right")
}

#' Summarize every team set piece in one match
summarize_team_set_piece_performance <- function(events_df,
                                               team_name,
                                               match_id = NULL,
                                               patterns = c("From Corner", "From Free Kick"),
                                               attacking_zone_only = TRUE,
                                               max_goal_distance_m = 35) {
  set_pieces <- identify_team_set_piece_possessions(
    events_df,
    team_name = team_name,
    match_id = match_id,
    patterns = patterns
  )

  if (isTRUE(attacking_zone_only)) {
    set_pieces <- filter_attacking_zone_set_pieces(
      set_pieces,
      events_df = events_df,
      max_goal_distance_m = max_goal_distance_m
    )
  }

  if (nrow(set_pieces) == 0) {
    stop("No set-piece possessions found for ", team_name, call. = FALSE)
  }

  direction <- infer_team_attacking_high_x(events_df)

  possessions <- purrr::map_dfr(seq_len(nrow(set_pieces)), function(i) {
    sp <- set_pieces[i, , drop = FALSE]
    attacks_high_x <- direction %>%
      dplyr::filter(
        .data$`team.name` == !!team_name,
        .data$period == sp$period
      ) %>%
      dplyr::pull(.data$attacks_high_x) %>%
      dplyr::first()
    attacks_high_x <- dplyr::coalesce(attacks_high_x, TRUE)

    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = sp$possession,
      team_name = team_name
    )
    poss_events <- events_df %>%
      dplyr::filter(.data$possession == sp$possession)
    passes <- sequence %>%
      dplyr::filter(
        .data$type_name == "Pass",
        is.na(.data$`pass.outcome.name`)
      )
    shot <- sequence %>% dplyr::filter(.data$type_name == "Shot")
    og_for <- poss_events %>%
      dplyr::filter(
        .data$`type.name` == "Own Goal For",
        .data$`team.name` == !!team_name
      )
    shot_xg <- if (nrow(shot) > 0) {
      max(shot$`shot.statsbomb_xg`, na.rm = TRUE)
    } else {
      0
    }
    ended_shot <- nrow(shot) > 0
    ended_goal <- nrow(og_for) > 0 ||
      (nrow(shot) > 0 && identical(shot$`shot.outcome.name`[1], "Goal"))
    outcome_bucket <- dplyr::case_when(
      ended_goal ~ "Goal",
      ended_shot ~ "Shot (no goal)",
      TRUE ~ "No shot"
    )
    corner_side <- if (identical(sp$set_piece_type, "Corner")) {
      classify_corner_side(
        sp$`location.x`,
        sp$`location.y`,
        attacks_high_x = attacks_high_x
      )
    } else {
      NA_character_
    }

    tibble::tibble(
      possession = sp$possession,
      minute = sp$minute,
      set_piece_type = sp$set_piece_type,
      corner_side = corner_side,
      completed_passes = nrow(passes),
      ended_shot = ended_shot,
      ended_goal = ended_goal,
      shot_xg = shot_xg,
      outcome_bucket = outcome_bucket,
      lost_possession = !ended_shot,
      direct_shot = ended_shot && nrow(passes) == 0L
    )
  })

  shot_possessions <- possessions %>% dplyr::filter(.data$ended_shot)
  lost_possessions <- possessions %>% dplyr::filter(.data$lost_possession)

  list(
    possessions = possessions,
    total_set_pieces = nrow(possessions),
    n_corners = sum(possessions$set_piece_type == "Corner"),
    n_free_kicks = sum(possessions$set_piece_type == "Free kick"),
    corners_left = sum(possessions$corner_side == "Left", na.rm = TRUE),
    corners_right = sum(possessions$corner_side == "Right", na.rm = TRUE),
    with_shot = sum(possessions$ended_shot),
    with_goal = sum(possessions$ended_goal),
    sequences_lost = sum(possessions$lost_possession),
    direct_shots = sum(possessions$direct_shot, na.rm = TRUE),
    completed_passes = sum(possessions$completed_passes),
    total_xg = sum(possessions$shot_xg),
    shot_rate = sum(possessions$ended_shot) / nrow(possessions),
    avg_passes = mean(possessions$completed_passes),
    avg_passes_to_shot = if (nrow(shot_possessions) > 0) {
      mean(shot_possessions$completed_passes)
    } else {
      NA_real_
    },
    avg_passes_before_loss = if (nrow(lost_possessions) > 0) {
      mean(lost_possessions$completed_passes)
    } else {
      NA_real_
    },
    max_passes = max(possessions$completed_passes)
  )
}

#' One set-piece passing network with freeze-frame player points
build_set_piece_network_panel <- function(sequence_df,
                                          network,
                                          freeze_positions,
                                          panel_title,
                                          actor_pins = NULL,
                                          team_color = SDC_PALETTE[["red"]],
                                          opponent_color = SDC_PALETTE[["cyan"]],
                                          pass_color = SDC_PALETTE[["blue"]],
                                          goal_color = SDC_PALETTE[["orange"]],
                                          saved_shot_color = SDC_PALETTE[["purple"]],
                                          x_min = 52,
                                          x_max = 122) {
  edges <- network$edges
  shots <- network$shots
  player_positions <- merge_static_sequence_actor_pins(
    freeze_positions %||% tibble::tibble(),
    actor_pins %||% tibble::tibble()
  )

  if (nrow(edges) > 0) {
    edges <- edges %>%
      dplyr::mutate(
        pass_number = dplyr::row_number(),
        marker_x = .data$x_from + 0.58 * (.data$x_to - .data$x_from),
        marker_y = .data$y_from + 0.58 * (.data$y_to - .data$y_from)
      )
  }

  if (nrow(shots) > 0) {
    shots <- add_shot_trajectory_endpoints(
      shots %>%
        dplyr::mutate(
          `location.x` = .data$pitch_x,
          `location.y` = .data$pitch_y
        )
    ) %>%
      dplyr::mutate(
        shot_color = dplyr::case_when(
          .data$`shot.outcome.name` %in% c("Goal", "Own Goal") ~ goal_color,
          .data$`shot.outcome.name` %in% c("Saved", "Blocked") ~ saved_shot_color,
          TRUE ~ SDC_PALETTE[["red"]]
        ),
        line_type = dplyr::if_else(
          .data$`shot.outcome.name` %in% c("Goal", "Own Goal"),
          "solid",
          "22"
        )
      )
  }

  p <- ggplot2::ggplot()
  for (layer in draw_decisive_sequence_pitch_layers(
    line_colour = SDC_ARTICLE_COLORS$grid,
    pitch_fill = SDC_ARTICLE_COLORS$pitch
  )) {
    p <- p + layer
  }

  defenders <- tibble::tibble()
  attackers <- tibble::tibble()
  actors <- tibble::tibble()
  keepers <- tibble::tibble()

  if (nrow(player_positions) > 0) {
    defenders <- player_positions %>%
      dplyr::filter(.data$team_side == "defending", !isTRUE(.data$is_keeper))
    attackers <- player_positions %>%
      dplyr::filter(
        .data$team_side == "attacking",
        !isTRUE(.data$is_keeper),
        !isTRUE(.data$is_actor)
      )
    actors <- player_positions %>%
      dplyr::filter(
        .data$team_side == "attacking",
        !isTRUE(.data$is_keeper),
        isTRUE(.data$is_actor)
      )
    keepers <- player_positions %>% dplyr::filter(isTRUE(.data$is_keeper))

    if (nrow(defenders) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = defenders,
          ggplot2::aes(x = .data$pitch_x, y = .data$pitch_y),
          colour = opponent_color,
          fill = opponent_color,
          shape = 21,
          size = 1.55,
          alpha = 0.82,
          stroke = 0.25
        )
    }
    if (nrow(attackers) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = attackers,
          ggplot2::aes(x = .data$pitch_x, y = .data$pitch_y),
          colour = team_color,
          fill = team_color,
          shape = 21,
          size = 1.75,
          alpha = 0.92,
          stroke = 0.3
        )
    }
    if (nrow(keepers) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = keepers,
          ggplot2::aes(x = .data$pitch_x, y = .data$pitch_y),
          colour = SDC_ARTICLE_COLORS$ink,
          fill = "white",
          shape = 21,
          size = 2,
          alpha = 1,
          stroke = 0.45
        )
    }
  }

  if (nrow(edges) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(
          x = .data$x_from,
          y = .data$y_from,
          xend = .data$x_to,
          yend = .data$y_to
        ),
        colour = pass_color,
        linewidth = 0.55,
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.05, "inches"), type = "closed")
      ) +
      ggplot2::geom_point(
        data = edges,
        ggplot2::aes(x = .data$marker_x, y = .data$marker_y),
        shape = 21,
        size = 1.65,
        fill = pass_color,
        colour = "white",
        stroke = 0.35
      ) +
      ggplot2::geom_text(
        data = edges,
        ggplot2::aes(x = .data$marker_x, y = .data$marker_y, label = .data$pass_number),
        family = SDC_FONTS$title,
        fontface = "bold",
        size = 1.85,
        colour = "white"
      )
  }

  if (nrow(shots) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = shots,
        ggplot2::aes(
          x = .data$`location.x`,
          y = .data$`location.y`,
          xend = .data$traj_xend,
          yend = .data$traj_yend,
          colour = I(.data$shot_color),
          linetype = I(.data$line_type)
        ),
        linewidth = 0.65,
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.05, "inches"), type = "closed")
      )
  }

  if (nrow(actors) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = actors,
        ggplot2::aes(x = .data$pitch_x, y = .data$pitch_y),
        colour = team_color,
        fill = team_color,
        shape = 21,
        size = 2.15,
        alpha = 1,
        stroke = 0.5
      )
  }

  p +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / (x_max - x_min)) +
    ggplot2::labs(title = panel_title, x = NULL, y = NULL) +
    theme_sdc(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 9,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0.5,
        lineheight = 0.9,
        margin = ggplot2::margin(b = 5, t = 2)
      ),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = SDC_ARTICLE_COLORS$pitch,
        colour = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = SDC_ARTICLE_COLORS$pitch,
        colour = NA
      ),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}

#' Colours for corner vs free-kick set pieces on combined pitch views
set_piece_type_colors <- function() {
  c(
    "Corner" = SDC_PALETTE[["purple"]],
    "Free kick" = SDC_PALETTE[["blue"]]
  )
}

#' Non-red SDC colours for the four shot-ending set-piece routines
SET_PIECE_SHOT_ROUTINE_COLORS <- c(
  SDC_PALETTE[["blue"]],
  SDC_PALETTE[["orange"]],
  SDC_PALETTE[["green"]],
  SDC_PALETTE[["purple"]]
)

#' Shot-ending possessions only, in chronological order
filter_shot_set_piece_possessions <- function(possessions_df) {
  possessions_df %>%
    dplyr::filter(.data$ended_shot | .data$shot_xg > 0) %>%
    dplyr::arrange(.data$minute, .data$possession)
}

#' Palette for shot routines numbered 1–4 (heatmap markers, table, arrows)
assign_shot_routine_palette <- function(possessions_df,
                                        colors = SET_PIECE_SHOT_ROUTINE_COLORS) {
  shot_df <- filter_shot_set_piece_possessions(possessions_df)
  if (nrow(shot_df) == 0L) {
    return(tibble::tibble(
      possession = integer(),
      routine_id = integer(),
      routine_color = character()
    ))
  }

  tibble::tibble(
    possession = shot_df$possession,
    routine_id = seq_len(nrow(shot_df)),
    routine_color = colors[seq_len(nrow(shot_df))]
  )
}

#' Chronological routine IDs for heatmap markers and reference table
assign_set_piece_routine_ids <- function(possessions_df) {
  possessions_df %>%
    dplyr::arrange(.data$minute, .data$possession) %>%
    dplyr::mutate(routine_id = dplyr::row_number())
}

#' One distinct SDC palette colour per numbered routine (no red)
set_piece_routine_palette <- function(possessions_df,
                                      colors = SET_PIECE_SHOT_ROUTINE_COLORS) {
  assign_shot_routine_palette(possessions_df, colors = colors)
}

#' Display label for set-piece type in tables
format_set_piece_type_label <- function(set_piece_types) {
  dplyr::case_when(
    set_piece_types == "Corner" ~ "Corner kick",
    set_piece_types == "Free kick" ~ "Free kick",
    TRUE ~ set_piece_types
  )
}

#' Compact table of shot-ending routines (#, type, xG) for the pie column
build_set_piece_shot_reference_table <- function(possessions_df) {
  ref_df <- filter_shot_set_piece_possessions(possessions_df) %>%
    dplyr::left_join(assign_shot_routine_palette(possessions_df), by = "possession") %>%
    dplyr::mutate(
      type_label = format_set_piece_type_label(.data$set_piece_type),
      xg_label = format(round(.data$shot_xg, 2), nsmall = 2)
    )

  if (nrow(ref_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  col_x <- c("#" = 0.12, "Type" = 0.46, "xG" = 0.82)
  header_y <- 0.92
  row_gap <- 0.18
  row_ys <- header_y - 0.14 - (seq_len(nrow(ref_df)) - 1L) * row_gap

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = unname(col_x),
      y = header_y,
      label = names(col_x),
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 2.6,
      colour = SDC_ARTICLE_COLORS$ink
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.04,
      xend = 0.96,
      y = header_y - 0.06,
      yend = header_y - 0.06,
      colour = SDC_ARTICLE_COLORS$grid,
      linewidth = 0.35
    )

  for (i in seq_len(nrow(ref_df))) {
    row <- ref_df[i, , drop = FALSE]
    p <- p +
      ggplot2::annotate(
        "text",
        x = col_x[["#"]],
        y = row_ys[i],
        label = row$routine_id,
        family = SDC_FONTS$title,
        fontface = "bold",
        size = 2.8,
        colour = row$routine_color
      ) +
      ggplot2::annotate(
        "text",
        x = col_x[["Type"]],
        y = row_ys[i],
        label = row$type_label,
        family = SDC_FONTS$body,
        size = 2.5,
        colour = SDC_ARTICLE_COLORS$ink
      ) +
      ggplot2::annotate(
        "text",
        x = col_x[["xG"]],
        y = row_ys[i],
        label = row$xg_label,
        family = SDC_FONTS$body,
        size = 2.5,
        colour = SDC_ARTICLE_COLORS$ink
      )
  }

  p +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::labs(title = "Shot routines") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 8.5,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0.5,
        margin = ggplot2::margin(b = 0, t = 0)
      ),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(t = 0, r = 6, b = 2, l = 6)
    )
}

#' Completed pass arrows for attacking-zone set-piece possessions
collect_attacking_set_piece_pass_edges <- function(events_df,
                                                 team_name,
                                                 possessions_df) {
  if (nrow(possessions_df) == 0) {
    return(tibble::tibble(
      possession = integer(),
      set_piece_type = character(),
      x_from = numeric(),
      y_from = numeric(),
      x_to = numeric(),
      y_to = numeric()
    ))
  }

  purrr::map_dfr(seq_len(nrow(possessions_df)), function(i) {
    pid <- possessions_df$possession[i]
    sp_type <- possessions_df$set_piece_type[i]
    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = pid,
      team_name = team_name
    )
    network <- compute_set_piece_sequence_network(sequence, events_df)
    if (nrow(network$edges) == 0) {
      return(tibble::tibble())
    }

    network$edges %>%
      dplyr::transmute(
        possession = pid,
        set_piece_type = sp_type,
        x_from = .data$x_from,
        y_from = .data$y_from,
        x_to = .data$x_to,
        y_to = .data$y_to
      )
  })
}

#' Shot trajectories for attacking-zone set-piece possessions that ended in a shot
collect_attacking_set_piece_shot_edges <- function(events_df,
                                                 team_name,
                                                 possessions_df) {
  shot_possessions <- possessions_df %>%
    dplyr::filter(.data$ended_shot)

  if (nrow(shot_possessions) == 0) {
    return(tibble::tibble(
      possession = integer(),
      set_piece_type = character(),
      x_from = numeric(),
      y_from = numeric(),
      x_to = numeric(),
      y_to = numeric()
    ))
  }

  direction <- infer_team_attacking_high_x(events_df)

  purrr::map_dfr(seq_len(nrow(shot_possessions)), function(i) {
    pid <- shot_possessions$possession[i]
    sp_type <- shot_possessions$set_piece_type[i]
    sp_row <- events_df %>%
      dplyr::filter(.data$possession == pid) %>%
      dplyr::arrange(.data$index) %>%
      dplyr::slice(1)
    attacks_high_x <- direction %>%
      dplyr::filter(
        .data$`team.name` == !!team_name,
        .data$period == sp_row$period
      ) %>%
      dplyr::pull(.data$attacks_high_x) %>%
      dplyr::first()
    attacks_high_x <- dplyr::coalesce(attacks_high_x, TRUE)

    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = pid,
      team_name = team_name
    )
    network <- compute_set_piece_sequence_network(sequence, events_df)
    if (nrow(network$shots) == 0) {
      return(tibble::tibble())
    }

    network$shots %>%
      dplyr::mutate(
        `location.x` = .data$pitch_x,
        `location.y` = .data$pitch_y,
        norm_end = purrr::pmap(
          list(.data$`shot.end_location.x`, .data$`shot.end_location.y`),
          function(x, y) {
            if (is.na(x) || is.na(y)) {
              return(list(x = NA_real_, y = NA_real_))
            }
            normalize_opponent_half_coords(x, y, attacks_high_x)
          }
        ),
        traj_xend = purrr::map_dbl(.data$norm_end, "x"),
        traj_yend = purrr::map_dbl(.data$norm_end, "y")
      ) %>%
      dplyr::filter(!is.na(.data$traj_xend), !is.na(.data$traj_yend)) %>%
      dplyr::transmute(
        possession = pid,
        set_piece_type = sp_type,
        x_from = .data$pitch_x,
        y_from = .data$pitch_y,
        x_to = .data$traj_xend,
        y_to = .data$traj_yend
      )
  })
}

#' Binned heatmap of set-piece starts with overlaid pass networks
build_set_piece_origin_heatmap <- function(events_df,
                                           team_name,
                                           match_id,
                                           possessions_df,
                                           arrow_possessions_df = NULL,
                                           heat_color = SDC_PALETTE[["red"]],
                                           highlight_possessions = NULL,
                                           x_min = 0,
                                           x_max = 120,
                                           n_x_bins = 6L,
                                           n_y_bins = 5L) {
  shot_possessions_df <- filter_shot_set_piece_possessions(possessions_df)
  arrow_possessions_df <- arrow_possessions_df %||% shot_possessions_df

  set_pieces <- identify_team_set_piece_possessions(
    events_df,
    team_name = team_name,
    match_id = match_id
  ) %>%
    dplyr::filter(.data$possession %in% shot_possessions_df$possession)

  if (nrow(set_pieces) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  direction <- infer_team_attacking_high_x(events_df)
  routine_palette <- assign_shot_routine_palette(possessions_df)
  routine_colors <- stats::setNames(
    routine_palette$routine_color,
    as.character(routine_palette$routine_id)
  )
  pass_edges <- collect_attacking_set_piece_pass_edges(
    events_df = events_df,
    team_name = team_name,
    possessions_df = arrow_possessions_df
  )
  shot_edges <- collect_attacking_set_piece_shot_edges(
    events_df = events_df,
    team_name = team_name,
    possessions_df = arrow_possessions_df
  )

  if (nrow(pass_edges) > 0) {
    pass_edges <- pass_edges %>%
      dplyr::left_join(
        routine_palette %>% dplyr::select(.data$possession, .data$routine_id),
        by = "possession"
      )
  }
  if (nrow(shot_edges) > 0) {
    shot_edges <- shot_edges %>%
      dplyr::left_join(
        routine_palette %>% dplyr::select(.data$possession, .data$routine_id),
        by = "possession"
      )
  }

  origins <- set_pieces %>%
    dplyr::left_join(direction, by = c("team.name" = "team.name", "period" = "period")) %>%
    dplyr::mutate(attacks_high_x = dplyr::coalesce(.data$attacks_high_x, TRUE)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      norm = list(normalize_opponent_half_coords(
        .data$`location.x`,
        .data$`location.y`,
        .data$attacks_high_x
      )),
      pitch_x = .data$norm$x,
      pitch_y = .data$norm$y
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$norm) %>%
    dplyr::left_join(
      shot_possessions_df %>%
        dplyr::select(.data$possession, .data$outcome_bucket, .data$minute),
      by = "possession"
    ) %>%
    dplyr::left_join(routine_palette, by = "possession") %>%
    dplyr::mutate(
      pitch_x = pmin(pmax(.data$pitch_x, x_min), x_max),
      pitch_y = pmin(pmax(.data$pitch_y, 0), 80),
      routine_id = factor(as.character(.data$routine_id), levels = names(routine_colors))
    )

  x_breaks <- seq(x_min, x_max, length.out = n_x_bins + 1L)
  y_breaks <- seq(0, 80, length.out = n_y_bins + 1L)
  total_pieces <- nrow(origins)

  heatmap_df <- origins %>%
    dplyr::mutate(
      x_bin = as.integer(cut(
        .data$pitch_x,
        breaks = x_breaks,
        include.lowest = TRUE,
        labels = FALSE
      )),
      y_bin = as.integer(cut(
        .data$pitch_y,
        breaks = y_breaks,
        include.lowest = TRUE,
        labels = FALSE
      ))
    ) %>%
    dplyr::count(.data$x_bin, .data$y_bin, name = "zone_count") %>%
    tidyr::complete(
      x_bin = seq_len(n_x_bins),
      y_bin = seq_len(n_y_bins),
      fill = list(zone_count = 0L)
    ) %>%
    dplyr::mutate(
      share = .data$zone_count / total_pieces,
      xmin = x_breaks[.data$x_bin],
      xmax = x_breaks[.data$x_bin + 1L],
      ymin = y_breaks[.data$y_bin],
      ymax = y_breaks[.data$y_bin + 1L]
    ) %>%
    dplyr::mutate(
      xmin = pmax(.data$xmin - 0.04, x_min),
      xmax = pmin(.data$xmax + 0.04, x_max),
      ymin = pmax(.data$ymin - 0.04, 0),
      ymax = pmin(.data$ymax + 0.04, 80)
    )

  heat_colors <- palette_binned_heatmap(color = heat_color, n = 9)
  legend_max <- max(heatmap_df$share, na.rm = TRUE)
  if (!is.finite(legend_max) || legend_max <= 0) {
    legend_max <- 1
  }
  legend_limit <- min(0.5, max(0.15, ceiling(legend_max * 100 / 5) * 5 / 100))
  plot_x_max <- x_max + GOAL_NET_DEPTH_SB

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = x_min,
      xmax = x_max,
      ymin = 0,
      ymax = 80,
      fill = heat_colors[1],
      colour = NA,
      alpha = 1
    ) +
    ggplot2::geom_rect(
      data = heatmap_df,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$share
      ),
      colour = NA,
      alpha = 0.78
    ) +
    draw_pitch_markings(colour = "black", linewidth = 0.55) +
    draw_pitch_outer_border(colour = "black", linewidth = 1.0)

  for (layer in draw_pitch_goal_net_layers(colour = "black", linewidth = 0.55)) {
    p <- p + layer
  }

  if (nrow(pass_edges) > 0) {
    pass_edges <- pass_edges %>%
      dplyr::mutate(
        routine_id = factor(as.character(.data$routine_id), levels = names(routine_colors))
      )
    p <- p +
      ggplot2::geom_segment(
        data = pass_edges,
        ggplot2::aes(
          x = .data$x_from,
          y = .data$y_from,
          xend = .data$x_to,
          yend = .data$y_to,
          colour = .data$routine_id,
          group = .data$possession
        ),
        linetype = "dashed",
        alpha = 0.35,
        linewidth = 0.88,
        lineend = "round",
        arrow = grid::arrow(
          length = grid::unit(0.07, "inches"),
          type = "closed"
        )
      ) +
      ggplot2::geom_segment(
        data = pass_edges,
        ggplot2::aes(
          x = .data$x_from,
          y = .data$y_from,
          xend = .data$x_to,
          yend = .data$y_to,
          colour = .data$routine_id,
          group = .data$possession
        ),
        linetype = "dashed",
        alpha = 0.95,
        linewidth = 0.5,
        lineend = "round",
        arrow = grid::arrow(
          length = grid::unit(0.065, "inches"),
          type = "closed"
        )
      )
  }

  if (nrow(shot_edges) > 0) {
    shot_edges <- shot_edges %>%
      dplyr::mutate(
        routine_id = factor(as.character(.data$routine_id), levels = names(routine_colors))
      )
    p <- p +
      ggplot2::geom_segment(
        data = shot_edges,
        ggplot2::aes(
          x = .data$x_from,
          y = .data$y_from,
          xend = .data$x_to,
          yend = .data$y_to,
          colour = .data$routine_id,
          group = .data$possession
        ),
        linetype = "solid",
        alpha = 0.4,
        linewidth = 0.92,
        lineend = "round",
        arrow = grid::arrow(
          length = grid::unit(0.075, "inches"),
          type = "closed"
        )
      ) +
      ggplot2::geom_segment(
        data = shot_edges,
        ggplot2::aes(
          x = .data$x_from,
          y = .data$y_from,
          xend = .data$x_to,
          yend = .data$y_to,
          colour = .data$routine_id,
          group = .data$possession
        ),
        linetype = "solid",
        alpha = 0.98,
        linewidth = 0.56,
        lineend = "round",
        arrow = grid::arrow(
          length = grid::unit(0.07, "inches"),
          type = "closed"
        )
      )
  }

  legend_routine_ids <- as.character(seq_len(nrow(routine_palette)))

  p <- p +
    ggplot2::geom_point(
      data = origins,
      ggplot2::aes(x = .data$pitch_x, y = .data$pitch_y),
      fill = origins$routine_color,
      colour = "white",
      shape = 21,
      stroke = 0.45,
      size = 4.6,
      alpha = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = origins,
      ggplot2::aes(
        x = .data$pitch_x,
        y = .data$pitch_y,
        label = as.character(.data$routine_id)
      ),
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 2.35,
      colour = "white"
    ) +
    ggplot2::geom_segment(
      data = tibble::tibble(x = 22, xend = 98, y = 5, yend = 5),
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      arrow = grid::arrow(
        length = grid::unit(0.08, "inches"),
        ends = "last",
        type = "closed"
      ),
      linewidth = 0.35,
      colour = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = heat_colors,
      limits = c(0, legend_limit),
      oob = scales::squish,
      guide = "none"
    ) +
    ggplot2::scale_colour_manual(
      values = routine_colors,
      name = NULL,
      breaks = legend_routine_ids,
      guide = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(
          alpha = 1,
          linewidth = 0.9,
          linetype = "dashed",
          shape = 16,
          size = 3
        ),
        keywidth = ggplot2::unit(0.4, "cm"),
        keyheight = ggplot2::unit(0.4, "cm")
      )
    ) +
    ggplot2::scale_x_continuous(limits = c(x_min, plot_x_max), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Set-piece locations & passing",
      subtitle = paste0(
        "Markers 1–", nrow(origins), " = shot routines · dashed = passes (", nrow(pass_edges), ")",
        " · solid = shots (", nrow(shot_edges), ")"
      ),
      x = NULL,
      y = NULL
    ) +
    theme_sdc_article(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 10,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 6.5,
        colour = SDC_ARTICLE_COLORS$muted,
        hjust = 0.5,
        margin = ggplot2::margin(b = 2)
      ),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.text = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 6.5,
        colour = SDC_ARTICLE_COLORS$ink
      ),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA),
      legend.margin = ggplot2::margin(b = 1),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )

  p
}

#' Integer pie-slice percentages that sum to 100 (largest remainder)
round_pie_percents <- function(counts) {
  total <- sum(counts)
  if (total == 0L) {
    return(integer(length(counts)))
  }

  raw <- 100 * counts / total
  rounded <- floor(raw)
  remainder <- 100L - sum(rounded)
  if (remainder > 0L) {
    bump <- base::order(raw - rounded, decreasing = TRUE)
    rounded[bump[seq_len(remainder)]] <- rounded[bump[seq_len(remainder)]] + 1L
  }

  as.integer(rounded)
}

#' Pie chart of set-piece outcomes — percentages on slices, legend above
build_set_piece_outcome_pie_plot <- function(outcome_counts,
                                            pie_colors,
                                            total_xg = NULL) {
  pie_df <- outcome_counts %>%
    dplyr::arrange(dplyr::desc(.data$outcome_bucket)) %>%
    dplyr::mutate(frac = .data$n / sum(.data$n))

  pie_df$pct_display <- round_pie_percents(pie_df$n)

  pie_df <- pie_df %>%
    dplyr::mutate(
      ymax = cumsum(.data$frac),
      ymin = dplyr::lag(.data$ymax, default = 0),
      label_y = (.data$ymax + .data$ymin) / 2,
      slice_label = paste0(.data$pct_display, "%"),
      label_size = dplyr::if_else(.data$frac < 0.2, 2.4, 2.8)
    )

  ggplot2::ggplot(pie_df, ggplot2::aes(x = 1, y = .data$frac, fill = .data$outcome_bucket)) +
    ggplot2::geom_col(width = 0.48, colour = "white", linewidth = 0.55) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data$label_y,
        label = .data$slice_label,
        size = .data$label_size
      ),
      x = 1,
      family = SDC_FONTS$body,
      fontface = "bold",
      colour = SDC_ARTICLE_COLORS$ink,
      lineheight = 0.85,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_size_identity(guide = "none") +
    ggplot2::scale_fill_manual(
      values = pie_colors,
      name = NULL,
      breaks = names(pie_colors),
      guide = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 0),
        keywidth = ggplot2::unit(0.45, "cm"),
        keyheight = ggplot2::unit(0.45, "cm")
      )
    ) +
    ggplot2::labs(
      title = "Set-piece outcomes",
      subtitle = if (!is.null(total_xg)) {
        paste0(format(round(total_xg, 2), nsmall = 2), " total xG")
      } else {
        NULL
      },
      x = NULL,
      y = NULL
    ) +
    theme_sdc_article(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 10,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 8.5,
        colour = SDC_PALETTE[["orange"]],
        hjust = 0.5,
        margin = ggplot2::margin(b = 2)
      ),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box.just = "center",
      legend.text = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 7,
        colour = SDC_ARTICLE_COLORS$ink
      ),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA),
      legend.margin = ggplot2::margin(b = 2),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(t = 0, r = 8, b = -10, l = 8)
    )
}

#' Compact bar chart for categorical set-piece counts
build_set_piece_count_bar <- function(data,
                                      title,
                                      subtitle = NULL,
                                      fill_colors = NULL,
                                      bar_width = 0.55) {
  if (nrow(data) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$category, y = .data$n, fill = .data$category)
  ) +
    ggplot2::geom_col(width = bar_width, colour = NA) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$n),
      vjust = -0.25,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 3,
      colour = SDC_ARTICLE_COLORS$ink
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL)

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = fill_colors, guide = "none")
  } else {
    p <- p + ggplot2::scale_fill_manual(values = SDC_PALETTE_VEC, guide = "none")
  }

  p +
    theme_sdc_article(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 8.5,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 6.5,
        colour = SDC_ARTICLE_COLORS$muted,
        hjust = 0
      ),
      axis.text.x = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 6.5,
        colour = SDC_ARTICLE_COLORS$ink
      ),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(2, 4, 2, 4)
    )
}

#' Set-piece type split (corners vs free kicks)
build_set_piece_type_distribution_plot <- function(performance) {
  data <- tibble::tibble(
    category = c("Corners", "Free kicks"),
    n = c(performance$n_corners, performance$n_free_kicks)
  )
  build_set_piece_count_bar(
    data,
    title = "Set-piece types",
    subtitle = paste0(performance$total_set_pieces, " attacking-zone routines"),
    fill_colors = c(
      "Corners" = SDC_PALETTE[["purple"]],
      "Free kicks" = SDC_PALETTE[["blue"]]
    )
  )
}

#' Corner-kick side split (left vs right)
build_corner_side_distribution_plot <- function(performance) {
  data <- tibble::tibble(
    category = c("Left", "Right"),
    n = c(performance$corners_left, performance$corners_right)
  )
  build_set_piece_count_bar(
    data,
    title = "Corner sides",
    subtitle = paste0(performance$n_corners, " corners taken"),
    fill_colors = c(
      "Left" = SDC_PALETTE[["cyan"]],
      "Right" = SDC_PALETTE[["orange"]]
    )
  )
}

#' Passing metrics for set-piece sequences
build_set_piece_pass_metrics_plot <- function(performance) {
  fmt_avg <- function(x) {
    if (is.na(x)) {
      return("—")
    }
    format(round(x, 1), nsmall = 1)
  }

  metrics <- tibble::tibble(
    category = c(
      "Total xG",
      "Avg passes / sequence",
      "Avg passes before shot",
      "Avg passes before loss",
      "Direct shots (0 passes)"
    ),
    value = c(
      performance$total_xg,
      performance$avg_passes,
      performance$avg_passes_to_shot,
      performance$avg_passes_before_loss,
      as.numeric(performance$direct_shots)
    ),
    display = c(
      format(round(performance$total_xg, 2), nsmall = 2),
      fmt_avg(performance$avg_passes),
      fmt_avg(performance$avg_passes_to_shot),
      fmt_avg(performance$avg_passes_before_loss),
      as.character(as.integer(performance$direct_shots))
    ),
    highlight = c(
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      TRUE
    )
  )

  ggplot2::ggplot(
    metrics,
    ggplot2::aes(
      x = reorder(.data$category, .data$value),
      y = .data$value,
      fill = .data$highlight
    )
  ) +
    ggplot2::geom_col(width = 0.58, colour = NA) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$display),
      hjust = -0.1,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 2.8,
      colour = SDC_ARTICLE_COLORS$ink
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = SDC_PALETTE[["orange"]], "FALSE" = SDC_PALETTE[["blue"]]),
      guide = "none"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = "Passing profile",
      subtitle = paste0(
        performance$completed_passes, " completed passes · ",
        format(round(performance$total_xg, 2), nsmall = 2), " total xG · max ",
        performance$max_passes, " in one sequence"
      ),
      x = NULL,
      y = NULL
    ) +
    theme_sdc_article(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 8.5,
        colour = SDC_ARTICLE_COLORS$ink,
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 6.5,
        colour = SDC_ARTICLE_COLORS$muted,
        hjust = 0
      ),
      axis.text.y = ggplot2::element_text(
        family = SDC_FONTS$body,
        size = 6.2,
        colour = SDC_ARTICLE_COLORS$ink
      ),
      axis.text.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(2, 22, 2, 4)
    )
}

#' Bar + pie summary of team set-piece performance
build_set_piece_performance_summary <- function(performance,
                                                team_label = "Portugal",
                                                events_df = NULL,
                                                team_name = NULL,
                                                match_id = NULL,
                                                team_color = SDC_PALETTE[["red"]],
                                                highlight_possessions = NULL,
                                                bar_color = SDC_PALETTE[["blue"]],
                                                accent_color = SDC_PALETTE[["orange"]]) {
  type_plot <- build_set_piece_type_distribution_plot(performance)
  corner_plot <- build_corner_side_distribution_plot(performance)
  pass_plot <- build_set_piece_pass_metrics_plot(performance)

  detail_column <- patchwork::wrap_plots(
    type_plot,
    corner_plot,
    pass_plot,
    ncol = 1,
    heights = c(0.28, 0.28, 0.44)
  )

  outcome_counts <- performance$possessions %>%
    dplyr::count(.data$outcome_bucket, name = "n") %>%
    dplyr::mutate(
      pct = round(100 * .data$n / sum(.data$n), 0),
      legend_label = paste0(.data$outcome_bucket, " · ", .data$n, " (", .data$pct, "%)")
    )

  pie_colors <- c(
    "Goal" = SDC_PALETTE[["orange"]],
    "Shot (no goal)" = SDC_PALETTE[["purple"]],
    "No shot" = SDC_ARTICLE_COLORS$grid
  )

  pie_plot <- build_set_piece_outcome_pie_plot(
    outcome_counts,
    pie_colors,
    total_xg = performance$total_xg
  )
  shot_table <- build_set_piece_shot_reference_table(performance$possessions)
  pie_column <- patchwork::wrap_plots(
    pie_plot,
    shot_table,
    ncol = 1,
    heights = c(0.46, 0.54)
  )

  arrow_possessions <- filter_shot_set_piece_possessions(performance$possessions)

  heatmap_plot <- if (!is.null(events_df) && !is.null(team_name)) {
    build_set_piece_origin_heatmap(
      events_df = events_df,
      team_name = team_name,
      match_id = match_id,
      possessions_df = performance$possessions,
      arrow_possessions_df = arrow_possessions,
      heat_color = team_color,
      highlight_possessions = highlight_possessions
    )
  } else {
    NULL
  }

  if (!is.null(heatmap_plot)) {
    patchwork::wrap_plots(
      detail_column,
      pie_column,
      heatmap_plot,
      ncol = 3,
      widths = c(1.05, 0.82, 1.05)
    ) +
      patchwork::plot_annotation(
        title = paste0(team_label, " dead-ball breakdown"),
        subtitle = paste0(
          performance$with_shot, " of ", performance$total_set_pieces,
          " sequences reached a shot (",
          format(round(100 * performance$shot_rate, 0), nsmall = 0),
          "%) · ",
          performance$with_goal, " goals · ",
          format(round(performance$total_xg, 2), nsmall = 2),
          " total xG"
        ),
        theme = theme_sdc_article(base_size = 9) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              family = SDC_FONTS$title,
              face = "bold",
              size = 10,
              colour = SDC_ARTICLE_COLORS$ink,
              hjust = 0.5
            ),
            plot.subtitle = ggplot2::element_text(
              family = SDC_FONTS$body,
              size = 7,
              colour = SDC_ARTICLE_COLORS$muted,
              hjust = 0.5,
              margin = ggplot2::margin(b = 4)
            ),
            plot.background = ggplot2::element_rect(fill = NA, colour = NA)
          )
      )
  } else {
    patchwork::wrap_plots(detail_column, pie_column, ncol = 2, widths = c(1.15, 0.85))
  }
}

#' UC13: Passing networks for highlighted set pieces + match performance summary
viz_portugal_set_piece_networks <- function(events_df,
                                            team_name,
                                            match_id = NULL,
                                            meta = NULL,
                                            lineups = NULL,
                                            opponent_name = NULL,
                                            frames_360 = NULL,
                                            panels = NULL,
                                            team_color = SDC_PALETTE[["red"]],
                                            opponent_color = SDC_PALETTE[["cyan"]],
                                            patterns = c("From Corner", "From Free Kick"),
                                            attacking_zone_only = TRUE,
                                            max_goal_distance_m = 35,
                                            eyebrow = "Set piece analysis",
                                            title = NULL,
                                            subtitle = NULL,
                                            caption = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  if (is.null(opponent_name) && !is.null(meta)) {
    opponent_name <- if (identical(team_name, meta$home_team)) {
      meta$away_team
    } else {
      meta$home_team
    }
  }
  if (is.null(frames_360) && !is.null(match_id)) {
    frames_360 <- load_statsbomb_360_frames(match_id)
  }
  frames_360 <- frames_360 %||% list(by_event_id = list())

  if (is.null(panels)) {
    panels <- list(
      list(
        possession_id = 29L,
        panel_title = "Nuno Mendes · FK · 16' · Goal"
      ),
      list(
        possession_id = 136L,
        panel_title = "Nematov own goal · Corner · 59'"
      ),
      list(
        possession_id = 135L,
        panel_title = "Ronaldo · FK · 57' · Saved · 0.36 xG"
      )
    )
  }

  network_panels <- purrr::map(panels, function(panel) {
    row <- resolve_set_piece_row(
      events_df,
      team_name = team_name,
      possession_id = panel$possession_id,
      attacking_zone_only = attacking_zone_only,
      max_goal_distance_m = max_goal_distance_m
    )
    if (!is.null(panel$panel_title)) {
      row$panel_headline <- panel$panel_title
    }
    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = row$possession,
      team_name = team_name
    )
    network <- compute_set_piece_sequence_network(sequence, events_df)
    if (!is.na(row$shot_outcome) && identical(row$shot_outcome, "Own Goal") &&
        nrow(network$shots) > 0) {
      network$shots <- network$shots %>%
        dplyr::mutate(`shot.outcome.name` = "Own Goal")
    }
    frame_entry <- frames_360$by_event_id[[row$key_event_id]]
    freeze_positions <- parse_freeze_frame_positions(
      frame_entry,
      events_df = events_df,
      attacking_team_name = team_name,
      period = row$period
    )
    poss_events <- events_df %>%
      dplyr::filter(.data$possession == row$possession)
    attacking_lookup <- if (!is.null(lineups)) {
      build_team_jersey_lookup(
        poss_events,
        lineups,
        team_name,
        events_df,
        row$period,
        normalize_team_name = team_name
      )
    } else {
      tibble::tibble()
    }
    actor_pins <- build_sequence_actor_pins(network, attacking_lookup)
    build_set_piece_network_panel(
      sequence_df = sequence,
      network = network,
      freeze_positions = freeze_positions,
      actor_pins = actor_pins,
      panel_title = dangerous_set_piece_panel_title(row),
      team_color = team_color,
      opponent_color = opponent_color
    )
  })

  performance <- summarize_team_set_piece_performance(
    events_df,
    team_name = team_name,
    match_id = match_id,
    patterns = patterns,
    attacking_zone_only = attacking_zone_only,
    max_goal_distance_m = max_goal_distance_m
  )

  team_label <- if (!is.null(meta)) {
    if (identical(team_name, meta$home_team)) meta$display_home else meta$display_away
  } else {
    team_name
  }

  summary_plot <- build_set_piece_performance_summary(
    performance,
    team_label = team_label,
    events_df = events_df,
    team_name = team_name,
    match_id = match_id,
    team_color = team_color,
    highlight_possessions = vapply(panels, function(p) p$possession_id, integer(1))
  )

  if (is.null(title)) {
    title <- paste0(toupper(team_label), "'S ATTACKING SET-PIECE PROFILE")
  }
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Key routines, locations, and outcomes from ",
      performance$total_set_pieces, " attacking-zone dead balls · ",
      performance$with_goal, " goals · ",
      format(round(performance$total_xg, 2), nsmall = 2), " total xG"
    )
    if (!is.null(meta)) {
      subtitle <- paste0(subtitle, " · ", match_score_line(meta))
    }
  }
  if (is.null(caption)) {
    caption <- paste0(
      "Passing networks at the shot or final pass freeze frame. ",
      team_label, " players in red, ",
      opponent_name %||% "opponent", " in cyan. ",
      "Numbers mark completed passes in sequence order."
    )
  }

  patchwork::wrap_plots(
    build_decisive_sequence_header(
      eyebrow = eyebrow,
      headline = title,
      subtitle = subtitle %||% ""
    ),
    patchwork::wrap_plots(network_panels, ncol = length(network_panels)),
    summary_plot,
    ncol = 1,
    heights = c(0.09, 0.42, 0.44)
  ) +
    patchwork::plot_annotation(
      caption = caption,
      theme = theme_sdc_article(base_size = 10) +
        ggplot2::theme(
          plot.caption = ggplot2::element_text(
            family = SDC_FONTS$body,
            size = 8,
            colour = SDC_ARTICLE_COLORS$muted,
            hjust = 0,
            margin = ggplot2::margin(t = 6)
          ),
          plot.background = ggplot2::element_rect(
            fill = SDC_ARTICLE_COLORS$offwhite,
            colour = NA
          ),
          plot.margin = ggplot2::margin(12, 14, 8, 14)
        )
    )
}
