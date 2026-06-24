#' Identify possession ids that begin with a team set piece
identify_team_set_piece_possessions <- function(events_df,
                                                team_name,
                                                match_id = NULL,
                                                patterns = c("From Corner", "From Free Kick")) {
  data <- events_df
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }

  data %>%
    dplyr::arrange(.data$index) %>%
    dplyr::group_by(.data$possession) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$possession_team_name == !!team_name,
      .data$play_pattern_name %in% patterns
    ) %>%
    dplyr::mutate(
      set_piece_type = dplyr::case_when(
        .data$play_pattern_name == "From Corner" ~ "Corner",
        .data$play_pattern_name == "From Free Kick" ~ "Free kick",
        TRUE ~ .data$play_pattern_name
      )
    ) %>%
    dplyr::arrange(.data$minute, .data$second, .data$index)
}

#' Keep corners and free kicks taken in the opponent's half near goal
#'
#' Free kicks must start in the opponent's half and within \code{max_goal_distance_m}
#' of the goal line (default 35 m). Corners are always retained.
filter_attacking_zone_set_pieces <- function(set_pieces,
                                             events_df,
                                             max_goal_distance_m = 35,
                                             opponent_half_x = 60) {
  if (nrow(set_pieces) == 0) {
    return(set_pieces)
  }

  direction <- infer_team_attacking_high_x(events_df)
  set_pieces %>%
    dplyr::left_join(direction, by = c("team.name" = "team.name", "period" = "period")) %>%
    dplyr::mutate(
      attacks_high_x = dplyr::coalesce(.data$attacks_high_x, TRUE),
      dist_to_goal_m = dplyr::if_else(
        .data$attacks_high_x,
        120 - .data$`location.x`,
        .data$`location.x`
      ),
      on_opponent_half = dplyr::if_else(
        .data$attacks_high_x,
        .data$`location.x` >= .env$opponent_half_x,
        .data$`location.x` <= .env$opponent_half_x
      ),
      keep_piece = .data$set_piece_type == "Corner" | (
        .data$set_piece_type == "Free kick" &
          .data$on_opponent_half &
          !is.na(.data$dist_to_goal_m) &
          .data$dist_to_goal_m <= .env$max_goal_distance_m
      )
    ) %>%
    dplyr::filter(.data$keep_piece) %>%
    dplyr::select(
      -dplyr::any_of(c(
        "attacks_high_x",
        "dist_to_goal_m",
        "on_opponent_half",
        "keep_piece"
      ))
    )
}

#' Extract team events from a set-piece possession until a shot or ball loss
extract_set_piece_sequence <- function(events_df, possession_id, team_name) {
  poss_events <- events_df %>%
    dplyr::filter(.data$possession == !!possession_id) %>%
    dplyr::arrange(.data$index)

  team_events <- poss_events %>%
    dplyr::filter(.data$`team.name` == !!team_name)

  if (nrow(team_events) == 0) {
    return(team_events)
  }

  lost_mask <- team_events$type_name == "Miscontrol" |
    team_events$type_name %in% c("Dispossessed", "Error") |
    (team_events$type_name == "Pass" & !is.na(team_events$`pass.outcome.name`))

  shot_idx <- which(team_events$type_name == "Shot")[1]
  lost_idx <- which(lost_mask)[1]
  end_idx <- nrow(team_events)
  if (!is.na(shot_idx)) {
    end_idx <- min(end_idx, shot_idx)
  }
  if (!is.na(lost_idx)) {
    end_idx <- min(end_idx, lost_idx)
  }

  team_events %>% dplyr::slice(1:end_idx)
}

SET_PIECE_OFF_TARGET_SHOT_OUTCOMES <- c("Off T", "Wayward", "Post")

#' Count completed passes in a set-piece sequence
count_set_piece_completed_passes <- function(sequence_df) {
  sum(
    sequence_df$type_name == "Pass" &
      is.na(sequence_df$`pass.outcome.name`) &
      !is.na(sequence_df$`pass.recipient.id`),
    na.rm = TRUE
  )
}

#' Whether the first shot in a sequence was taken without a completed pass
is_direct_off_target_set_piece_shot <- function(sequence_df) {
  shot_idx <- which(sequence_df$type_name == "Shot")[1]
  if (is.na(shot_idx)) {
    return(FALSE)
  }

  outcome <- sequence_df$`shot.outcome.name`[shot_idx]
  if (is.na(outcome) || !outcome %in% SET_PIECE_OFF_TARGET_SHOT_OUTCOMES) {
    return(FALSE)
  }

  before_shot <- sequence_df[seq_len(shot_idx), , drop = FALSE]
  count_set_piece_completed_passes(before_shot) == 0L
}

#' Keep sequences with at least one completed pass and no direct off-target shot
keep_set_piece_sequence_for_network <- function(sequence_df) {
  if (count_set_piece_completed_passes(sequence_df) == 0L) {
    return(FALSE)
  }
  !is_direct_off_target_set_piece_shot(sequence_df)
}

#' Normalize sequence coordinates to opponent half (goal line at x = 120)
normalize_sequence_coords <- function(sequence_df, events_df) {
  if (nrow(sequence_df) == 0) {
    return(sequence_df)
  }

  direction <- infer_team_attacking_high_x(events_df)
  sequence_df %>%
    dplyr::left_join(direction, by = c("team.name" = "team.name", "period" = "period")) %>%
    dplyr::mutate(
      attacks_high_x = dplyr::coalesce(.data$attacks_high_x, TRUE),
      norm_start = purrr::pmap(
        list(.data$`location.x`, .data$`location.y`, .data$attacks_high_x),
        normalize_opponent_half_coords
      ),
      pitch_x = purrr::map_dbl(.data$norm_start, "x"),
      pitch_y = purrr::map_dbl(.data$norm_start, "y"),
      norm_end = purrr::pmap(
        list(
          .data$`pass.end_location.x`,
          .data$`pass.end_location.y`,
          .data$attacks_high_x
        ),
        function(x, y, high) {
          if (is.na(x) || is.na(y)) {
            return(list(x = NA_real_, y = NA_real_))
          }
          normalize_opponent_half_coords(x, y, high)
        }
      ),
      pass_end_x = purrr::map_dbl(.data$norm_end, "x"),
      pass_end_y = purrr::map_dbl(.data$norm_end, "y")
    ) %>%
    dplyr::select(-dplyr::starts_with("norm_"), -"attacks_high_x")
}

#' Passing edges and node positions for one set-piece sequence
compute_set_piece_sequence_network <- function(sequence_df, events_df) {
  sequence_df <- normalize_sequence_coords(sequence_df, events_df)

  passes <- sequence_df %>%
    dplyr::filter(
      .data$type_name == "Pass",
      is.na(.data$`pass.outcome.name`),
      !is.na(.data$`pass.recipient.id`)
    )

  edges <- if (nrow(passes) > 0) {
    passes %>%
      dplyr::transmute(
        passer_id = .data$`player.id`,
        recipient_id = .data$`pass.recipient.id`,
        x_from = .data$pitch_x,
        y_from = .data$pitch_y,
        x_to = .data$pass_end_x,
        y_to = .data$pass_end_y,
        pass_count = 1L
      )
  } else {
    tibble::tibble(
      passer_id = numeric(),
      recipient_id = numeric(),
      x_from = numeric(),
      y_from = numeric(),
      x_to = numeric(),
      y_to = numeric(),
      pass_count = integer()
    )
  }

  touches <- sequence_df %>%
    dplyr::filter(!is.na(.data$`player.id`), !is.na(.data$pitch_x))

  nodes <- if (nrow(touches) > 0) {
    touches %>%
      dplyr::group_by(.data$`player.id`) %>%
      dplyr::summarise(
        player_label = passing_network_label(
          .data$`player.name`[1],
          .data$player_display_name[1]
        ),
        x = mean(.data$pitch_x, na.rm = TRUE),
        y = mean(.data$pitch_y, na.rm = TRUE),
        touches = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::rename(player_id = `player.id`)
  } else {
    tibble::tibble(
      player_id = numeric(),
      player_label = character(),
      x = numeric(),
      y = numeric(),
      touches = integer()
    )
  }

  shots <- sequence_df %>%
    dplyr::filter(.data$type_name == "Shot")

  list(nodes = nodes, edges = edges, shots = shots)
}

#' Label one set-piece panel (type, minute, outcome)
set_piece_panel_label <- function(set_piece_row, sequence_df) {
  outcome <- sequence_df %>%
    dplyr::filter(.data$type_name == "Shot") %>%
    dplyr::slice(1)

  suffix <- if (nrow(outcome) > 0) {
    paste0(" · ", outcome$`shot.outcome.name`)
  } else if (any(sequence_df$type_name == "Miscontrol", na.rm = TRUE)) {
    " · Lost"
  } else if (any(
    sequence_df$type_name == "Pass" & !is.na(sequence_df$`pass.outcome.name`),
    na.rm = TRUE
  )) {
    " · Lost"
  } else {
    ""
  }

  paste0(set_piece_row$set_piece_type, " · ", set_piece_row$minute, "'", suffix)
}

#' Build one set-piece sequence plot on a full pitch
build_set_piece_sequence_plot <- function(network,
                                          panel_title,
                                          team_color = SDC_PALETTE[["red"]],
                                          edge_alpha = 0.75) {
  nodes <- network$nodes
  edges <- network$edges
  shots <- network$shots

  p <- ggplot2::ggplot() +
    draw_pitch_markings(colour = "black", linewidth = 0.45) +
    draw_pitch_outer_border(colour = "black", linewidth = 0.85)

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
        alpha = edge_alpha,
        linewidth = 0.65,
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed")
      )
  }

  if (nrow(nodes) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = nodes,
        ggplot2::aes(x = .data$x, y = .data$y, size = .data$touches),
        fill = "white",
        colour = team_color,
        shape = 21,
        stroke = 0.75
      ) +
      ggplot2::scale_size_continuous(range = c(2.2, 4.5), guide = "none") +
      ggplot2::geom_text(
        data = nodes,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$player_label),
        family = SDC_FONTS$body,
        size = 2.3,
        colour = "#222222",
        fontface = "bold"
      )
  }

  if (nrow(shots) > 0) {
    shot_traj <- add_shot_trajectory_endpoints(shots)
    traj_cols <- shot_trajectory_outcome_colors()
    shot_traj <- shot_traj %>%
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
    ggplot2::scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / 120) +
    ggplot2::labs(title = panel_title, x = NULL, y = NULL) +
    theme_sdc(base_size = 8) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 8.5,
        colour = "#111111",
        hjust = 0.5,
        margin = ggplot2::margin(b = 2)
      ),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(2, 3, 2, 3)
    )
}

#' UC11: Per set-piece passing networks until shot or ball lost
viz_team_set_piece_passing_networks <- function(events_df,
                                                team_name,
                                                match_id = NULL,
                                                meta = NULL,
                                                team_color = SDC_PALETTE[["red"]],
                                                patterns = c("From Corner", "From Free Kick"),
                                                attacking_zone_only = FALSE,
                                                max_goal_distance_m = 35,
                                                ncol = 5,
                                                title = NULL,
                                                subtitle = NULL,
                                                caption = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

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

  set_piece_sequences <- purrr::map(seq_len(nrow(set_pieces)), function(i) {
    sp <- set_pieces[i, , drop = FALSE]
    sequence <- extract_set_piece_sequence(
      events_df,
      possession_id = sp$possession,
      team_name = team_name
    )
    list(set_piece = sp, sequence = sequence)
  })
  set_piece_sequences <- purrr::keep(
    set_piece_sequences,
  ~ keep_set_piece_sequence_for_network(.x$sequence)
  )

  if (length(set_piece_sequences) == 0) {
    stop(
      "No set-piece possessions with completed passes found for ",
      team_name,
      call. = FALSE
    )
  }

  plots <- purrr::map(set_piece_sequences, function(item) {
    network <- compute_set_piece_sequence_network(item$sequence, events_df)
    panel_title <- set_piece_panel_label(item$set_piece, item$sequence)
    build_set_piece_sequence_plot(
      network,
      panel_title = panel_title,
      team_color = team_color
    )
  })

  n_panels <- length(plots)
  ncol_eff <- min(ncol, n_panels)
  nrow_eff <- ceiling(n_panels / ncol_eff)

  grid <- patchwork::wrap_plots(plots, ncol = ncol_eff)

  team_label <- if (!is.null(meta)) {
    meta$display_home[meta$home_team == team_name][1] %||%
      meta$display_away[meta$away_team == team_name][1] %||%
      team_name
  } else {
    team_name
  }

  if (is.null(title)) {
    title <- paste0(team_label, " set-piece passing networks")
  }
  if (is.null(subtitle) && !is.null(meta)) {
    subtitle <- match_score_line(meta)
  }
  if (is.null(caption)) {
    zone_note <- if (isTRUE(attacking_zone_only)) {
      paste0(
        "Corners plus free kicks within ", max_goal_distance_m,
        " m of goal in the opponent's half. "
      )
    } else {
      ""
    }
    caption <- paste0(
      zone_note,
      "Set pieces with no completed pass or a direct off-target shot are omitted. ",
      "Each panel is one set-piece possession until a shot or ball loss. ",
      "Arrows are completed passes; dashed lines are shots."
    )
  }

  grid +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption,
      theme = theme_sdc_article()
    ) +
    patchwork::plot_layout(guides = "keep")
}

#' Save a multi-panel set-piece figure with sensible dimensions
save_set_piece_figure <- function(plot,
                                  path,
                                  ncol = 5,
                                  n_panels = NULL,
                                  panel_width = 2.35,
                                  panel_height = 1.65,
                                  dpi = 96) {
  if (is.null(n_panels)) {
    n_panels <- length(plot)
  }
  nrow_eff <- ceiling(n_panels / ncol)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = ncol * panel_width,
    height = nrow_eff * panel_height + 1.1,
    units = "in",
    dpi = dpi,
    bg = "white"
  )
  invisible(path)
}
