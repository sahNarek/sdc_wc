#' Article layout colours (Haiti–Scotland template / SDC style guide)
SDC_ARTICLE_COLORS <- list(
  ink = "#14213D",
  muted = "#5B6577",
  grid = "#D9E1E8",
  pitch = "#F1F4ED",
  offwhite = "#F7F8F5",
  narrative = "#FFF3E9"
)

#' Seconds from match clock columns
event_timestamp_sec <- function(minute, second) {
  minute * 60L + second
}

#' Locate one goal shot row for a team
find_goal_shot_event <- function(events_df,
                                 team_name,
                                 goal_minute = NULL,
                                 goal_second = NULL,
                                 scorer_name = NULL,
                                 match_id = NULL) {
  data <- ensure_viz_aliases(events_df)
  if (!is.null(match_id)) {
    data <- data %>% dplyr::filter(.data$match_id == !!match_id)
  }

  goals <- data %>%
    dplyr::filter(
      .data$`type.name` == "Shot",
      .data$`shot.outcome.name` == "Goal",
      .data$`team.name` == !!team_name
    )

  if (!is.null(goal_minute)) {
    goals <- goals %>% dplyr::filter(.data$minute == !!goal_minute)
    if (!is.null(goal_second)) {
      goals <- goals %>% dplyr::filter(.data$second == !!goal_second)
    }
  }
  if (!is.null(scorer_name)) {
    goals <- goals %>%
      dplyr::filter(
        .data$`player.name` == !!scorer_name |
          .data$player_display_name == !!scorer_name
      )
  }

  if (nrow(goals) == 0) {
    stop("No matching goal found for ", team_name, call. = FALSE)
  }

  goals %>% dplyr::slice(1)
}

#' Team events from possession start through the goal shot
extract_goal_buildup_sequence <- function(events_df,
                                          goal_event,
                                          team_name,
                                          trim_final_third = TRUE,
                                          final_third_x = 80) {
  data <- ensure_viz_aliases(events_df)
  poss_events <- data %>%
    dplyr::filter(.data$possession == goal_event$possession) %>%
    dplyr::arrange(.data$index)

  team_events <- poss_events %>%
    dplyr::filter(.data$`team.name` == !!team_name)

  shot_idx <- which(
    team_events$`type.name` == "Shot" &
      team_events$index == goal_event$index
  )[1]
  if (is.na(shot_idx)) {
    stop("Goal shot not found in team possession sequence.", call. = FALSE)
  }

  sequence <- team_events %>% dplyr::slice(seq_len(shot_idx))
  sequence <- normalize_goal_sequence_coords(sequence, events_df)

  if (isTRUE(trim_final_third)) {
    ft_idx <- which(sequence$pitch_x >= final_third_x)[1]
    if (!is.na(ft_idx) && ft_idx > 1L) {
      sequence <- sequence %>% dplyr::slice(ft_idx:dplyr::n())
    }
  }

  sequence
}

#' Normalize sequence coordinates to opponent half (goal line at x = 120)
normalize_goal_sequence_coords <- function(sequence_df, events_df) {
  if (nrow(sequence_df) == 0) {
    return(sequence_df)
  }

  direction <- infer_team_attacking_high_x(events_df)
  sequence_df %>%
    dplyr::left_join(direction, by = c("team.name", "period")) %>%
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

#' Tabular sequence for decisive-goal visualisation
prepare_goal_sequence_table <- function(sequence_df) {
  if (nrow(sequence_df) == 0) {
    return(tibble::tibble())
  }

  rows <- purrr::map(seq_len(nrow(sequence_df)), function(i) {
    row <- sequence_df[i, , drop = FALSE]
    next_row <- if (i < nrow(sequence_df)) sequence_df[i + 1, , drop = FALSE] else NULL
    event_type <- row$`type.name`

  if (event_type == "Pass" && is.na(row$`pass.outcome.name`)) {
      tibble::tibble(
        sequence_order = i,
        event_type = "Pass",
        player = row$`player.name`,
        player_label = passing_network_label(
          row$`player.name`,
          row$player_display_name
        ),
        recipient = row$`pass.recipient.name`,
        recipient_label = passing_network_label(row$`pass.recipient.name`),
        start_x = row$pitch_x,
        start_y = row$pitch_y,
        end_x = row$pass_end_x,
        end_y = row$pass_end_y,
        xg = NA_real_,
        outcome = NA_character_
      )
    } else if (event_type == "Carry") {
      end_x <- if (!is.null(next_row)) next_row$pitch_x else row$pitch_x
      end_y <- if (!is.null(next_row)) next_row$pitch_y else row$pitch_y
      tibble::tibble(
        sequence_order = i,
        event_type = "Carry",
        player = row$`player.name`,
        player_label = passing_network_label(
          row$`player.name`,
          row$player_display_name
        ),
        recipient = NA_character_,
        recipient_label = NA_character_,
        start_x = row$pitch_x,
        start_y = row$pitch_y,
        end_x = end_x,
        end_y = end_y,
        xg = NA_real_,
        outcome = NA_character_
      )
    } else if (event_type == "Shot") {
      traj <- add_shot_trajectory_endpoints(
        row %>%
          dplyr::mutate(
            `location.x` = .data$pitch_x,
            `location.y` = .data$pitch_y
          )
      )
      tibble::tibble(
        sequence_order = i,
        event_type = "Shot",
        player = row$`player.name`,
        player_label = passing_network_label(
          row$`player.name`,
          row$player_display_name
        ),
        recipient = NA_character_,
        recipient_label = NA_character_,
        start_x = row$pitch_x,
        start_y = row$pitch_y,
        end_x = traj$traj_xend[1],
        end_y = traj$traj_yend[1],
        xg = row$`shot.statsbomb_xg`,
        outcome = row$`shot.outcome.name`
      )
    } else {
      NULL
    }
  })

  dplyr::bind_rows(rows)
}

#' Summary metrics for headline cards
summarize_goal_sequence <- function(sequence_df, sequence_table) {
  passes <- sequence_table %>% dplyr::filter(.data$event_type == "Pass")
  shots <- sequence_table %>% dplyr::filter(.data$event_type == "Shot")
  start_sec <- event_timestamp_sec(sequence_df$minute[1], sequence_df$second[1])
  end_sec <- event_timestamp_sec(
    sequence_df$minute[nrow(sequence_df)],
    sequence_df$second[nrow(sequence_df)]
  )

  list(
    seconds = end_sec - start_sec,
    completed_passes = nrow(passes),
    combined_xg = sum(shots$xg, na.rm = TRUE),
    longest_pass_idx = if (nrow(passes) > 0) {
      which.max(sqrt((passes$end_x - passes$start_x)^2 + (passes$end_y - passes$start_y)^2))
    } else {
      NA_integer_
    }
  )
}

#' Headline in template style: "26 SECONDS. SIX PASSES. ONE DECISIVE GOAL."
decisive_sequence_headline <- function(seconds, completed_passes) {
  pass_word <- if (completed_passes == 1L) "PASS" else "PASSES"
  paste0(
    seconds,
    " SECONDS. ",
    completed_passes,
    " ",
    pass_word,
    ".\nONE DECISIVE GOAL."
  )
}

#' Auto-generated pass chain for the narrative footer
build_goal_sequence_chain_text <- function(sequence_table) {
  passes <- sequence_table %>% dplyr::filter(.data$event_type == "Pass")
  shots <- sequence_table %>% dplyr::filter(.data$event_type == "Shot")

  if (nrow(passes) == 0 && nrow(shots) == 0) {
    return("")
  }

  parts <- character()
  if (nrow(passes) > 0) {
    parts <- c(parts, passes$player_label[1])
    parts <- c(parts, passes$recipient_label)
  }
  if (nrow(shots) > 0) {
    shot_labels <- ifelse(
      shots$outcome == "Goal",
      paste0(shots$player_label, " goal"),
      paste0(shots$player_label, " ", tolower(shots$outcome))
    )
    parts <- c(parts, shot_labels)
  }

  paste(parts, collapse = " -> ")
}

#' Pitch background and markings for decisive-sequence maps
draw_decisive_sequence_pitch_layers <- function(line_colour = "#AAB7AA",
                                                pitch_fill = SDC_ARTICLE_COLORS$pitch) {
  list(
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = 120,
      ymin = 0,
      ymax = 80,
      fill = pitch_fill,
      colour = NA
    ),
    draw_pitch_markings(colour = line_colour, linewidth = 0.55),
    draw_pitch_outer_border(colour = line_colour, linewidth = 0.85)
  )
}

#' Offset player labels away from pitch action
auto_sequence_label_offset <- function(x, y) {
  dx <- dplyr::case_when(
    x >= 95 ~ -28,
    x >= 75 ~ -22,
    x <= 35 ~ 18,
    TRUE ~ -16
  )
  dy <- dplyr::case_when(
    y <= 22 ~ 16,
    y >= 58 ~ -16,
    TRUE ~ 12
  )
  c(dx, dy)
}

#' Header block: eyebrow, headline, and subtitle
build_decisive_sequence_header <- function(eyebrow,
                                         headline,
                                         subtitle) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.92,
      label = toupper(eyebrow),
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 4.2,
      colour = SDC_PALETTE[["purple"]],
      hjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.62,
      label = headline,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 7.2,
      colour = SDC_ARTICLE_COLORS$ink,
      hjust = 0,
      vjust = 1,
      lineheight = 0.92
    ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0.08,
      label = subtitle,
      family = SDC_FONTS$body,
      size = 3.8,
      colour = SDC_ARTICLE_COLORS$muted,
      hjust = 0,
      vjust = 0,
      lineheight = 1.1
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 4, 0))
}

#' One metric card grob for the template header row
build_decisive_sequence_metric_card <- function(value,
                                                label,
                                                value_color = SDC_PALETTE[["orange"]]) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      fill = "white",
      colour = SDC_ARTICLE_COLORS$grid,
      linewidth = 0.45
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.72,
      label = value,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 7.2,
      colour = value_color
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.28,
      label = toupper(label),
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 3.1,
      colour = SDC_ARTICLE_COLORS$muted
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}

#' Narrative footer panel
build_decisive_sequence_narrative_panel <- function(chain_text, detail_text) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      fill = SDC_ARTICLE_COLORS$narrative,
      colour = NA
    ) +
    ggplot2::annotate(
      "text",
      x = 0.03,
      y = 0.72,
      label = chain_text,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 4.6,
      colour = SDC_ARTICLE_COLORS$ink,
      hjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = 0.03,
      y = 0.28,
      label = detail_text,
      family = SDC_FONTS$body,
      size = 3.6,
      colour = SDC_ARTICLE_COLORS$muted,
      hjust = 0
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 6, 4, 6))
}

#' Pitch panel with numbered passes, carries, and shots
build_decisive_sequence_pitch_plot <- function(sequence_table,
                                                 team_color = SDC_PALETTE[["blue"]],
                                                 carry_color = SDC_PALETTE[["cyan"]],
                                                 saved_shot_color = SDC_PALETTE[["purple"]],
                                                 goal_color = SDC_PALETTE[["orange"]],
                                                 longest_pass_idx = NA_integer_,
                                                 highlight_carry_players = NULL) {
  passes <- sequence_table %>%
    dplyr::filter(.data$event_type == "Pass") %>%
    dplyr::mutate(pass_number = dplyr::row_number())

  carries <- sequence_table %>%
    dplyr::filter(.data$event_type == "Carry")

  if (!is.null(highlight_carry_players) && nrow(carries) > 0) {
    carries <- carries %>%
      dplyr::filter(.data$player %in% highlight_carry_players)
  }

  shots <- sequence_table %>% dplyr::filter(.data$event_type == "Shot")

  if (nrow(passes) > 0) {
    passes <- passes %>%
      dplyr::mutate(
        marker_x = .data$start_x + 0.58 * (.data$end_x - .data$start_x),
        marker_y = .data$start_y + 0.58 * (.data$end_y - .data$start_y),
        line_width = dplyr::if_else(
          .data$pass_number == longest_pass_idx,
          1.05,
          0.8
        )
      )
  }

  label_players <- sequence_table %>%
    dplyr::filter(.data$event_type %in% c("Pass", "Shot", "Carry")) %>%
    dplyr::group_by(.data$player_label) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  label_offsets <- purrr::map(
    seq_len(nrow(label_players)),
    function(i) {
      auto_sequence_label_offset(
        label_players$start_x[i],
        label_players$start_y[i]
      )
    }
  )
  label_players$label_dx <- purrr::map_dbl(label_offsets, 1)
  label_players$label_dy <- purrr::map_dbl(label_offsets, 2)

  p <- ggplot2::ggplot()

  for (layer in draw_decisive_sequence_pitch_layers()) {
    p <- p + layer
  }

  if (nrow(carries) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = carries,
        ggplot2::aes(
          x = .data$start_x,
          y = .data$start_y,
          xend = .data$end_x,
          yend = .data$end_y
        ),
        colour = carry_color,
        linewidth = 0.75,
        linetype = "22",
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed")
      )
  }

  if (nrow(passes) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = passes,
        ggplot2::aes(
          x = .data$start_x,
          y = .data$start_y,
          xend = .data$end_x,
          yend = .data$end_y,
          linewidth = .data$line_width
        ),
        colour = team_color,
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")
      ) +
      ggplot2::geom_point(
        data = passes,
        ggplot2::aes(x = .data$marker_x, y = .data$marker_y),
        shape = 21,
        size = 3.1,
        fill = team_color,
        colour = "white",
        stroke = 0.7
      ) +
      ggplot2::geom_text(
        data = passes,
        ggplot2::aes(x = .data$marker_x, y = .data$marker_y, label = .data$pass_number),
        family = SDC_FONTS$title,
        fontface = "bold",
        size = 2.6,
        colour = "white"
      ) +
      ggplot2::scale_linewidth(range = c(0.75, 1.05), guide = "none")
  }

  if (nrow(shots) > 0) {
    shot_labels <- letters[seq_len(nrow(shots))]
    shots <- shots %>%
      dplyr::mutate(
        shot_label = shot_labels,
        shot_color = dplyr::case_when(
          .data$outcome == "Goal" ~ goal_color,
          .data$outcome %in% c("Saved", "Blocked") ~ saved_shot_color,
          TRUE ~ SDC_PALETTE[["red"]]
        ),
        line_type = dplyr::if_else(.data$outcome == "Goal", "solid", "22")
      )

    p <- p +
      ggplot2::geom_segment(
        data = shots,
        ggplot2::aes(
          x = .data$start_x,
          y = .data$start_y,
          xend = .data$end_x,
          yend = .data$end_y,
          colour = I(.data$shot_color),
          linetype = I(.data$line_type)
        ),
        linewidth = 0.95,
        lineend = "round",
        arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")
      )

    if (nrow(shots) > 1) {
      p <- p +
        ggplot2::geom_point(
          data = shots,
          ggplot2::aes(x = .data$start_x, y = .data$start_y),
          shape = 21,
          size = 3.8,
          fill = shots$shot_color,
          colour = "white",
          stroke = 0.8
        ) +
        ggplot2::geom_text(
          data = shots,
          ggplot2::aes(x = .data$start_x, y = .data$start_y, label = .data$shot_label),
          family = SDC_FONTS$title,
          fontface = "bold",
          size = 3,
          colour = "white"
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = shots,
          ggplot2::aes(x = .data$start_x, y = .data$start_y),
          shape = 21,
          size = 3.6,
          fill = shots$shot_color[1],
          colour = "white",
          stroke = 0.8
        )
    }
  }

  if (nrow(label_players) > 0) {
    p <- p +
      ggplot2::geom_label(
        data = label_players,
        ggplot2::aes(
          x = .data$start_x,
          y = .data$start_y,
          label = .data$player_label
        ),
        family = SDC_FONTS$title,
        fontface = "bold",
        size = 2.7,
        fill = "white",
        colour = team_color,
        linewidth = 0.25,
        label.padding = ggplot2::unit(0.12, "lines"),
        label.r = grid::unit(0.1, "lines"),
        vjust = 0.5,
        hjust = 0.5,
        nudge_x = label_players$label_dx * 0.04,
        nudge_y = -label_players$label_dy * 0.04
      )
  }

  p +
    ggplot2::scale_x_continuous(limits = c(0, 120), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(80, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed(ratio = 80 / 120) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_sdc(base_size = 8) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = SDC_ARTICLE_COLORS$grid,
        linewidth = 0.45
      ),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

#' UC12: Decisive goal sequence map (article / social template)
viz_decisive_goal_sequence <- function(events_df,
                                       team_name,
                                       match_id = NULL,
                                       meta = NULL,
                                       goal_minute = NULL,
                                       goal_second = NULL,
                                       scorer_name = NULL,
                                       trim_final_third = TRUE,
                                       final_third_x = 80,
                                       team_color = SDC_PALETTE[["red"]],
                                       eyebrow = "The decisive sequence",
                                       headline = NULL,
                                       subtitle = NULL,
                                       chain_text = NULL,
                                       detail_text = NULL,
                                       highlight_carry_players = NULL,
                                       format = c("4_5", "16_9")) {
  format <- match.arg(format)

  goal_event <- find_goal_shot_event(
    events_df,
    team_name = team_name,
    goal_minute = goal_minute,
    goal_second = goal_second,
    scorer_name = scorer_name,
    match_id = match_id
  )

  sequence_df <- extract_goal_buildup_sequence(
    events_df,
    goal_event = goal_event,
    team_name = team_name,
    trim_final_third = trim_final_third,
    final_third_x = final_third_x
  )
  sequence_table <- prepare_goal_sequence_table(sequence_df)
  summary <- summarize_goal_sequence(sequence_df, sequence_table)

  if (is.null(headline)) {
    headline <- decisive_sequence_headline(
      summary$seconds,
      summary$completed_passes
    )
  }
  if (is.null(chain_text)) {
    chain_text <- build_goal_sequence_chain_text(sequence_table)
  }
  if (is.null(detail_text)) {
    team_label <- if (!is.null(meta)) {
      if (identical(team_name, meta$home_team)) meta$display_home else meta$display_away
    } else {
      team_name
    }
    detail_text <- paste0(
      "The ",
      if (summary$combined_xg > 0) {
        paste0(format(round(summary$combined_xg, 2), nsmall = 2), " xG ")
      } else {
        ""
      },
      "finish gave ",
      team_label,
      " the breakthrough."
    )
  }

  metric_row <- patchwork::wrap_plots(
    build_decisive_sequence_metric_card(
      as.character(summary$seconds),
      "Seconds"
    ),
    build_decisive_sequence_metric_card(
      as.character(summary$completed_passes),
      "Completed passes"
    ),
    build_decisive_sequence_metric_card(
      format(round(summary$combined_xg, 2), nsmall = 2),
      "Combined xG"
    ),
    ncol = 3
  )

  pitch_plot <- build_decisive_sequence_pitch_plot(
    sequence_table,
    team_color = team_color,
    longest_pass_idx = summary$longest_pass_idx,
    highlight_carry_players = highlight_carry_players
  )

  narrative_panel <- build_decisive_sequence_narrative_panel(
    chain_text = chain_text,
    detail_text = detail_text
  )

  body <- patchwork::wrap_plots(
    build_decisive_sequence_header(
      eyebrow = eyebrow,
      headline = headline,
      subtitle = subtitle
    ),
    metric_row,
    pitch_plot,
    narrative_panel,
    ncol = 1,
    heights = c(0.16, 0.12, 0.58, 0.12)
  )

  team_label <- if (!is.null(meta)) {
    if (identical(team_name, meta$home_team)) meta$display_home else meta$display_away
  } else {
    team_name
  }

  if (is.null(subtitle)) {
    scorer_label <- passing_network_label(
      goal_event$`player.name`,
      goal_event$player_display_name
    )
    subtitle <- paste0(
      scorer_label,
      "'s ",
      goal_event$minute,
      "' goal — ",
      team_label,
      if (!is.null(meta)) paste0(" ", meta$home_score, "–", meta$away_score) else ""
    )
  }

  body +
    patchwork::plot_annotation(
      caption = paste0(
        "Completed passes (",
        team_label,
        ") · Dashed carries · Shot trajectories to goal"
      ),
      theme = theme_sdc_article(base_size = 11) +
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
          plot.margin = ggplot2::margin(14, 16, 10, 16)
        )
    )
}
