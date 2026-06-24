#' Default outcome colours for the goal-mouth panel (SDC palette)
default_goal_outcome_colors <- function(goal_color = SDC_PALETTE[["blue"]]) {
  list(
    Goal = goal_color,
    Saved = SDC_PALETTE[["orange"]],
    Blocked = SDC_PALETTE[["purple"]],
    "Off T" = "#333333",
    Post = SDC_PALETTE[["orange"]],
    Wayward = "#333333"
  )
}

#' Resolve fill colour per shot outcome on the goal panel
goal_panel_outcome_colors <- function(outcomes,
                                      outcome_colors = NULL,
                                      goal_color = NULL) {
  colors <- outcome_colors %||% default_goal_outcome_colors(goal_color = goal_color)
  if (!is.null(goal_color)) {
    colors$Goal <- goal_color
  }
  vapply(
    outcomes,
    function(outcome) {
      col <- colors[[outcome]]
      if (is.null(col)) "#333333" else col
    },
    character(1)
  )
}

#' Binary goal-mouth shapes: scored vs not scored
goal_scored_shapes <- function() {
  c(
    Goal = 21,
    `No Goal` = 22
  )
}

#' Per-outcome goal-mouth shapes (optional detail mode)
goal_outcome_shapes <- function() {
  c(
    Goal = 21,
    Saved = 22,
    "Off T" = 24,
    Post = 23
  )
}

#' Map shot outcomes to goal-panel shape groups
goal_panel_shape_values <- function(outcomes, shape_by = c("binary", "outcome")) {
  shape_by <- match.arg(shape_by)
  if (shape_by == "binary") {
    ifelse(outcomes == "Goal", "Goal", "No Goal")
  } else {
    outcomes
  }
}

#' Impute missing shot end height for goal-panel mapping
#'
#' StatsBomb omits \code{z} on some blocked and wayward attempts even when
#' horizontal end coordinates are present.
impute_shot_end_height <- function(outcome, end_z) {
  dplyr::if_else(
    !is.na(end_z),
    end_z,
    dplyr::case_when(
      outcome == "Blocked" ~ 0.65,
      outcome %in% c("Off T", "Wayward", "Post") ~ 1.0,
      outcome == "Saved" ~ 1.0,
      TRUE ~ 0.85
    )
  )
}

#' Map StatsBomb shot end_location to front-on goal panel coordinates (metres)
#'
#' Horizontal (\code{net_x}): \code{shot.end_location.y} along the 8-yard mouth.
#' Vertical (\code{net_y}): \code{shot.end_location.z} in metres above ground.
map_shot_to_goal_panel <- function(end_y,
                                   end_z,
                                   goal_y_min = GOAL_POST_Y_MIN,
                                   goal_y_max = GOAL_POST_Y_MAX,
                                   goal_width_m = GOAL_WIDTH_M,
                                   goal_height_m = GOAL_HEIGHT_M,
                                   clip = TRUE) {
  goal_width_sb <- goal_y_max - goal_y_min
  net_x_raw <- (end_y - goal_y_min) / goal_width_sb * goal_width_m
  net_y_raw <- end_z

  if (!clip) {
    return(tibble::tibble(net_x = net_x_raw, net_y = net_y_raw))
  }

  tibble::tibble(
    net_x = pmin(pmax(net_x_raw, 0), goal_width_m),
    net_y = pmin(pmax(net_y_raw, 0), goal_height_m),
    net_x_raw = net_x_raw,
    net_y_raw = net_y_raw,
    clipped = (
      net_x_raw < 0 | net_x_raw > goal_width_m |
        net_y_raw < 0 | net_y_raw > goal_height_m
    )
  )
}

#' Goal-panel marker positions: off-target shots sit just outside the frame
#'
#' Off-target markers use a fixed offset from the nearest post/crossbar corner
#' (SofaScore-style) rather than true end coordinates, which can sit far away.
add_goal_net_display_positions <- function(net_data,
                                           goal_width_m = GOAL_WIDTH_M,
                                           goal_height_m = GOAL_HEIGHT_M,
                                           icon_size = 0.22,
                                           miss_offset_x_frac = 0.038,
                                           miss_offset_y_frac = 0.11) {
  off_x <- goal_width_m * miss_offset_x_frac
  off_y <- goal_height_m * miss_offset_y_frac
  off_y_low <- goal_height_m * 0.07
  icon_r <- goal_height_m * icon_size * 0.42
  off_target <- c("Off T", "Wayward", "Post")

  net_data %>%
    dplyr::mutate(
      .miss = .data$`shot.outcome.name` %in% off_target & .data$clipped,
      .base_x = dplyr::case_when(
        .data$.miss & .data$net_x_raw < 0 ~ -off_x,
        .data$.miss & .data$net_x_raw > goal_width_m ~ goal_width_m + off_x,
        TRUE ~ .data$net_x_raw
      ),
      .base_y = dplyr::case_when(
        .data$.miss & .data$net_y_raw > goal_height_m ~ goal_height_m + off_y,
        .data$.miss & .data$net_y_raw < 0 ~ -off_y_low,
        .data$.miss ~ pmin(pmax(.data$net_y_raw, 0), goal_height_m),
        .data$`shot.outcome.name` == "Saved" &
          .data$net_y_raw >= goal_height_m - 0.08 ~
          goal_height_m - icon_r * 0.15,
        TRUE ~ .data$net_y_raw
      ),
      .corner = dplyr::case_when(
        !.data$.miss ~ NA_character_,
        .data$.base_x < 0 & .data$.base_y > goal_height_m * 0.85 ~ "top_left",
        .data$.base_x > goal_width_m & .data$.base_y > goal_height_m * 0.85 ~
          "top_right",
        .data$.base_y > goal_height_m ~ "top_mid",
        .data$.base_y < 0 ~ "bottom_mid",
        .data$.base_x < 0 ~ "left_mid",
        .data$.base_x > goal_width_m ~ "right_mid",
        TRUE ~ "mid"
      )
    ) %>%
    dplyr::group_by(.data$.corner) %>%
    dplyr::mutate(
      .corner_n = dplyr::n(),
      .corner_i = dplyr::row_number(),
      .spread = (.data$.corner_i - (.data$.corner_n + 1) / 2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      net_x = dplyr::if_else(
        .data$.miss,
        .data$.base_x + .data$.spread * goal_width_m * 0.055,
        .data$net_x_raw
      ),
      net_y = dplyr::if_else(
        .data$.miss | (.data$`shot.outcome.name` == "Saved" &
          .data$net_y_raw >= goal_height_m - 0.08),
        .data$.base_y + dplyr::if_else(
          .data$.miss,
          dplyr::case_when(
            .data$.corner %in% c("top_left", "top_right", "top_mid") ~
              abs(.data$.spread) * goal_height_m * 0.07,
            TRUE ~ abs(.data$.spread) * goal_height_m * 0.045
          ),
          0
        ),
        .data$net_y_raw
      )
    ) %>%
    dplyr::select(-dplyr::starts_with("."))
}

#' Shots with valid end locations for the goal panel; others stay pitch-only
#'
#' Any shot with \code{shot.end_location.x/y/z} is mapped onto the front-on goal
#' panel (including off-target and saved attempts that stop short of the line).
#' Shots without end-height data remain pitch-only.
filter_shots_for_goal_net <- function(data,
                                      goal_width_m = GOAL_WIDTH_M,
                                      goal_height_m = GOAL_HEIGHT_M) {
  if (!"shot.end_location.x" %in% names(data)) {
    stop(
      "Shot end locations are not available. Rebuild processed data with ",
      "shot.end_location parsing enabled.",
      call. = FALSE
    )
  }

  if (!"shot.end_location.z" %in% names(data)) {
    stop(
      "Shot end height (shot.end_location.z) is not available. Rebuild processed ",
      "data with shot.end_location z parsing enabled.",
      call. = FALSE
    )
  }

  data <- data %>%
    dplyr::filter(
      !is.na(.data$`shot.end_location.x`),
      !is.na(.data$`shot.end_location.y`)
    ) %>%
    dplyr::mutate(
      shot_end_z_plot = impute_shot_end_height(
        .data$`shot.outcome.name`,
        .data$`shot.end_location.z`
      )
    )

  if (nrow(data) == 0) {
    stop("No shots with end-location data found.", call. = FALSE)
  }

  on_net <- data %>%
    dplyr::filter(!is.na(.data$shot_end_z_plot))

  if (nrow(on_net) > 0) {
    mapped <- map_shot_to_goal_panel(
      end_y = on_net$`shot.end_location.y`,
      end_z = on_net$shot_end_z_plot,
      goal_width_m = goal_width_m,
      goal_height_m = goal_height_m,
      clip = TRUE
    )
    on_net <- dplyr::bind_cols(on_net, mapped) %>%
      dplyr::mutate(on_goal_panel = TRUE)
  } else {
    on_net <- on_net %>%
      dplyr::mutate(
        on_goal_panel = TRUE,
        net_x = double(),
        net_y = double(),
        net_x_raw = double(),
        net_y_raw = double(),
        clipped = logical()
      )
  }

  pitch_only <- data %>%
    dplyr::filter(is.na(.data$shot_end_z_plot)) %>%
    dplyr::mutate(
      on_goal_panel = FALSE,
      net_x = NA_real_,
      net_y = NA_real_,
      net_x_raw = NA_real_,
      net_y_raw = NA_real_,
      clipped = NA
    )

  dplyr::bind_rows(on_net, pitch_only)
}

#' Summary counts for optional UC8 header strip (player- or team-filtered data)
compute_shot_summary_stats <- function(data, subject_label = NULL) {
  outcomes <- data$`shot.outcome.name`
  goals <- sum(outcomes == "Goal", na.rm = TRUE)
  on_target <- sum(outcomes %in% c("Goal", "Saved"), na.rm = TRUE)
  saved <- sum(outcomes == "Saved", na.rm = TRUE)
  blocked <- sum(outcomes == "Blocked", na.rm = TRUE)
  missed <- sum(outcomes %in% c("Off T", "Wayward", "Post"), na.rm = TRUE)

  name_prefix <- if (!is.null(subject_label) && nzchar(subject_label)) {
    paste0(toupper(subject_label), " — ")
  } else {
    ""
  }

  list(
    goals = goals,
    on_target = on_target,
    saved = saved,
    blocked = blocked,
    missed = missed,
    total_shots = nrow(data),
    label = paste0(
      name_prefix,
      "SHOTS: ", nrow(data),
      "  |  ON TARGET: ", on_target,
      "  |  SAVED: ", saved,
      "  |  MISSED: ", missed,
      "  |  BLOCKED: ", blocked,
      "  |  GOALS: ", goals
    )
  )
}

#' Compact horizontal bar chart for UC8 shot summary strip (SDC palette)
plot_shot_summary_bar <- function(stats, subject_label = NULL) {
  metrics <- c("Shots", "On target", "Saved", "Missed", "Blocked", "Goals")
  df <- tibble::tibble(
    metric = factor(metrics, levels = rev(metrics)),
    value = c(
      stats$total_shots,
      stats$on_target,
      stats$saved,
      stats$missed,
      stats$blocked,
      stats$goals
    )
  )

  fills <- c(
    Goals = SDC_PALETTE[["green"]],
    Blocked = SDC_PALETTE[["purple"]],
    Missed = SDC_PALETTE[["red"]],
    Saved = SDC_PALETTE[["orange"]],
    `On target` = SDC_PALETTE[["blue"]],
    Shots = SDC_PALETTE[["cyan"]]
  )

  p <- ggplot(df, aes(x = .data$value, y = .data$metric, fill = .data$metric)) +
    geom_col(width = 0.68, colour = NA) +
    geom_text(
      aes(label = .data$value),
      hjust = -0.2,
      family = SDC_FONTS$body,
      size = 3.2,
      colour = "#333333"
    ) +
    scale_fill_manual(values = fills, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(x = NULL, y = NULL) +
    theme_sdc(base_size = 9) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        family = SDC_FONTS$body,
        size = 8.5,
        colour = "#333333"
      ),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(0, 10, 0, 10)
    )

  if (!is.null(subject_label) && nzchar(subject_label)) {
    p <- p +
      labs(title = toupper(subject_label)) +
      theme(
        plot.title = element_text(
          family = SDC_FONTS$title,
          face = "bold",
          size = 10,
          colour = "#111111",
          hjust = 0,
          margin = margin(b = 1)
        ),
        plot.margin = margin(0, 10, 0, 10)
      )
  }

  p
}

#' Shot-outcome colours for pie and compact bar charts
shot_outcome_pie_colors <- function() {
  c(
    Goals = SDC_PALETTE[["green"]],
    Saved = SDC_PALETTE[["orange"]],
    `On target` = SDC_PALETTE[["blue"]],
    Missed = SDC_PALETTE[["red"]],
    Blocked = SDC_PALETTE[["purple"]]
  )
}

#' Compact horizontal bar chart of shot outcomes
plot_shot_outcome_bar_compact <- function(stats, title = "Shots") {
  df <- tibble::tibble(
    outcome = factor(
      c("Goals", "Saved", "On target", "Missed", "Blocked"),
      levels = c("Blocked", "Missed", "On target", "Saved", "Goals")
    ),
    value = c(
      stats$goals,
      stats$saved,
      stats$on_target,
      stats$missed,
      stats$blocked
    )
  )

  fills <- shot_outcome_pie_colors()

  ggplot(df, aes(x = .data$value, y = .data$outcome, fill = .data$outcome)) +
    geom_col(width = 0.62, colour = NA) +
    geom_text(
      aes(label = .data$value),
      hjust = -0.15,
      family = SDC_FONTS$body,
      size = 4.2,
      colour = "#111111",
      fontface = "bold"
    ) +
    scale_fill_manual(values = fills, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = title, x = NULL, y = NULL) +
    theme_sdc(base_size = 11) +
    theme(
      plot.title = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 11,
        colour = "#111111",
        hjust = 0,
        margin = margin(b = 2)
      ),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        family = SDC_FONTS$body,
        size = 9.5,
        colour = "#111111",
        face = "plain"
      ),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(2, 10, 0, 2)
    )
}

#' Square avatar placeholder for a featured-player headshot (optional image path)
plot_player_image_slot <- function(image_path = NULL, subject_label = NULL) {
  if (!requireNamespace("ggimage", quietly = TRUE)) {
    install.packages("ggimage", repos = "https://cloud.r-project.org")
  }

  label <- if (!is.null(subject_label) && nzchar(subject_label)) {
    "Insert\nimage"
  } else {
    "Insert\nimage"
  }

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0.18,
      xmax = 0.82,
      ymin = 0.18,
      ymax = 0.82,
      fill = "#F7F7F7",
      colour = "#C8C8C8",
      linewidth = 0.55,
      linetype = "22"
    ) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      aspect.ratio = 1,
      plot.margin = ggplot2::margin(0, 4, 2, 4)
    )

  if (!is.null(image_path) && nzchar(image_path) && file.exists(image_path)) {
    p <- p +
      ggimage::geom_image(
        ggplot2::aes(x = 0.5, y = 0.5, image = image_path),
        size = 0.58
      )
  } else {
    p <- p +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = label,
        family = SDC_FONTS$body,
        size = 2.2,
        colour = "#9A9A9A",
        lineheight = 0.9
      )
  }

  p
}

#' Left summary block: compact shot outcome bars
plot_shot_outcomes_with_player_slot <- function(stats,
                                                subject_label = NULL,
                                                player_image_path = NULL) {
  plot_shot_outcome_bar_compact(stats)
}

#' Pie chart of shot outcomes (goals, saved, missed, blocked)
plot_shot_outcome_pie <- function(stats, subject_label = NULL) {
  df <- tibble::tibble(
    outcome = c("Goals", "Saved", "Missed", "Blocked"),
    value = c(stats$goals, stats$saved, stats$missed, stats$blocked)
  ) %>%
    dplyr::filter(.data$value > 0)

  if (nrow(df) == 0) {
    df <- tibble::tibble(outcome = "No shots", value = 1)
  }

  fills <- shot_outcome_pie_colors()

  p <- ggplot(df, aes(x = "", y = .data$value, fill = .data$outcome)) +
    geom_col(width = 1, colour = "white", linewidth = 0.6) +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = paste0(.data$outcome, "\n", .data$value)),
      position = position_stack(vjust = 0.5),
      family = SDC_FONTS$body,
      size = 4.2,
      colour = "#111111",
      lineheight = 0.95,
      fontface = "bold"
    ) +
    scale_fill_manual(values = fills, guide = "none") +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_void(base_family = SDC_FONTS$body) +
    theme(
      plot.margin = margin(2, 4, 2, 4),
      legend.position = "none"
    )

  if (!is.null(subject_label) && nzchar(subject_label)) {
    p <- p +
      labs(title = toupper(subject_label)) +
      theme(
        plot.title = element_text(
          family = SDC_FONTS$title,
          face = "bold",
          size = 11,
          colour = "#111111",
          hjust = 0.5,
          margin = margin(b = 3)
        )
      )
  }

  p
}

#' Center-forward attacking metrics for the rose chart (no shot-count overlap)
CF_ATTACKING_ROSE_METRICS <- list(
  list(label = "NP xG", column = "player_match_np_xg", digits = 2),
  list(label = "Box touches", column = "player_match_touches_inside_box", digits = 0),
  list(label = "xG chain", column = "player_match_op_xgchain", digits = 2),
  list(label = "Pressures", column = "player_match_pressures", digits = 0),
  list(label = "F3 passes", column = "player_match_op_f3_passes", digits = 0),
  list(label = "Aerials won", column = "player_match_successful_aerials", digits = 0),
  list(label = "Press regains", column = "player_match_pressure_regains", digits = 0)
)

#' @rdname CF_ATTACKING_ROSE_METRICS
CF_ATTACKING_PIZZA_METRICS <- CF_ATTACKING_ROSE_METRICS

#' Normalize a player metric against the match pool (0–1 scale)
normalize_match_player_metric <- function(player_match_stats_df,
                                          metric_col,
                                          player_id,
                                          min_minutes = 30) {
  pool <- player_match_stats_df %>%
    dplyr::filter(
      !is.na(.data$player_match_minutes),
      .data$player_match_minutes >= min_minutes
    )

  if (nrow(pool) == 0 || !metric_col %in% names(pool)) {
    return(0)
  }

  player_val <- pool %>%
    dplyr::filter(.data$player_id == !!player_id) %>%
    dplyr::pull(.data[[metric_col]])

  if (length(player_val) == 0 || is.na(player_val[[1]])) {
    return(0)
  }

  match_max <- max(pool[[metric_col]], na.rm = TRUE)
  if (is.na(match_max) || match_max <= 0) {
    return(0)
  }

  as.numeric(pmin(player_val[[1]] / match_max, 1))
}

#' Build normalized center-forward attacking metrics for a rose chart
compute_cf_attacking_rose_metrics <- function(player_match_stats_df, player_id) {
  purrr::map_dfr(CF_ATTACKING_ROSE_METRICS, function(spec) {
    raw <- player_match_stats_df %>%
      dplyr::filter(.data$player_id == !!player_id) %>%
      dplyr::pull(.data[[spec$column]])

    raw_val <- if (length(raw) == 0 || is.na(raw[[1]])) 0 else as.numeric(raw[[1]])
    normalized <- normalize_match_player_metric(
      player_match_stats_df,
      spec$column,
      player_id
    )

    display <- if (spec$digits == 0) {
      as.character(as.integer(round(raw_val)))
    } else {
      format(round(raw_val, spec$digits), nsmall = spec$digits)
    }

    tibble::tibble(
      metric = spec$label,
      raw_value = raw_val,
      value = normalized,
      display = display
    )
  })
}

#' @rdname compute_cf_attacking_rose_metrics
compute_cf_attacking_pizza_metrics <- compute_cf_attacking_rose_metrics

#' Nightingale rose chart of center-forward attacking metrics (match-normalized)
plot_cf_attacking_rose <- function(metrics_df,
                                   fill_color = SDC_PALETTE[["red"]],
                                   title = "Attacking profile") {
  if (nrow(metrics_df) == 0) {
    stop("No attacking metrics available for rose chart.", call. = FALSE)
  }

  metrics_df <- metrics_df %>%
    dplyr::mutate(
      metric_axis = paste0(.data$metric, "\n", .data$display),
      metric = factor(.data$metric_axis, levels = .data$metric_axis),
      value_plot = pmax(.data$value, 0.04)
    )

  ggplot(metrics_df, aes(x = .data$metric, y = .data$value_plot)) +
    geom_hline(
      yintercept = c(0.25, 0.5, 0.75),
      colour = "#E6E6E6",
      linewidth = 0.35
    ) +
    geom_col(
      fill = fill_color,
      width = 0.88,
      colour = "white",
      linewidth = 0.45
    ) +
    coord_polar(start = -pi / 2, clip = "off") +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_void(base_family = SDC_FONTS$body) +
    theme(
      plot.title = element_text(
        family = SDC_FONTS$title,
        face = "bold",
        size = 10.5,
        colour = "#111111",
        hjust = 0.5,
        margin = margin(b = 3)
      ),
      axis.text.x = element_text(
        family = SDC_FONTS$body,
        size = 7.8,
        colour = "#111111",
        face = "plain",
        lineheight = 0.92,
        margin = margin(t = 6, b = 2)
      ),
      plot.margin = margin(6, 14, 6, 14),
      legend.position = "none"
    )
}

#' @rdname plot_cf_attacking_rose
plot_cf_attacking_pizza <- plot_cf_attacking_rose

#' Compact match summary table for a featured player
plot_player_match_summary_table <- function(player_match_stats_df,
                                            player_id,
                                            subject_label = NULL) {
  row <- player_match_stats_df %>%
    dplyr::filter(.data$player_id == !!player_id) %>%
    dplyr::slice(1)

  if (nrow(row) == 0) {
    stop("Player not found in player_match_stats.", call. = FALSE)
  }

  fmt_num <- function(x, digits = 0) {
    if (is.na(x)) {
      return("–")
    }
    if (digits == 0) {
      as.character(as.integer(round(x)))
    } else {
      format(round(x, digits), nsmall = digits)
    }
  }

  fmt_pct <- function(x) {
    if (is.na(x)) {
      return("–")
    }
    scales::percent(x, accuracy = 1)
  }

  summary_df <- tibble::tibble(
    stat = c(
      "Minutes",
      "Pass accuracy",
      "Assists",
      "OP xG buildup",
      "Touches",
      "Shot OBV",
      "Successful passes"
    ),
    value = c(
      fmt_num(row$player_match_minutes, 0),
      if ("player_match_passing_ratio" %in% names(row)) {
        fmt_pct(row$player_match_passing_ratio)
      } else {
        "–"
      },
      fmt_num(row$player_match_assists, 0),
      fmt_num(row$player_match_op_xgbuildup, 2),
      fmt_num(row$player_match_touches, 0),
      fmt_num(row$player_match_obv_shot, 2),
      fmt_num(row$player_match_successful_passes, 0)
    ),
    row_id = seq_len(7)
  )

  title_label <- if (!is.null(subject_label) && nzchar(subject_label)) {
    toupper(subject_label)
  } else {
    "MATCH SUMMARY"
  }

  ggplot(summary_df, aes(x = 1, y = -row_id)) +
    annotate(
      "text",
      x = 0.15,
      y = 0.4,
      label = title_label,
      family = SDC_FONTS$title,
      fontface = "bold",
      size = 4.8,
      colour = "#111111",
      hjust = 0
    ) +
    geom_text(
      aes(x = 0.15, label = stat),
      family = SDC_FONTS$body,
      size = 4.2,
      colour = "#111111",
      hjust = 0
    ) +
    geom_text(
      aes(x = 0.95, label = value),
      family = SDC_FONTS$body,
      size = 4.2,
      colour = "#111111",
      fontface = "bold",
      hjust = 1
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-nrow(summary_df) - 0.5, 0.8), clip = "off") +
    theme_void() +
    theme(plot.margin = margin(2, 8, 2, 8))
}

#' Summary row: player slot + shot bars, attacking rose chart, and match table
plot_shot_map_summary_row <- function(stats,
                                      subject_label = NULL,
                                      player_match_stats_df = NULL,
                                      player_id = NULL,
                                      shot_color = SDC_PALETTE[["red"]],
                                      player_image_path = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  left_plot <- plot_shot_outcomes_with_player_slot(
    stats = stats,
    subject_label = subject_label,
    player_image_path = player_image_path
  )

  if (
    is.null(player_match_stats_df) ||
      is.null(player_id) ||
      nrow(player_match_stats_df) == 0
  ) {
    return(left_plot)
  }

  rose_metrics <- compute_cf_attacking_rose_metrics(
    player_match_stats_df,
    player_id = player_id
  )

  rose_plot <- plot_cf_attacking_rose(
    rose_metrics,
    fill_color = shot_color,
    title = "Attacking profile"
  )

  table_plot <- plot_player_match_summary_table(
    player_match_stats_df,
    player_id = player_id,
    subject_label = subject_label
  )

  patchwork::wrap_plots(
    left_plot,
    rose_plot,
    table_plot,
    ncol = 3,
    widths = c(0.28, 0.40, 0.32)
  )
}

#' Compute minute-label positions that clear the ball icon and goal frame
add_goal_net_minute_label_positions <- function(net_data,
                                              goal_width_m = GOAL_WIDTH_M,
                                              goal_height_m = GOAL_HEIGHT_M,
                                              icon_size = 0.22) {
  icon_r <- goal_height_m * icon_size * 0.42
  text_gap <- goal_height_m * 0.07
  frame_pad <- goal_height_m * 0.05
  side_pad <- goal_width_m * 0.07

  net_data %>%
    dplyr::mutate(
      .above_y = .data$net_y + .env$icon_r + .env$text_gap,
      .below_y = .data$net_y - .env$icon_r - .env$text_gap,
      label_y = dplyr::case_when(
        .data$.above_y > .env$goal_height_m + .env$icon_r * 0.25 ~
          .data$.above_y,
        .data$.above_y > .env$goal_height_m - .env$frame_pad ~
          .env$goal_height_m + .env$icon_r + .env$text_gap * 0.85,
        .data$net_y < .env$icon_r + .env$frame_pad ~ .data$.above_y,
        TRUE ~ .data$.above_y
      ),
      label_vjust = 0,
      label_x = dplyr::case_when(
        .data$net_x < .env$side_pad ~ .data$net_x + .env$side_pad * 0.85,
        .data$net_x > .env$goal_width_m - .env$side_pad ~
          .data$net_x - .env$side_pad * 0.85,
        TRUE ~ .data$net_x
      ),
      label_hjust = dplyr::case_when(
        .data$net_x < .env$side_pad ~ 0,
        .data$net_x > .env$goal_width_m - .env$side_pad ~ 1,
        TRUE ~ 0.5
      ),
      .label_cluster = paste0(
        round(.data$net_x, 1),
        "_",
        round(.data$net_y, 1)
      )
    ) %>%
    dplyr::group_by(.data$.label_cluster) %>%
    dplyr::arrange(.data$minute, .by_group = TRUE) %>%
    dplyr::mutate(
      .cluster_i = dplyr::row_number(),
      .cluster_n = dplyr::n(),
      label_y = .data$label_y +
        (.data$.cluster_i - 1) * .env$goal_height_m * 0.085,
      label_x = .data$label_x + (.data$.cluster_i - 1) *
        .env$goal_width_m * 0.05 *
        dplyr::if_else(.data$.cluster_i %% 2L == 0L, 1, -1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::starts_with("."))
}

#' UC8: Combined shot map (pitch + trajectories) and goal-mouth panel
viz_shot_map_goal_net <- function(events_df,
                                  player_name = NULL,
                                  player_id = NULL,
                                  team_name = NULL,
                                  both_teams = FALSE,
                                  match_id = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  title_suffix = "shot map and goal mouth",
                                  exclude_penalties = TRUE,
                                  exclude_shot_minutes = NULL,
                                  shot_color = SDC_PALETTE[["red"]],
                                  team_colors = NULL,
                                  lightest_color = NULL,
                                  gradient_colors = NULL,
                                  goal_net_bg_svg = NULL,
                                  goal_width_m = GOAL_WIDTH_M,
                                  goal_height_m = GOAL_HEIGHT_M,
                                  goal_shape_by = c("binary", "outcome"),
                                  show_minute_labels = TRUE,
                                  show_xg_labels = FALSE,
                                  show_trajectories = TRUE,
                                  show_summary = TRUE,
                                  xg_limits = c(0, 0.8),
                                  outcome_colors = NULL,
                                  goal_panel_width_frac = 0.72,
                                  goal_section_height = 0.42,
                                  goal_display_tallness = 2.35,
                                  show_goal_net_legend = FALSE,
                                  show_pitch_subtitle = FALSE,
                                  marker_size = 4.5,
                                  icon_size = 0.12,
                                  icon_set = "footprint",
                                  goal_net_use_ball_icons = TRUE,
                                  goal_net_icon_size = 0.22,
                                  show_attacking_heatmap = FALSE,
                                  heatmap_color = NULL,
                                  player_match_stats_df = NULL,
                                  player_image_path = NULL,
                                  team_labels = NULL,
                                  display_home = NULL,
                                  display_away = NULL) {
  goal_shape_by <- match.arg(goal_shape_by)

  data <- filter_shot_map_data(
    events_df,
    player_name = player_name,
    player_id = player_id,
    team_name = team_name,
    both_teams = both_teams,
    match_id = match_id,
    exclude_penalties = exclude_penalties,
    exclude_shot_minutes = exclude_shot_minutes,
    restrict_to_attacking_third = FALSE
  )

  data <- filter_shots_for_goal_net(
    data,
    goal_width_m = goal_width_m,
    goal_height_m = goal_height_m
  )

  label <- resolve_shot_map_label(
    data,
    player_name = player_name,
    team_name = team_name,
    both_teams = both_teams,
    team_labels = team_labels,
    display_home = display_home,
    display_away = display_away
  )
  shot_colors <- resolve_single_hue_gradient(
    color = shot_color,
    lightest_color = lightest_color,
    gradient_colors = gradient_colors,
    n = 9,
    variant = "shot_map"
  )
  stats <- compute_shot_summary_stats(data, subject_label = label)
  resolved_outcome_colors <- outcome_colors %||% default_goal_outcome_colors(
    goal_color = shot_color
  )

  goal_shape_values <- if (goal_shape_by == "binary") {
    goal_scored_shapes()
  } else {
    goal_outcome_shapes()
  }

  net_data <- data %>%
    dplyr::filter(.data$on_goal_panel) %>%
    dplyr::mutate(
      goal_shape = goal_panel_shape_values(
        .data$`shot.outcome.name`,
        shape_by = goal_shape_by
      ),
      marker_fill = goal_panel_outcome_colors(
        .data$`shot.outcome.name`,
        outcome_colors = resolved_outcome_colors,
        goal_color = shot_color
      ),
      marker_alpha = dplyr::case_when(
        .data$`shot.outcome.name` == "Goal" ~ 1,
        .data$`shot.outcome.name` == "Saved" ~ 0.9,
        isTRUE(.data$clipped) ~ 0.55,
        TRUE ~ 0.8
      )
    ) %>%
    add_goal_net_display_positions(
      goal_width_m = goal_width_m,
      goal_height_m = goal_height_m,
      icon_size = goal_net_icon_size,
      miss_offset_x_frac = 0.055,
      miss_offset_y_frac = 0.14
    )

  if (isTRUE(goal_net_use_ball_icons)) {
    ensure_ball_icon()
    ensure_gloves_icon()
    net_data <- add_goal_net_ball_icons(net_data)
  }

  goal_layout <- goal_panel_layout(
    goal_width_m = goal_width_m,
    goal_height_m = goal_height_m,
    display_tallness = if (isTRUE(show_attacking_heatmap)) {
      2.1
    } else {
      goal_display_tallness
    },
    icon_size = goal_net_icon_size
  )

  goal_marker_size <- marker_size * 1.08

  goal_plot <- ggplot() +
    draw_goal_net(
      width_m = goal_width_m,
      height_m = goal_height_m,
      bg_svg = goal_net_bg_svg
    )

  if (isTRUE(goal_net_use_ball_icons)) {
    if (!requireNamespace("ggimage", quietly = TRUE)) {
      install.packages("ggimage", repos = "https://cloud.r-project.org")
    }

    goal_plot <- goal_plot +
      ggimage::geom_image(
        data = net_data,
        aes(
          x = .data$net_x,
          y = .data$net_y,
          image = .data$colored_icon
        ),
        size = goal_net_icon_size
      )
  } else {
    goal_plot <- goal_plot +
      geom_point(
        data = net_data,
        aes(
          x = .data$net_x,
          y = .data$net_y,
          shape = .data$goal_shape,
          alpha = .data$marker_alpha
        ),
        fill = net_data$marker_fill,
        size = goal_marker_size,
        colour = "#222222",
        stroke = 0.35
      ) +
      scale_shape_manual(values = goal_shape_values, guide = "none") +
      scale_alpha_identity()
  }

  goal_plot <- goal_plot +
    coord_fixed(
      ratio = goal_layout$coord_ratio,
      xlim = goal_layout$xlim,
      ylim = goal_layout$ylim,
      clip = "off",
      expand = FALSE
    ) +
    theme_sdc() +
    theme(
      aspect.ratio = goal_layout$aspect_ratio,
      plot.subtitle = element_text(
        hjust = 0.5,
        face = "bold",
        size = rel(1.05),
        margin = margin(b = 1)
      ),
      plot.margin = margin(t = 4, r = 2, b = 0, l = 2),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )

  label_data <- if (isTRUE(show_minute_labels) && nrow(net_data) > 0) {
    net_data %>%
      dplyr::filter(!is.na(.data$minute)) %>%
      add_goal_net_minute_label_positions(
        goal_width_m = goal_width_m,
        goal_height_m = goal_height_m,
        icon_size = goal_net_icon_size
      )
  } else {
    net_data[0, , drop = FALSE]
  }

  if (nrow(label_data) > 0) {
    goal_plot <- goal_plot +
      geom_label(
        data = label_data,
        aes(
          x = .data$label_x,
          y = .data$label_y,
          label = paste0(.data$minute, "'"),
          colour = .data$marker_fill,
          vjust = .data$label_vjust,
          hjust = .data$label_hjust
        ),
        size = 3.1,
        fontface = "bold",
        family = SDC_FONTS$body,
        fill = "white",
        linewidth = 0,
        label.padding = grid::unit(0.15, "lines"),
        alpha = 0.92,
        show.legend = FALSE
      ) +
      scale_colour_identity()
  }

  goal_legend_plot <- plot_goal_mouth_ball_legend()
  goal_plot <- patchwork::wrap_plots(
    goal_plot,
    goal_legend_plot,
    ncol = 1,
    heights = c(1, 0.12)
  )

  pitch_data <- add_shot_trajectory_endpoints(data) %>%
    add_colored_shot_icons(
      shot_color = shot_color,
      lightest_color = lightest_color,
      gradient_colors = gradient_colors,
      team_colors = team_colors,
      limits = xg_limits,
      icon_set = icon_set
    )

  pitch_x_min <- min(
    85,
    floor(min(pitch_data$location.x, na.rm = TRUE) - 1.5)
  )

  pitch_field_plot <- ggplot() +
    draw_pitch_half_attacking(x_min = pitch_x_min) +
    build_shot_map_pitch_icon_layers(
      data = pitch_data,
      shot_colors = shot_colors,
      shot_color = shot_color,
      team_colors = team_colors,
      icon_size = icon_size,
      xg_limits = xg_limits,
      show_trajectories = show_trajectories,
      show_xg_labels = isTRUE(show_xg_labels),
      icon_set = icon_set
    ) +
    labs(x = NULL, y = NULL) +
    theme_sdc() +
    theme(
      legend.position = "none",
      plot.subtitle = element_blank(),
      plot.margin = margin(t = 0, r = 2, b = 0, l = 2),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  if (isTRUE(show_pitch_subtitle)) {
    pitch_field_plot <- pitch_field_plot +
      labs(subtitle = "Shot origins and trajectories") +
      theme(
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = rel(1.05))
      )
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }

  heatmap_legend <- NULL
  if (isTRUE(show_attacking_heatmap)) {
    heatmap_legend <- prepare_shot_map_attacking_heatmap(
      events_df = events_df,
      player_id = player_id,
      player_name = player_name,
      match_id = match_id,
      heat_color = heatmap_color %||% shot_color
    )
  }

  pitch_plot <- assemble_shot_map(
    pitch_field_plot,
    icon_set = icon_set,
    icon_color = shot_color,
    shot_colors = shot_colors,
    xg_limits = xg_limits,
    show_xg_legend = is.null(team_colors),
    show_trajectory_legend = isTRUE(show_trajectories)
  )

  if (isTRUE(show_attacking_heatmap)) {
    heatmap_panel <- assemble_player_heatmap(
      heatmap_legend$plot,
      heat_colors = heatmap_legend$heat_colors,
      legend_title = "Share of attacking actions",
      legend_limits = heatmap_legend$legend_limits,
      compact_legend = TRUE
    )

    goal_row <- patchwork::wrap_plots(
      goal_plot,
      heatmap_panel,
      ncol = 2,
      widths = c(0.52, 0.48)
    )
  } else {
    goal_row <- wrap_goal_panel_block(
      goal_plot,
      width_frac = goal_panel_width_frac
    )
  }

  goal_section_height_eff <- if (isTRUE(show_attacking_heatmap)) {
    0.58
  } else {
    goal_section_height
  }

  pitch_height <- 1 - goal_section_height_eff
  panels <- list(goal_row, pitch_plot)
  heights <- c(goal_section_height_eff, pitch_height)

  if (show_summary) {
    summary_plot <- plot_shot_map_summary_row(
      stats = stats,
      subject_label = label,
      player_match_stats_df = player_match_stats_df,
      player_id = player_id,
      shot_color = shot_color,
      player_image_path = player_image_path
    )

    panels <- c(list(summary_plot), panels)
    summary_height <- 0.24
    scale <- (1 - summary_height)
    heights <- c(
      summary_height,
      goal_section_height_eff * scale,
      pitch_height * scale
    )
  }

  combined <- patchwork::wrap_plots(panels, ncol = 1, heights = heights) +
    patchwork::plot_layout(heights = heights, guides = "keep") +
    patchwork::plot_annotation(
      title = title %||% player_chart_title(label, title_suffix),
      subtitle = subtitle,
      theme = theme_sdc()
    )

  combined
}
