SHIRT_ICON_CACHE <- new.env(parent = emptyenv())

TEAM_PLAYER_ASSETS <- c("portugal", "uzbek", "white", "gk")

#' Path to a team player SVG asset
get_team_player_asset_path <- function(asset_key, root = get_project_root()) {
  if (!asset_key %in% TEAM_PLAYER_ASSETS) {
    stop("Unknown player asset: ", asset_key, call. = FALSE)
  }
  file.path(root, "assets", paste0(asset_key, ".svg"))
}

#' Rasterise a team player SVG to a cached PNG path
render_team_asset_png <- function(asset_key, root = get_project_root()) {
  svg_path <- get_team_player_asset_path(asset_key, root)
  if (!file.exists(svg_path)) {
    stop("Player asset not found: ", svg_path, call. = FALSE)
  }

  cache_dir <- file.path(tempdir(), "sdc_shirt_icons")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(cache_dir, paste0(asset_key, "_base.png"))

  if (!file.exists(out) && requireNamespace("rsvg", quietly = TRUE)) {
    rsvg::rsvg_png(svg_path, out, width = 320, height = 320)
  }

  if (file.exists(out)) {
    return(out)
  }

  svg_path
}

#' Shirt icon with an optional jersey number rendered into the asset
numbered_team_icon_path <- function(asset_key,
                                    jersey_number = NULL,
                                    root = get_project_root()) {
  number_label <- if (is.null(jersey_number) || is.na(jersey_number)) {
    ""
  } else {
    as.character(jersey_number)
  }
  key <- paste0(asset_key, "|", number_label)
  if (exists(key, envir = SHIRT_ICON_CACHE, inherits = FALSE)) {
    return(get(key, envir = SHIRT_ICON_CACHE))
  }

  if (!requireNamespace("magick", quietly = TRUE)) {
    install.packages("magick", repos = "https://cloud.r-project.org")
  }

  base_path <- render_team_asset_png(asset_key, root)
  colored <- magick::image_read(base_path)

  cache_dir <- file.path(tempdir(), "sdc_shirt_icons")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (nzchar(number_label) && !identical(asset_key, "gk")) {
    font_size <- if (nchar(number_label) >= 2) 46 else 54
    number_fill <- if (asset_key %in% c("white", "uzbek")) "#14213D" else "white"
    svg_text <- sprintf(
      paste0(
        '<svg width="320" height="320" xmlns="http://www.w3.org/2000/svg">',
        '<text x="160" y="178" text-anchor="middle" font-size="%d" ',
        'font-weight="700" fill="%s" font-family="Arial, sans-serif">%s</text>',
        "</svg>"
      ),
      font_size,
      number_fill,
      number_label
    )
    svg_path <- file.path(cache_dir, paste0(asset_key, "_num_", number_label, ".svg"))
    num_png <- file.path(cache_dir, paste0(asset_key, "_num_", number_label, ".png"))
    writeLines(svg_text, svg_path, useBytes = TRUE)
    if (requireNamespace("rsvg", quietly = TRUE)) {
      rsvg::rsvg_png(svg_path, num_png, width = 320, height = 320)
      text_img <- magick::image_read(num_png)
    } else {
      text_img <- magick::image_read(svg_path)
    }
    colored <- magick::image_composite(colored, text_img, gravity = "center")
  }

  out <- file.path(
    cache_dir,
    paste0(gsub("[^A-Za-z0-9]+", "_", key), ".png")
  )
  magick::image_write(colored, out)
  assign(key, out, envir = SHIRT_ICON_CACHE)
  out
}

#' Legacy tint helper (generic shirt.svg + colour)
colored_shirt_icon_path <- function(hex_color, root = get_project_root()) {
  numbered_team_icon_path("portugal", jersey_number = NULL, root = root)
}

#' Add per-player team icons (Portugal / white away shirts, GK gloves)
add_colored_shirt_icons <- function(positions_df,
                                    team_color = NULL,
                                    opponent_color = NULL,
                                    defending_asset = "white") {
  if (!defending_asset %in% TEAM_PLAYER_ASSETS) {
    stop("Unknown defending asset: ", defending_asset, call. = FALSE)
  }
  positions_df %>%
    dplyr::mutate(
      player_asset = dplyr::case_when(
        isTRUE(.data$is_keeper) ~ "gk",
        .data$team_side == "attacking" ~ "portugal",
        TRUE ~ defending_asset
      ),
      shirt_image = mapply(
        function(asset, label, is_gk) {
          numbered_team_icon_path(
            asset,
            jersey_number = if (
              !is_gk && !is.na(label) && nzchar(label)
            ) {
              label
            } else {
              NULL
            }
          )
        },
        .data$player_asset,
        dplyr::coalesce(.data$jersey_label, ""),
        .data$is_keeper
      )
    )
}
