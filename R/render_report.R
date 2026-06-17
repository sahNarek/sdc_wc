#' Build output file basename: Germany_Curacao_statsbomb | Germany_Curacao_all
report_output_basename <- function(home_team,
                                   away_team,
                                   requested_providers,
                                   active_providers) {
  teams_slug <- figure_slug(home_team, away_team)
  requested <- parse_providers_arg(requested_providers)

  provider_slug <- if (length(requested) == 1 && requested == "all") {
    "all"
  } else if (length(active_providers) > 1) {
    "all"
  } else {
    active_providers[1]
  }

  paste(teams_slug, provider_slug, sep = "_")
}

#' Ensure a LaTeX engine is available for PDF reports
ensure_pdf_engine <- function() {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    install.packages("tinytex", repos = "https://cloud.r-project.org")
  }

  if (!tinytex::is_tinytex()) {
    message("Installing TinyTeX (one-time setup for PDF export)...")
    tinytex::install_tinytex()
  }

  invisible(TRUE)
}

#' Default Rmd params for known development / assigned matches
match_report_defaults <- function(match_id) {
  switch(
    as.character(match_id),
    "4036731" = list(
      featured_icon_player = "Jamal Musiala",
      home_color = "#1F77B4",
      away_color = "#FF7F0E"
    ),
    "4036737" = list(
      featured_icon_player = "Lionel Messi",
      home_color = "#74ACDF",
      away_color = "#006233",
      shot_map_scope = "featured_only",
      shot_map_iterate_palette = FALSE,
      shot_map_icon_set = "footprint"
    ),
    list()
  )
}

#' Render the match report to HTML, PDF, or both
#'
#' @param match_id Canonical match ID (StatsBomb ID for development matches)
#' @param format One of "html", "pdf", or "both"
#' @param providers Provider slug(s), comma-separated string, or "all"
#' @param root Project root directory
#' @param output_dir Directory for rendered files
#' @param ... Additional params passed to the Rmd template
render_match_report <- function(match_id = 4036731,
                                format = c("html", "pdf", "both"),
                                providers = "statsbomb",
                                root = get_project_root(),
                                output_dir = file.path(root, "output", "reports"),
                                ...) {
  format <- match.arg(format)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  active_providers <- resolve_providers_for_match(
    providers = providers,
    match_id = match_id,
    root = root
  )

  if (length(active_providers) == 0) {
    stop(
      "No provider data available for match ", match_id,
      ". Requested: ", paste(parse_providers_arg(providers), collapse = ", "),
      call. = FALSE
    )
  }

  primary_provider <- if ("statsbomb" %in% active_providers) {
    "statsbomb"
  } else {
    active_providers[1]
  }
  primary_meta <- load_match_data(primary_provider, match_id, root = root)$meta
  if (nrow(primary_meta) == 0) {
    stop("No metadata for match ", match_id, " (provider: ", primary_provider, ").", call. = FALSE)
  }

  params <- c(
    list(
      match_id = match_id,
      providers = active_providers
    ),
    match_report_defaults(match_id),
    list(...)
  )

  output_basename <- report_output_basename(
    home_team = primary_meta$home_team[1],
    away_team = primary_meta$away_team[1],
    requested_providers = providers,
    active_providers = active_providers
  )

  rmd_path <- file.path(root, "reports", "02_match_report_template.Rmd")
  outputs <- character(0)

  render_one <- function(output_format) {
    ext <- if (output_format == "html_document") "html" else "pdf"
    rmarkdown::render(
      input = rmd_path,
      output_format = output_format,
      output_file = paste0(output_basename, ".", ext),
      output_dir = output_dir,
      params = params,
      quiet = FALSE,
      envir = new.env(parent = globalenv())
    )
  }

  if (format %in% c("html", "both")) {
    message("Rendering HTML report...")
    outputs <- c(outputs, render_one("html_document"))
  }

  if (format %in% c("pdf", "both")) {
    ensure_pdf_engine()
    message("Rendering PDF report...")
    outputs <- c(outputs, render_one("pdf_document"))
  }

  invisible(outputs)
}
