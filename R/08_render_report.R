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

#' Render the match report to HTML, PDF, or both
#'
#' @param match_id StatsBomb match ID
#' @param format One of "html", "pdf", or "both"
#' @param root Project root directory
#' @param output_dir Directory for rendered files
#' @param ... Additional params passed to the Rmd template
render_match_report <- function(match_id = 4036731,
                                format = c("html", "pdf", "both"),
                                root = get_project_root(),
                                output_dir = file.path(root, "output", "reports"),
                                ...) {
  format <- match.arg(format)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  rmd_path <- file.path(root, "reports", "02_match_report_template.Rmd")
  params <- c(list(match_id = match_id), list(...))

  outputs <- character(0)

  render_one <- function(output_format) {
    rmarkdown::render(
      input = rmd_path,
      output_format = output_format,
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
