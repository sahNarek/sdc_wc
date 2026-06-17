#' Whether events support coordinate-based charts (UC1–UC8)
events_usable_for_viz <- function(events_df) {
  if (is.null(events_df) || nrow(events_df) == 0) {
    return(FALSE)
  }

  loc_x <- events_df$`location.x` %||% events_df$location_x
  any(!is.na(loc_x))
}

#' Whether a provider section can render summary charts without events
provider_has_summary_viz <- function(provider) {
  provider == "wyscout"
}
