#' Named list of provider build functions (sourced from R/providers/)
get_provider_registry <- function() {
  registry <- list(
    statsbomb = build_match_statsbomb,
    wyscout = build_match_wyscout
  )

  enabled <- enabled_providers()
  registry[names(registry) %in% enabled]
}
