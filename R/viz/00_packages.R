required_packages <- c(
  "tidyverse",
  "jsonlite",
  "yaml",
  "scales",
  "grid",
  "showtext",
  "sysfonts",
  "ggimage",
  "rsvg",
  "patchwork",
  "magick"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
  invisible(lapply(missing_packages, library, character.only = TRUE))
}

library(tidyverse)
library(jsonlite)
library(yaml)
library(scales)
library(grid)
library(showtext)
library(sysfonts)

register_sdc_fonts <- function() {
  if (!requireNamespace("sysfonts", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  tryCatch(
    {
      sysfonts::font_add_google("Barlow Condensed", "barlow", regular.wt = 700)
      sysfonts::font_add_google("Open Sans", "opensans")
      showtext::showtext_auto(enable = TRUE)
      TRUE
    },
    error = function(e) {
      message("Could not load Google fonts; using system fallbacks.")
      FALSE
    }
  )
}

invisible(register_sdc_fonts())
