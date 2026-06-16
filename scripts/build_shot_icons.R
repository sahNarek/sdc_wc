#!/usr/bin/env Rscript
# Rebuild shot-map body-part icons from the reference footprint image.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg)), ".."), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("rsvg", quietly = TRUE)) {
  install.packages("rsvg", repos = "https://cloud.r-project.org")
}

library(magick)

out <- file.path(root, "assets", "icons")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

ref <- file.path(
  root,
  ".cursor",
  "projects",
  "Users-nareksahakyan-workspace-sdc-wc",
  "assets",
  "image-a4982f43-f2e0-4575-a9f1-7b17dc523713.png"
)

if (!file.exists(ref)) {
  ref <- Sys.glob(file.path(root, "**/image-a4982f43*.png"))[1]
}

if (is.na(ref) || !nzchar(ref)) {
  stop("Reference footprint image not found.", call. = FALSE)
}

im <- image_read(ref)
w <- image_info(im)$width
h <- image_info(im)$height
solid <- image_crop(im, geometry_area(w / 2, h, w / 2, 0))
sw <- image_info(solid)$width

process_foot <- function(img) {
  img <- image_trim(img)
  img <- image_transparent(img, "white", fuzz = 15)
  img <- image_extent(img, geometry_area(100, 220), color = "none", gravity = "center")
  image_convert(img, "png")
}

left <- process_foot(image_crop(solid, geometry_area(sw / 2, h, 0, 0)))
right <- process_foot(image_crop(solid, geometry_area(sw / 2, h, sw / 2, 0)))
image_write(left, file.path(out, "left_foot.png"))
image_write(right, file.path(out, "right_foot.png"))

rsvg::rsvg_png(
  file.path(out, "head.svg"),
  file.path(out, "head.png"),
  width = 140,
  height = 160
)

message("Icons written to ", out)
