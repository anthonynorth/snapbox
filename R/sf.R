#' Get mercator bbox
#'
#' @name get_mercator_bbox
#' @param area
#' An sf, sfc or bbox object
#' @param area_buffer
#' Buffer the area by `area_buffer` metres.
#'
#' @noRd
get_mercator_bbox <- function(area, area_buffer = 0) {
  UseMethod("get_mercator_bbox")
}

get_mercator_bbox.sfc <- function(area, area_buffer = 0) {

  area_web_mercator <-
    sf::st_transform(area, 3857)

  if (area_buffer > 0) {
    target_area <-
      area_web_mercator %>%
      sf::st_buffer(area_buffer)
  } else {
    target_area <-
      area_web_mercator
  }

  area_bbox_mercator <- sf::st_bbox(target_area)
  area_bbox_mercator_area <-
    (area_bbox_mercator$xmax - area_bbox_mercator$xmin) *
    (area_bbox_mercator$ymax - area_bbox_mercator$ymin)

  if (is_zero(area_bbox_mercator_area)) {
    stop("Bounding box of supplied geometry has 0 area, so cannot be used as a map viewport. Try setting 'buffer' argument.")
  }

  area_bbox_mercator
}

get_mercator_bbox.sf <- function(area, area_buffer = 0) {
  get_mercator_bbox(sf::st_geometry(area), area_buffer)
}

get_mercator_bbox.bbox <- function(area, area_buffer = 0) {
  get_mercator_bbox(sf::st_as_sfc(area), area_buffer)
}

#' Get aspect ratio
#'
#' @name get_aspect_ratio
#' @param bbox
#' A bbox object
#'
#' @noRd
get_aspect_ratio <- function(bbox) {
  stopifnot(inherits(bbox, "bbox"))

  as.numeric(abs(bbox$xmax - bbox$xmin) / abs(bbox$ymax - bbox$ymin))
}
##' is_zero
##'
##' using all.equal to allow for areas that are very very small to be considered
##' as equal to zero.
##'
##' @name is_zero
##' @param x the number to compare to 0
##' @return {boolean}
##'
##' @nord
is_zero <- function(x) isTRUE(all.equal(0, unname(x)))
