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
  buffered_area <- area %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(area_buffer)

  if (as.numeric(sf::st_area(buffered_area)) == 0) {
    stop("Bounding box of supplied geometry has 0 area, so cannot be used as a map viewport. Try setting 'buffer' argument.")
  }

  sf::st_bbox(buffered_area)
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
