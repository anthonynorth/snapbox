#' Get map zoom
#'
#' @name get_map_zoom
#' @param bbox
#' @param width
#' @param height
#' @param max_zoom
#'
#' @noRd
get_map_zoom <- function(bbox, width, height, max_zoom = 20) {
  points <- sf::st_sfc(
    sf::st_point(c(bbox$xmin, bbox$ymin)),
    sf::st_point(c(bbox$xmax, bbox$ymax)),
    crs = sf::st_crs(bbox)
  ) %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates()

  # get world coordinates
  world_coords <- lng_lat_to_world(points[, 1], points[, 2])

  # width & height in web mercator
  size_x <- abs(diff(world_coords[, 1]))
  size_y <- abs(diff(world_coords[, 2]))

  # pixels per unit
  scale_x <- width / size_x
  scale_y <- height / size_y

  zoom <- log2(abs(min(scale_x, scale_y)))
  # clamp zoom to [0, 20]
  min(max(zoom, 0), 20)
}

#' Lng lat to world
#'
#' Compute web mercator world coordinates
#' @name lng_lat_to_world
#' @param lng
#' @param lat
#' @param tile_size
#' @seealso <https://en.wikipedia.org/wiki/Web_Mercator_projection#Formulas>
#'
#' @noRd
lng_lat_to_world <- function(lng, lat, tile_size = 512) {
  rad <- function(deg) deg * pi / 180

  lambda <- rad(lng)
  phi <- rad(lat)

  x <- (tile_size / (2 * pi)) * (lambda + pi)
  y <- (tile_size / (2 * pi)) * (pi - log(tan(pi / 4 + phi / 2)))

  cbind(x, y)
}

#' Get map centre
#'
#' @name get_map_centre
#' @param bbox
#'
#' @noRd
get_map_centre <- function(bbox) {
  sf::st_as_sfc(bbox) %>%
    sf::st_transform(3857) %>%
    sf::st_centroid() %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates() %>%
    as.vector()
}
