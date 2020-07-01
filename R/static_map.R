#' Get static map
#'
#' @name get_static_map
#'
#' @param area
#' @param map_style
#' @param mapbox_api_access_token
#' @param retina
#' @param scale_ratio
#'
#' @export
get_static_map <- function(area,
                           map_style = "mapbox://styles/mapbox/dark-v10",
                           mapbox_api_access_token = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
                           retina = TRUE,
                           scale_ratio = 1) {
  stopifnot(inherits(area, c("sf", "sfc")))
  style <- sub("mapbox://styles/", "", map_style)
  max_dim <- min(1280, round(1280 * scale_ratio))

  mercator_bbox <- get_mercator_bbox(area)
  aspect_ratio <- get_aspect_ratio(mercator_bbox)

  width <- min(max_dim, round(max_dim * aspect_ratio))
  height <- min(max_dim, round(max_dim / aspect_ratio))

  overlay <- mercator_bbox %>%
    sf::st_as_sfc() %>%
    sf::st_sf() %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(
      `fill-opacity` = 0L,
      `stroke-width` = 0L
    )

  url <- paste(
    sep = "/",
    "https://api.mapbox.com/styles/v1",
    style,
    "static",
    paste0("geojson(", geojsonsf::sf_geojson(overlay), ")"),
    "auto",
    paste0(width, "x", height, ifelse(retina, "@2x", "")),
    paste0("?access_token=", mapbox_api_access_token)
  )

  response <- curl::curl_fetch_memory(url)
  if(response$status_code != 200) stop("The remote server returned status code ", response$status_code, " in response to the image request.")

  img <- png::readPNG(response$content) * 255

  tile <- raster::brick(img) %>%
    raster::setExtent(raster::extent(
      mercator_bbox$xmin,
      mercator_bbox$xmax,
      mercator_bbox$ymin,
      mercator_bbox$ymax
    ))

  raster::crs(tile) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

  tile
}

get_mercator_bbox <- function(area) {
  stopifnot(inherits(area, c("sf", "sfc")))
  area %>%
    sf::st_transform(3857) %>%
    sf::st_bbox()
}

get_aspect_ratio <- function(bbox) {
  stopifnot(inherits(bbox, "bbox"))

  as.numeric(abs(bbox$xmax - bbox$xmin) / abs(bbox$ymax - bbox$ymin))
}
