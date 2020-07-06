#' Get static Mapbox map as a georeferenced raster
#'
#' @name get_static_map
#'
#' @param area 
#' @param map_style 
#' @param mapbox_api_access_token 
#' @param retina 
#' @param scale_ratio 
#' @param area_buffer a buffer to appear around the `area` geometry in meters (web mercator projection, EPSG 3857). Use this to create space around your data in the map visual.
#' @param purge_cache forget cached api calls and responses before making api call if TRUE.
#' @export
get_static_map <- function(area,
                           map_style = "mapbox://styles/mapbox/dark-v10",
                           mapbox_api_access_token = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
                           retina = TRUE,
                           scale_ratio = 1,
                           area_buffer = 0,
                           purge_cache = FALSE) {
  stopifnot(inherits(area, c("sf", "sfc")))

  max_dim <- min(1280, round(1280 * scale_ratio))

  mercator_bbox <- get_mercator_bbox(area, area_buffer)
  aspect_ratio <- get_aspect_ratio(mercator_bbox)

  width <- min(max_dim, round(max_dim * aspect_ratio))
  height <- min(max_dim, round(max_dim / aspect_ratio))

  map_img <- get_map_image(mercator_bbox,
                           map_style,
                           width, height,
                           retina,
                           mapbox_api_access_token,
                           purge_cache)

  tile <- raster::brick(map_img) %>%
    raster::setExtent(raster::extent(
      mercator_bbox$xmin,
      mercator_bbox$xmax,
      mercator_bbox$ymin,
      mercator_bbox$ymax
    ))

  raster::crs(tile) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

  tile
}

get_map_overlay <- function(bbox) {
  stopifnot(inherits(bbox, "bbox"))
  bbox %>%
    sf::st_as_sfc() %>%
    sf::st_sf() %>%
    sf::st_transform(4326) %>%
    within(
      {
        `fill-opacity` <- 0L
        `stroke-width` <- 0L
      }
    )
}

get_map_image <- function(bbox,
                          map_style = "mapbox://styles/mapbox/dark-v10",
                          width,
                          height,
                          retina = TRUE,
                          mapbox_api_access_token = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
                          purge_cache = FALSE) {
  overlay <- get_map_overlay(bbox)
  url <- get_request_url(map_style, overlay, width, height, retina, mapbox_api_access_token)

  if(purge_cache) forget(api_query) ## reset memoisation
  response <- api_query(url)
  if(response$status_code != 200) stop("The remote server returned status code ", response$status_code, " in response to the image request.")

  png::readPNG(response$content)
}

api_query <- memoise::memoise(curl::curl_fetch_memory)

get_request_url <- function(map_style, overlay, width, height, retina, mapbox_api_access_token) {
  paste(
    sep = "/",
    "https://api.mapbox.com/styles/v1",
    sub("mapbox://styles/", "", map_style),
    "static",
    paste0("geojson(", geojsonsf::sf_geojson(overlay), ")"),
    "auto",
    paste0(width, "x", height, ifelse(retina, "@2x", "")),
    paste0("?access_token=", mapbox_api_access_token)
  )
}

get_mercator_bbox <- function(area, area_buffer = 0) {
  stopifnot(inherits(area, c("sf", "sfc")))

 buffered_area <- 
   area %>%
   sf::st_transform(3857) %>%
   sf::st_buffer(area_buffer)

 if(as.numeric(sf::st_area(buffered_area)) == 0) stop("Bounding box of supplied geometry has 0 area, so cannot be used as a map viewport. Try setting 'buffer' argument.")

  sf::st_bbox(buffered_area)

}

get_aspect_ratio <- function(bbox) {
  stopifnot(inherits(bbox, "bbox"))

  as.numeric(abs(bbox$xmax - bbox$xmin) / abs(bbox$ymax - bbox$ymin))
}
