#' memoised curl fetch
#' @noRd
api_query <- memoise::memoise(curl::curl_fetch_memory)

#' Get map image
#'
#' @name get_map_image
#' @param bbox
#' @param map_style
#' @param width
#' @param height
#' @param retina
#' @param mapbox_api_access_token
#' @param purge_cache
#'
#' @noRd
get_map_image <- function(bbox,
                          map_style = mapbox_dark(),
                          width,
                          height,
                          retina = TRUE,
                          mapbox_api_access_token = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
                          purge_cache = FALSE) {
  zoom <- get_map_zoom(bbox, width, height)
  centre <- get_map_centre(bbox)
  url <- get_request_url(
    map_style,
    centre,
    zoom,
    width,
    height,
    retina,
    mapbox_api_access_token
  )

  if (purge_cache) forget(api_query) # reset memoisation
  response <- api_query(url)
  if (response$status_code != 200) stop("The remote server returned status code ", response$status_code, " in response to the image request.")

  png::readPNG(response$content)
}

#' Get request url
#'
#' @name get_request_url
#' @param map_style
#' @param centre
#' @param zoom
#' @param width
#' @param height
#' @param retina
#' @param mapbox_api_access_token
#'
#' @noRd
get_request_url <- function(map_style,
                            centre,
                            zoom,
                            width,
                            height,
                            retina,
                            mapbox_api_access_token) {
  paste(
    sep = "/",
    "https://api.mapbox.com/styles/v1",
    sub("mapbox://styles/", "", map_style),
    "static",
    paste(sep = ",", centre[1], centre[2], zoom, 0),
    paste0(width, "x", height, ifelse(retina, "@2x", "")),
    paste0("?access_token=", mapbox_api_access_token)
  )
}