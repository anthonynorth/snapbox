#' memoised curl fetch
#' @noRd
api_query <- memoise::memoise(curl::curl_fetch_memory)

#' Get map image
#'
#' @name get_map_image
#' @param bbox bbox of the map
#' @param map_style mapbox style, see `stylebox::mapbox_styles`
#' @param width output width
#' @param height output height
#' @param retina render the map at 2x scale if `TRUE`
#' @param mapbox_api_access_token mapbox api access token
#' @param purge_cache forget cached api calls and responses before making api call if `TRUE`.
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

  if (purge_cache) memoise::forget(api_query) # reset memoisation
  response <- api_query(url)
  if (response$status_code != 200) stop("The remote server returned status code ", response$status_code, " in response to the image request.")

  switch(
    response$type,
    "image/png" = png::readPNG(response$content),
    "image/jpeg" = jpeg::readJPEG(response$content),
    stop("Unknown content type", response$type)
  )
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
