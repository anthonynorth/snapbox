#' memoised curl fetch
#'
#' Hack to make R CMD check happy since it doens't see curl_fetch_memory being used.
#' @importFrom curl curl_fetch_memory
#' @noRd
api_query <- memoise::memoise(curl::curl_fetch_memory)

#' warn_once
#' @noRd
warn_once <- memoise::memoise(warning)

#' Get map image
#'
#' @name get_map_image
#' @param bbox bbox of the map
#' @param width output width
#' @param height output height
#' @inheritParams get_static_map
#'
#' @noRd
get_map_image <- function(bbox,
                          map_style,
                          width,
                          height,
                          retina,
                          mapbox_logo,
                          attribution,
                          mapbox_api_access_token,
                          purge_cache) {
  if (!attribution) {
    warn_once(
      "You have a legal responsibility to attribute maps that use OpenStreetMap data, which includes most maps from Mapbox.
    If you specify attribution = FALSE, you are legally required to include proper attribution elsewhere on the webpage or document.
    See <https://docs.mapbox.com/help/how-mapbox-works/attribution/#static--print>"
    )
  }

  zoom <- get_map_zoom(bbox, width, height)
  centre <- get_map_centre(bbox)

  url <- get_request_url(
    map_style = map_style,
    centre = centre,
    zoom = zoom,
    width = width,
    height = height,
    retina = retina,
    mapbox_logo = mapbox_logo,
    attribution = attribution,
    mapbox_api_access_token = mapbox_api_access_token
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
#' @param centre map centre point
#' @param zoom map zoom level
#' @inheritParams get_map_image
#'
#' @noRd
get_request_url <- function(map_style,
                            centre,
                            zoom,
                            width,
                            height,
                            retina,
                            mapbox_logo,
                            attribution,
                            mapbox_api_access_token) {
  path <- paste(
    sep = "/",
    "https://api.mapbox.com/styles/v1",
    sub("mapbox://styles/", "", map_style),
    "static",
    paste(sep = ",", centre[1], centre[2], zoom),
    paste0(width, "x", height, ifelse(retina, "@2x", ""))
  )

  query_string <- paste(
    sep = "&",
    paste0("logo=", tolower(mapbox_logo)),
    paste0("attribution=", tolower(attribution)),
    paste0("access_token=", mapbox_api_access_token)
  )

  paste(sep = "?", path, query_string)
}
