#' Layer mapbox
#'
#' Create a mapbox map ggplot layer
#' @name layer_mapbox
#' @inheritParams get_static_map
#'
#' @export
layer_mapbox <- function(area,
                         map_style = mapbox_dark(),
                         mapbox_api_access_token = Sys.getenv("MAPBOX_ACCESS_TOKEN"),
                         retina = TRUE,
                         scale_ratio = 1,
                         area_buffer = 0,
                         mapbox_logo = TRUE,
                         attribution = TRUE,
                         purge_cache = FALSE) {
  static_map <- get_static_map(
    area = area,
    map_style = map_style,
    mapbox_api_access_token = mapbox_api_access_token,
    retina = retina,
    scale_ratio = scale_ratio,
    area_buffer = area_buffer,
    mapbox_logo = mapbox_logo,
    attribution = attribution,
    purge_cache = purge_cache
  )

  ggspatial::layer_spatial(data = static_map)
}
