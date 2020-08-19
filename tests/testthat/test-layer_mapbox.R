test_that("layer mapbox works", {

  skip_on_travis()
  skip_on_cran()

  ## test hobart can be fetched
  area <- sf::st_bbox(
    c(xmin = 147, ymin = -43, xmax = 147.7, ymax = -42.65),
    crs = 4326
  )

  ## standard
  hobart_map <-
    ggplot2::ggplot() +
    layer_mapbox(area, scale_ratio = 0.5)


  vdiffr::expect_doppelganger("standard mapbox dark of Hobart", hobart_map)

  ## no logo or attribution
  hobart_map2 <-
    ggplot2::ggplot() +
    layer_mapbox(area, scale_ratio = 0.5, mapbox_logo = FALSE, attribution = FALSE)
  
  vdiffr::expect_doppelganger("mapbox dark with no logo or attribution", hobart_map2)

})
