test_that("mercator bbox calculation works", {

  brisbane_point <- sf::st_sf(
    sf::st_sfc(sf::st_point(c(153.03792, -27.414733))),
    crs = 4326
  )

  ## single points should fail
  expect_error(get_mercator_bbox(brisbane_point),
               "Bounding box of supplied geometry has 0 area")
  expect_error(get_static_map(brisbane_point),
               "Bounding box of supplied geometry has 0 area")

  brisbane_bbox <- get_mercator_bbox(brisbane_point, area_buffer = 50)
  expect_equal(
    as.numeric(brisbane_bbox$xmax - brisbane_bbox$xmin),
    100
  )
  expect_equal(
    as.numeric(brisbane_bbox$ymax - brisbane_bbox$ymin),
    100
  )

  ## many points should work
  brisbane_2point <- sf::st_sf(
                           sf::st_sfc(list(sf::st_point(c(153.03792, -27.414733)),
                                      sf::st_point(c(153.035, -27.415)))),
                          crs = 4326
                        )
  expect_equal(
    as.numeric(get_mercator_bbox(brisbane_2point)),
    c(17035778.2735486, -3175416.88179225, 17036103.3264617, -3175383.39926467)
  )
 

})
