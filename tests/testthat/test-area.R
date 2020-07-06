test_that("mercator bbox calculation works", {

  brisbane_point <- sf::st_sf(sf::st_sfc(sf::st_point(c(153.03792, -27.414733))),
                          crs = 4326)

  expect_error(get_mercator_bbox(brisbane_point),
               "Bounding box of supplied geometry has 0 area")

  expect_error(get_static_map(brisbane_point),
               "Bounding box of supplied geometry has 0 area")

  brisbane_bbox <- get_mercator_bbox(brisbane_point, area_buffer = 50)
  expect_equal(brisbane_bbox$xmax - brisbane_bbox$xmin,
               100)
  expect_equal(brisbane_bbox$ymax - brisbane_bbox$ymin,
               100)

})
