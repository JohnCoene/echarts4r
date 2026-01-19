
test_that("e_leaflet sets roam option", {
  e <- mtcars |> e_charts(mpg)
  result <- e_leaflet(e, roam = FALSE)
  expect_equal(result$x$opts$leaflet$roam, FALSE)
})

test_that("e_leaflet initializes empty tiles list", {
  e <- mtcars |> e_charts(mpg)
  result <- e_leaflet(e)
  expect_equal(result$x$opts$leaflet$tiles, list())
})

test_that("e_leaflet removes x and y axis", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_leaflet(e)
  expect_null(result$x$opts$xAxis)
  expect_null(result$x$opts$yAxis)
})

test_that("e_leaflet adds leaflet dependencies", {
  e <- mtcars |> e_charts(mpg)
  result <- e_leaflet(e)
  dep_names <- sapply(result$dependencies, function(x) x$name)
  expect_true("leaflet" %in% dep_names)
  expect_true("echarts-leaflet" %in% dep_names)
})

test_that("e_leaflet accepts additional options", {
  e <- mtcars |> e_charts(mpg)
  result <- e_leaflet(e, center = c(0, 0), zoom = 10)
  expect_equal(result$x$opts$leaflet$center, c(0, 0))
  expect_equal(result$x$opts$leaflet$zoom, 10)
})


# e_leaflet_tile ----------------------------------------------------------
test_that("e_leaflet_tile errors informatively without leaflet package", {
  testthat::local_mocked_bindings(
    installed.packages = function() {
      matrix(c("echarts4r", "dplyr"), ncol = 1, dimnames = list(NULL, "Package"))
    },
    .package = "utils"
  )
  e <- mtcars |> e_charts(mpg)
  expect_snapshot(e_leaflet_tile(e), error = TRUE)
})

test_that("e_leaflet_tile calls e_leaflet when roam not set", {
  e <- population |>
    dplyr::filter(value > 8) |>
    e_charts(lon)
  result <- e_leaflet_tile(e)
  expect_true(length(result$x$opts$leaflet$roam) > 0)
})

test_that("e_leaflet_tile does not call e_leaflet when roam already set", {
  e <- mtcars |> e_charts(mpg, roam = FALSE) |> e_leaflet(roam = FALSE)
  result <- e_leaflet_tile(e)
  expect_equal(result$x$opts$leaflet$roam, FALSE)
})

test_that("e_leaflet_tile sets urlTemplate", {
  e <- mtcars |> e_charts(mpg) |> e_leaflet()
  result <- e_leaflet_tile(e)
  expect_equal(result$x$opts$leaflet$tiles[[1]]$urlTemplate, "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png")
})

test_that("e_leaflet_tile accepts custom template", {
  e <- mtcars |> e_charts(mpg) |> e_leaflet()
  custom_template <- "https://example.com/{z}/{x}/{y}.png"
  result <- e_leaflet_tile(e, template = custom_template)
  expect_equal(result$x$opts$leaflet$tiles[[1]]$urlTemplate, custom_template)
})

test_that("e_leaflet_tile sets options when provided", {
  e <- mtcars |> e_charts(mpg) |> e_leaflet()
  opts <- list(attribution = "Test", label = "Test Label")
  result <- e_leaflet_tile(e, options = opts)
  expect_equal(result$x$opts$leaflet$tiles[[1]]$options, opts)
})

test_that("e_leaflet_tile does not set options when NULL", {
  e <- mtcars |> e_charts(mpg) |> e_leaflet()
  result <- e_leaflet_tile(e, options = NULL)
  expect_null(result$x$opts$leaflet$tiles[[1]]$options)
})

test_that("e_leaflet_tile accepts additional arguments", {
  e <- population |>
    dplyr::filter(value > 8) |>
    e_charts(lon) |> e_leaflet()
  result <- e_leaflet_tile(e, options = list(maxZoom = 3, minZoom = 1))
  expect_equal(result$x$opts$leaflet$tiles[[1]]$options$maxZoom, 3)
  expect_equal(result$x$opts$leaflet$tiles[[1]]$options$minZoom, 1)
})
