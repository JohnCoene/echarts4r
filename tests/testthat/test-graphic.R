test_that("expects error when missing e ", {
  expect_error(e_graphic_g(), "must pass e")
  expect_error(e_group_g(), "must pass e")
  expect_error(e_image_g(), "must pass e")

  expect_error(e_text_g(), "must pass e")
  expect_error(e_rect_g(), "must pass e")
  expect_error(e_circle_g(), "must pass e")

  expect_error(e_ring_g(), "must pass e")
  expect_error(e_sector_g(), "must pass e")
  expect_error(e_arc_g(), "must pass e")

  expect_error(e_polygon_g(), "must pass e")
  expect_error(e_polyline_g(), "must pass e")
  expect_error(e_line_g(), "must pass e")

  expect_error(e_bezier_curve_g(), "must pass e")
  expect_error(e_draft(), "must pass e")
})

test_that("e_graphic_g initiates", {
  e <- mtcars |> e_charts(mpg)
  result <- e_graphic_g(e)
  expect_equal(result$x$opts$graphic, list())
})

test_that("e_graphic_g initiates w/ timeline", {
  e <- mtcars |> dplyr::group_by(cyl) |> e_charts(mpg, timeline = TRUE)
  result <- e_graphic_g(e)
  expect_equal(result$x$opts$baseOption$graphic, list())
})

test_that("e_group_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_group_g()
  expect_equal(e$x$opts$graphic$type, "group")
})

test_that("e_image_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_image_g()
  expect_equal(e$x$opts$graphic$type, "image")
})


test_that("e_text_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_text_g()
  expect_equal(e$x$opts$graphic$type, "text")
})

test_that("e_rect_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_rect_g()
  expect_equal(e$x$opts$graphic$type, "rect")
})

test_that("e_circle_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_circle_g()
  expect_equal(e$x$opts$graphic$type, "circle")
})

test_that("e_ring_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_ring_g()
  expect_equal(e$x$opts$graphic$type, "ring")
})

test_that("e_sector_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_sector_g()
  expect_equal(e$x$opts$graphic$type, "sector")
})

test_that("e_arc_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_arc_g()
  expect_equal(e$x$opts$graphic$type, "arc")
})

test_that("e_polygon_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_polygon_g()
  expect_equal(e$x$opts$graphic$type, "polygon")
})

test_that("e_polyline_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_polyline_g()
  expect_equal(e$x$opts$graphic$type, "polyline")
})

test_that("e_line_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_line_g()
  expect_equal(e$x$opts$graphic$type, "line")
})

test_that("e_bezier_curve_g initiates", {
  e <- mtcars |> e_charts(mpg) |> e_bezier_curve_g()
  expect_equal(e$x$opts$graphic$type, "bezierCurve")
})

test_that("e_draft initiates", {
  e <- mtcars |> e_charts(mpg) |> e_draft(text = "DRAFT2", size = "12px", opacity = 0.2, color = "#d3d343")
  expect_equal(e$x$opts$graphic$type, "text")
  expect_equal(e$x$opts$graphic$style$fill, "#d3d343")
  expect_equal(e$x$opts$graphic$style$text, "DRAFT2")
  expect_equal(e$x$opts$graphic$style$opacity, 0.2)
})
