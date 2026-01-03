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
