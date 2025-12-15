
test_that("sets options correctly with NULL inputs", {
  withr::local_options(ECHARTS4R_THEME = NULL, ECHARTS4R_FONT_FAMILY = NULL)
  e_common(NULL, NULL)
  expect_null(getOption("ECHARTS4R_THEME"))
  expect_null(getOption("ECHARTS4R_FONT_FAMILY"))
})

test_that("sets options correctly with non-NULL inputs", {
  withr::local_options(ECHARTS4R_THEME = NULL, ECHARTS4R_FONT_FAMILY = NULL)
  e_common("Arial", "dark")
  expect_equal(getOption("ECHARTS4R_THEME"), "dark")
  expect_equal(getOption("ECHARTS4R_FONT_FAMILY"), "Arial")
})

test_that("overwrites existing options", {
  withr::local_options(ECHARTS4R_THEME = "light", ECHARTS4R_FONT_FAMILY = "Helvetica")
  e_common("Times", "vintage")
  expect_equal(getOption("ECHARTS4R_THEME"), "vintage")
  expect_equal(getOption("ECHARTS4R_FONT_FAMILY"), "Times")
})

test_that("returns options list invisibly", {
  withr::local_options(ECHARTS4R_THEME = NULL, ECHARTS4R_FONT_FAMILY = NULL)
  result <- e_common("Arial", "dark")
  expect_type(result, "list")
  expect_named(result, c("ECHARTS4R_THEME", "ECHARTS4R_FONT_FAMILY"))
})
