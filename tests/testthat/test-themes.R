
# e_theme_custom ----------------------------------------------------------
test_that("e_theme_custom errors informatively without theme", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  expect_error(e_theme_custom(e), "must pass `theme`")
})

test_that("e_theme_custom sets theme properties", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  theme_json <- '{"color":["#ff715e","#ffaf51"]}'

  result <- e_theme_custom(e, theme_json)

  expect_true(result$x$theme2)
  expect_equal(result$x$customTheme, theme_json)
  expect_equal(result$x$theme, "custom")
  expect_equal(result$x$theme_name, "custom")
})

test_that("e_theme_custom accepts custom name", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  theme_json <- '{"color":["#ff715e","#ffaf51"]}'

  result <- e_theme_custom(e, theme_json, name = "mytheme")

  expect_equal(result$x$theme_name, "mytheme")
})

test_that("e_theme_custom reads from JSON file", {
  temp_file <- tempfile(fileext = ".json")
  writeLines('{"color":["#ff715e","#ffaf51"]}', temp_file)

  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  result <- e_theme_custom(e, temp_file)

  expect_equal(result$x$customTheme, '{"color":["#ff715e","#ffaf51"]}')

  unlink(temp_file)
})


# e_theme_register --------------------------------------------------------

test_that("e_theme_register errors informatively without theme", {
  expect_error(e_theme_register(), "must pass `theme`")
})

test_that("e_theme_register returns shiny singleton", {
  theme_json <- '{"color":["#ff715e","#ffaf51"]}'

  result <- e_theme_register(theme_json)

  expect_s3_class(result, "shiny.tag")
  expect_true(shiny::is.singleton(result))

})

test_that("e_theme_register accepts custom name", {
  theme_json <- '{"color":["#ff715e","#ffaf51"]}'

  result <- e_theme_register(theme_json, name = "mytheme")

  script_content <- as.character(result$children[[1]])
  expect_true(grepl("mytheme", script_content))
})

test_that("e_theme_register reads from JSON file", {
  temp_file <- tempfile(fileext = ".json")
  writeLines('{"color":["#ff715e","#ffaf51"]}', temp_file)

  result <- e_theme_register(temp_file)

  script_content <- as.character(result$children[[1]])
  expect_true(grepl("ff715e", script_content))

  unlink(temp_file)
})

test_that("e_theme_register generates correct JavaScript", {
  theme_json <- '{"color":["#ff715e"]}'

  result <- e_theme_register(theme_json, name = "testtheme")

  script_content <- as.character(result$children[[1]])
  expect_true(grepl("window.echarts.registerTheme", script_content))
  expect_true(grepl("testtheme", script_content))
  expect_true(grepl("ff715e", script_content))
})
