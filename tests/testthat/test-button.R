
test_that("e_button fails informatively without id", {
  e <- e_charts()
  expect_snapshot(e_button(e), error = TRUE)
})

test_that("e_button initializes button list when id not present", {
  e <- e_charts()
  result <- e_button(e, "myBtn", "Click me")
  expect_true("myBtn" %in% names(result$x$buttons))
})

test_that("e_button does not overwrite existing button id", {
  e <- e_charts()
  result <- e_button(e, "myBtn", "Click me")
  result2 <- e_button(result, "foo", "Click me")
  # It makes 2 buttons!
  expect_equal(length(result2$x$buttons), 2)
})

test_that("e_button prepends content when position is top", {
  e <- e_charts()
  testthat::local_mocked_bindings(
    prependContent = function(e, tag) {
      e$prepended <- TRUE
      e
    },
    .package = "htmlwidgets"
  )
  result <- e_button(e, "myBtn", "Click", position = "top")
  expect_true(result$prepended)
})

test_that("e_button appends content when position is not top", {
  e <- e_charts()
  testthat::local_mocked_bindings(
    appendContent = function(e, tag) {
      e$appended <- TRUE
      e
    },
    .package = "htmlwidgets"
  )
  result <- e_button(e, "myBtn", "Click", position = "bottom")
  expect_true(result$appended)
})

test_that("e_button uses custom tag function", {
  e <- e_charts()
  custom_tag <- function(...) list(tag = "custom", ...)
  testthat::local_mocked_bindings(
    prependContent = function(e, tag) {
      e$tag_used <- tag
      e
    },
    .package = "htmlwidgets"
  )
  result <- e_button(e, "myBtn", "Click", tag = custom_tag)
  expect_equal(result$tag_used$tag, "custom")
})
