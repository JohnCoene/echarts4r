test_that("returns data as-is when not grouped", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- map_grps_(df)

  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(result[[1]], df)
  # Row names were removed so should be just the index
  expect_equal(row.names(result[[1]]) |> as.integer(), 1:3)
})

test_that("splits grouped_df into list by group", {
  df <- data.frame(x = 1:6, y = 7:12, grp = rep(c("A", "B"), each = 3))
  grouped <- dplyr::group_by(df, grp)

  result <- map_grps_(grouped)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("A", "B"))
  expect_equal(nrow(result[[1]]), 3)
  expect_equal(nrow(result[[2]]), 3)
  # Row names were removed so should be just the index
  expect_equal(row.names(result[[1]]) |> as.integer(), 1:3)
  expect_equal(row.names(result[[2]]) |> as.integer(), 1:3)
})

test_that("splits grouped_tt into list by group", {
  df <- data.frame(x = 1:6, y = 7:12, grp = c("A", "A", "A", "B", "B", "B"))
  attr(df, "groups") <- "grp"
  class(df) <- c("grouped_tt", "data.frame")

  result <- map_grps_(df)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("A", "B"))
  expect_equal(nrow(result[[1]]), 3)
  expect_equal(nrow(result[[2]]), 3)
  # Row names were removed so should be just the index
  expect_equal(row.names(result[[1]]) |> as.integer(), 1:3)
  expect_equal(row.names(result[[2]]) |> as.integer(), 1:3)
})

test_that("removes row names from all output data frames", {
  df <- data.frame(x = 1:3, y = 4:6)
  row.names(df) <- c("a", "b", "c")

  result <- map_grps_(df)
  expect_type(result, "list")
  # Row names were removed so should be just the index
  expect_equal(row.names(result[[1]]) |> as.integer(), 1:3)
})

test_that("forces data.frame conversion for data.frame input", {
  tbl <- tibble::tibble(x = 1:3, y = 4:6)

  result <- map_grps_(tbl)

  expect_s3_class(result[[1]], "data.frame")
  expect_false(inherits(result[[1]], "tbl_df"))
})
