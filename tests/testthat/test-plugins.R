
# cloud -------------------------------------------------------------------


# liquid ------------------------------------------------------------------


# modularity --------------------------------------------------------------


# doughnut ----------------------------------------------------------------


# violin ------------------------------------------------------------------


# barrange ----------------------------------------------------------------

test_that("e_barRange works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> e_chart(Species) |>
    e_barRange(lower=min_length,
               upper=max_length,
               textSymbol = 'NEWSYMBOL'
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # text symbol has been applied.
  expect_equal(plot$x$opts$series[[1]]$itemPayload$textSymbol, "NEWSYMBOL")

  expect_equal(
    plot$x$opts$series[[1]]$data[[1]]$value,
    c("setosa", "4.3", "5.8")
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "custom"
  )
})

test_that("e_barRange x and y index works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> e_chart(Species) |>
    # TODO how to know this works?
    e_barRange(lower=min_length,
               upper=max_length,
               y_index = 10,
               x_index = 10
    )
})

test_that("e_barRange timeline works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  plot <- df |> dplyr::group_by(Species) |> e_chart(Species, timeline = TRUE) |>
    e_barRange(lower=min_length,
               upper=max_length
    ) |> e_timeline_serie(
          title = list(
            list(text = "setosa"),
            list(text = "versicolor"),
            list(text = "virginica")
          )
        )
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("setosa", "versicolor", "virginica")
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "custom")
})


# contour -----------------------------------------------------------------



