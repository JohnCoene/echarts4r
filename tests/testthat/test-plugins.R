test_that("e_barRange works", {
  df <- iris |>
    dplyr::group_by(Species) |>
    dplyr::summarise(min_length = min(Sepal.Length),
                     max_length = max(Sepal.Length))

  e <- df |> e_chart(Species) |>
    e_barRange(min="min_length",
               max="max_length",
               textSymbol = 'NEWSYMBOL'
    )

  # text symbol has been applied.
  expect_equal(e$x$opts$series[[1]]$itemPayload$textSymbol, "NEWSYMBOL")
})
