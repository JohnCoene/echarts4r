### Tests of the functions in the tab "Maps"
### https://echarts4r.john-coene.com/articles/map.html

test_that("e_map plot has the good data structure and type", {
  set.seed(1)
  cns <- countrycode::codelist$country.name.en[1:5]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6)
  )

  plot <- cns |>
    e_charts(country) |>
    e_map(value) |>
    e_visual_map(value)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(2.062035), name = "Afghanistan"),
      list(value = c(2.488496), name = "Albania"),
      list(value = c(3.291413), name = "Algeria"),
      list(value = c(4.632831), name = "American Samoa"),
      list(value = c(1.806728), name = "Andorra")
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "map"
  )
})

test_that("e_lines plot has the good data structure and type", {
  flights <- flights[1:5,]

  plot <- flights |>
    e_charts() |>
    e_geo() |>
    e_lines(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      name = "flights",
      lineStyle = list(normal = list(curveness = 0.3))
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # test that difference is near 0 because decimals are problematic
  difference <-
    unlist(plot$x$opts$series[[1]]$data) - unlist(list(
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-106.60919, 35.04022)
      )),
      list(coords = list(
        c(-87.90446, 41.97960),
        c(-97.66987, 30.19453)
      )),
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-72.68323, 41.93887)
      )),
      list(coords = list(
        c(-66.00183, 18.43942),
        c(-72.68323, 41.93887)
      )),
      list(coords = list(
        c(-97.03720, 32.89595),
        c(-86.75355, 33.56294)
      ))
    ))
  difference_test <- difference < 10^-5

  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "lines"
  )
})

test_that("e_lines plot with source, target, value", {
  flights <- flights[1:5,]

  plot <- flights |>
    e_charts() |>
    e_geo() |>
    e_lines(
      start_lon,
      start_lat,
      end_lon,
      end_lat,
      source_name = airport1,
      target_name = airport2,
      value = cnt,
      name = "flights",
      lineStyle = list(normal = list(curveness = 0.3))
    )
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  first_result <- plot$x$opts$series[[1]]$data[[1]]

  expect_equal(first_result$source_name, flights[1, "airport1"])
  expect_equal(first_result$target_name, flights[1, "airport2"])
  expect_equal(first_result$value, flights[1, "cnt"])
})

test_that("e_lines.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      plot <- flights |>
        e_charts() |>
        e_geo() |>
        e_lines(
          start_lon,
          start_lat,
          end_lon,
          end_lat,
          name = "flights",
          effect = list(show = TRUE)
        )
    })

    observeEvent(input$update, {

      chart <- echarts4rProxy("line",
                              data = flights) |>
        e_lines(
          end_lon,
          end_lat,
          start_lon,
          start_lat,
          effect = list(show = TRUE)
        ) |>
        e_execute()
      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())

    # These were turned to lat in the proxy
    new_start_lon_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$coords[[1]][1]) |> unlist()

    expect_identical(
      new_start_lon_values, (flights[["end_lon"]])
    )

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "lines"
    )

    expect_error(echarts4rProxy("line", data = flights) |>
                   e_lines(), "missing coordinates")
  })
})

test_that("e_lines.echarts4r and e_lines_ expects error when missing e and coordinates", {
  expect_error(iris |> e_charts() |> e_lines.echarts4r(), "missing coordinates")
  expect_error(e_lines.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_lines_(), "missing coordinates")
  expect_error(e_lines_() , "must pass e")
})


### Either need to put echarts4r.maps in imports, or to find another way without importing it

# test_that("em_map plot has the good data structure and type", {
#   library(echarts4r.maps)
#
#   df <- data.frame(
#     region = c("Rajasthan", "Odisha", "Gujarat"),
#     value = c(1,2, 3)
#   )
#
#   df |>
#     e_charts(region) |>
#     em_map("India") |>
#     e_map(value, map = "India") |>
#     e_visual_map(value) |>
#     e_theme("infographic")
#
#   expect_s3_class(plot, "echarts4r")
#   expect_s3_class(plot, "htmlwidget")
#
#   expect_equal(
#     plot$x$opts$series[[1]]$data,
#     list(list(value = c(2.062035), name = "Afghanistan"),
#          list(value = c(2.488496), name = "Åland Islands"),
#          list(value = c(3.291413), name = "Albania"),
#          list(value = c(4.632831), name = "Algeria"),
#          list(value = c(1.806728), name = "American Samoa"))
#   )
#   expect_equal(
#     plot$x$opts$series[[1]]$type,
#     "map"
#   )
# })


test_that("geojson support is functional", {
  json <- jsonlite::read_json("https://raw.githubusercontent.com/shawnbot/topogram/master/data/us-states.geojson")

  plot <- USArrests |>
    tibble::rownames_to_column("states") |>
    e_charts(states) |>
    e_map_register("USA", json) |>
    e_map(Murder, map = "USA") |>
    e_visual_map(Murder)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # different test: it's enough to check if data is equal for two random states to confirm that geojson is supported
  expect_equal(
    plot$x$opts$series[[1]]$data[[2]],
    list(value = 10, name = "Alaska")
  )
  expect_equal(
    plot$x$opts$series[[1]]$data[[22]],
    list(value = 12.1, name = "Michigan")
  )

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "map"
  )
})
