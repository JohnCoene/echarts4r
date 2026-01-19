### Tests of the functions in the tab "Maps"
### https://echarts4r.john-coene.com/articles/map.html
test_that("errors informatively without e", {
  expect_error(e_map(), "must pass e")
  expect_error(e_map_(), "must pass e")
  expect_error(e_map_3d(), "must pass e")
  expect_error(e_map_3d_(), "must pass e")
  expect_error(e_map_3d_custom(), "must pass e")
  expect_error(e_charts() |> e_map_3d_custom(), "must pass id, value, and height")
  expect_error(e_charts() |> e_mapbox(), "missing token")
})


# e_map -------------------------------------------------------------------
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

test_that("e_map_ plot has the good data structure and type", {
  set.seed(1)
  cns <- countrycode::codelist$country.name.en[1:5]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6)
  )

  plot <- cns |>
    e_charts(country) |>
    e_map_("value") |>
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

test_that("e_map_ timeline works", {

  cns <- countrycode::codelist$country.name.en[116:130]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6),
    group = LETTERS[1:5]
  )
  plot <- cns |> dplyr::group_by(group) |> e_chart(country, timeline = TRUE) |>  e_map_("value", name = "ss")

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    unlist(plot$x$opts$baseOption$timeline$data),
    LETTERS[1:5]
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "map")
})
# e_map_3d_ ---------------------------------------------------------------
test_that("e_map_3d plot has the good data structure and type", {
  set.seed(1)
  cns <- countrycode::codelist$country.name.en[1:5]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6)
  )

  plot <- cns |>
    e_charts(country) |>
    e_map_3d(value) |>
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
    "map3D"
  )
})

test_that("e_map_3d_ plot has the good data structure and type", {
  set.seed(1)
  cns <- countrycode::codelist$country.name.en[1:5]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6)
  )

  plot <- cns |>
    e_charts(country) |>
    e_map_3d_("value") |>
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
    "map3D"
  )
})

test_that("e_map_3d_ timeline works", {

  cns <- countrycode::codelist$country.name.en[116:130]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6),
    group = LETTERS[1:5]
  )
  plot <- cns |> dplyr::group_by(group) |> e_chart(country, timeline = TRUE) |>  e_map_3d_("value", name = "ss")

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    unlist(plot$x$opts$baseOption$timeline$data),
    LETTERS[1:5]
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "map3D")
})


# e_map_3d_custom ---------------------------------------------------------
test_that("e_map_3d_custom errors informatively without registered map", {
  e <- data.frame(id = c("a", "b"), value = c(1, 2), height = c(10, 20)) |>
    e_charts()
  expect_snapshot(e_map_3d_custom(e, id, value, height), error = TRUE)
})

test_that("e_map_3d_custom uses registered map when map is NULL", {
  e <- echarts4r::buildings_sample |>
    e_charts() |>
    e_map_register("buildings_sample_json", echarts4r::buildings_sample_json) |>
    e_map_3d_custom(name, value, height) |>
    e_visual_map(
      show = FALSE,
      min = 0.4,
      max = 1
    )
  expect_equal(e$x$opts$series[[1]]$map, "buildings_sample_json")

  # renders with webgl
  expect_equal(e$x$mainOpts$renderer, "webgl")

  # removes axes when rm_x and rm_y are TRUE
  expect_null(e$x$opts$xAxis)
  expect_null(e$x$opts$yAxis)

  expect_equal(e$x$opts$series[[1]]$type, "map3D")
})

test_that("e_map_3d_custom sets series name when provided", {
  e <- echarts4r::buildings_sample |>
    e_charts() |>
    e_map_register("buildings_sample_json", echarts4r::buildings_sample_json) |>
    e_map_3d_custom(name, value, height, name = "customName") |>
    e_visual_map(
      show = FALSE,
      min = 0.4,
      max = 1
    )
  expect_equal(e$x$opts$series[[1]]$name, "customName")
})

test_that("e_map_3d_custom builds data correctly", {
  df <- data.frame(id = c("a", "b"), value = c(1, 2), height = c(10, 20))
  e <- df |>
    e_charts() |>
    e_map_register("buildings_sample_json", echarts4r::buildings_sample_json) |>
    e_map_3d_custom(id, value, height, name = "customName")
  e$x$registerMap <- list(list(mapName = "testMap"))
  expect_equal(e$x$opts$series[[1]]$data[[1]]$name, "a")
  expect_equal(e$x$opts$series[[1]]$data[[1]]$value, '1')
  expect_equal(e$x$opts$series[[1]]$data[[1]]$height, '10')
})

test_that("e_map_3d_custom adds echarts-gl dependency", {
  df <- data.frame(id = c("a", "b"), value = c(1, 2), height = c(10, 20))
  e <- df |>
    e_charts() |>
    e_map_register("buildings_sample_json", echarts4r::buildings_sample_json) |>
    e_map_3d_custom(id, value, height)
  dep_names <- sapply(e$dependencies, function(x) x$name)
  expect_true("echarts-gl" %in% dep_names)
})

test_that("e_map_3d_custom passes additional arguments", {
  df <- data.frame(id = c("a", "b"), value = c(1, 2), height = c(10, 20))
  e <- df |>
    e_charts() |>
    e_map_register("buildings_sample_json", echarts4r::buildings_sample_json) |>
    e_map_3d_custom(id, value, height, shading = "lambert")
  e
  # result <- e_map_3d_custom(e, id, value, height, ")
  expect_equal(e$x$opts$series[[1]]$shading, "lambert")
})

# e_svg_register ----------------------------------------------------------
test_that("e_svg_register.echarts4r initializes registerMap when empty", {
  e <- mtcars |> e_charts(mpg)
  e$x$registerMap <- NULL
  result <- e_svg_register(e, "testSvg", "<svg></svg>")
  expect_equal(length(result$x$registerMap), 1)
})

test_that("e_svg_register.echarts4r registers svg with correct structure", {
  e <- mtcars |> e_charts(mpg)
  result <- e_svg_register(e, "testSvg", "<svg></svg>")
  expect_equal(result$x$registerMap[[1]]$mapName, "testSvg")
  expect_equal(result$x$registerMap[[1]]$geoJSON$svg, "<svg></svg>")
})

test_that("e_svg_register.echarts4r appends to existing registerMap", {
  e <- mtcars |> e_charts(mpg)
  e$x$registerMap <- list(list(mapName = "existing"))
  result <- e_svg_register(e, "testSvg", "<svg></svg>")
  expect_equal(length(result$x$registerMap), 2)
  expect_equal(result$x$registerMap[[2]]$mapName, "testSvg")
})

test_that("e_svg_register.echarts4r returns echarts4r object", {
  e <- mtcars |> e_charts(mpg)
  result <- e_svg_register(e, "testSvg", "<svg></svg>")
  expect_s3_class(result, "echarts4r")
  expect_s3_class(result, "htmlwidget")
})

# e_map_register_p --------------------------------------------------------
test_that("e_map_register_p and e_map_register_ui runs with no errors", {
  proxy_called <- shiny::reactiveVal(FALSE)
  cns <- countrycode::codelist$country.name.en[1:5]
  cns <- data.frame(
    country = cns,
    value = round(runif(length(cns), 1, 5), 6)
  )

  ui <- shiny::tagList(
    e_map_register_ui("mapName", json = "json"),
    shiny::fluidPage(
      echarts4rOutput("pie"),
      shiny::actionButton("update", "Update")
    ))

  expect_match(as.character(ui), "echarts.registerMap\\('mapName', map);")

  server <- function(input, output, session) {
    output$pie <- renderEcharts4r({
      cns |>
        e_charts(country) |>
        e_map(value)
    })

    shiny::observeEvent(input$update, {
      e_map_register_p(name = "shinyMap", json = echarts4r::buildings_sample_json)
      proxy_called(TRUE)
    })
  }
  # shinyApp(ui, server)
  shiny::testServer(server, {
    # Trigger update
    session$setInputs(update = 1)
    expect_true(proxy_called())
  })
})

test_that("e_map_register_p returns invisible NULL", {
  mock_session <- list(
    sendCustomMessage = function(type, message) {}
  )

  result <- e_map_register_p("testMap", list(), session = mock_session)
  expect_null(result)
})
# e_mapbox ----------------------------------------------------------------
test_that("e_mapbox adds mapbox dependencies", {
  e <- mtcars |> e_charts(mpg)
  result <- e_mapbox(e, token = "FAKE")
  dep_names <- sapply(result$dependencies, function(x) x$name)
  expect_true("echarts-mapbox" %in% dep_names)
})

# e_map_register.echarts4r ------------------------------------------------
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

# Not sure how to test either of these

