### Tests of the functions in the tab "Statistical"
### https://echarts4r.john-coene.com/articles/stats.html


# e_band ------------------------------------------------------------------
test_that("e_band plot has the good data structure and type", {
  set.seed(1)

  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )

  plot <- df |>
    e_charts(x) |>
    e_line(y) |>
    e_band(lwr, upr)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(1.000000, 6.327543)),
      list(value = c(2.000000, 6.860619)),
      list(value = c(3.000000, 7.864267)),
      list(value = c(4.000000, 9.541039)),
      list(value = c(5.00000, 6.00841))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
  expect_error(df |> dplyr::group_by(x) |> e_chart(timeline = TRUE) |>
                 e_band(lwr, upre) , "timeline not supported")
})

test_that("e_band.echarts4rProxy plot responds", {

  # Looking at lwr bc upr is based on the difference
  set.seed(1)
  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6),
      new_lwr = lwr - 1
    )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      df |>
        e_charts(x) |>
        e_band(min = lwr, max = upr)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = df, x = x) |>
        e_band(min = new_lwr, max = upr) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(
      do.call(rbind, json$x$opts$series$data[[1]]$value),
      unname(as.matrix( df[c("x", "lwr")]))
    )

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname() |> sort()

    expected <- df[c("new_lwr", "x")] |> unlist() |> sort() |> unname()

    expect_equal(new_values, expected)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )

    expect_error(echarts4rProxy("line", data = df) |>
                   e_band(), "must pass min and max")
  })
})

test_that("e_band.echarts4r and e_band_ expects error when missing e and min,max", {
  expect_error(iris |> e_charts() |> e_band.echarts4r(), "must pass min and max")
 expect_error(e_band.echarts4r() , "must pass e")

 expect_error( iris |> e_charts() |> e_band_(), "must pass min and max")
  expect_error(e_band_(), "must pass e")
})

# e_band2 -----------------------------------------------------------------

test_that("e_band2 plot has good data structure and type", {
  df <- data.frame(x = seq(3), y = c(1, 3, 9), w = c(3, 4, 3))
  plot <- df |>
    e_charts(x) |>
    e_band2(y, w)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$renderItem,
    htmlwidgets::JS("renderBand")
  )
  expect_equal(
    plot$x$opts$series[[1]]$data[[2]]$value,
    c(2, 3, 4)
  )
})

test_that("e_band2 timeline works", {

  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6),
    group = c("A", "A", "A", "B", "B")
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )

  plot <- df |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |>
    e_line(y) |>
    e_band2(lwr, upr)

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "custom")
})

test_that("e_band2 x and y index works", {
  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )

  plot <- df |>
    e_charts(x) |>
    e_band2(lwr, upr,
            y_index = 10,
            x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_band2.echarts4rProxy plot responds", {

  # Looking at lwr bc upr is based on the difference
  set.seed(1)
  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6),
      new_lwr = lwr - 1
    )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      df |>
        e_charts(x) |>
        e_band2( lwr, upr)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = df, x = x) |>
        e_band2(new_lwr, upr) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(
      do.call(rbind, json$x$opts$series$data[[1]]$value),
      unname(as.matrix( df[c("x", "lwr", "upr")]))
    )

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname() |> sort()

    expected <- df[c("new_lwr", "x", "upr")] |> unlist() |> sort() |> unname()

    expect_equal(new_values, expected)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "custom"
    )

    expect_error(echarts4rProxy("line", data = df) |>
                   e_band2(), "must pass lower and upper")
  })
})

test_that("e_band2.echarts4r and e_band2_ expects error when missing e and lower,upper and chech coord_system", {
  expect_error(iris |> e_charts() |> e_band2.echarts4r(),
               "must pass lower and upper")
  expect_error(iris |> e_charts() |> e_band2.echarts4r(lower = Sepal.Length, upper = Sepal.Length, coord_system = "ERROR"),
               "only cartesian2d supported")
  expect_error(e_band2.echarts4r(),"must pass e")

 expect_error( iris |> e_charts() |> e_band2_(), "must pass lower and upper")
 expect_error( iris |> e_charts() |> e_band2_(lower = "Sepal.Length", upper = "Sepal.Length", coord_system = "ERROR") ,
               "only cartesian2d supported")
 expect_error( e_band2_(), "must pass e")
})

# e_correlations ----------------------------------------------------------
test_that("e_correlations plot has the good data structure and type", {
  plot <- cor(mtcars[6:9, 1:3]) |>
    round(7) |>
    e_charts() |>
    e_correlations(order = "hclust") |>
    e_tooltip()

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c("mpg", "mpg", " 1.0000000")),
      list(value = c("cyl", "mpg", "-0.9856266")),
      list(value = c("disp", "mpg", "-0.9618611")),
      list(value = c("mpg", "cyl", "-0.9856266")),
      list(value = c("cyl", "cyl", " 1.0000000")),
      list(value = c("disp", "cyl", " 0.9912808")),
      list(value = c("mpg", "disp", "-0.9618611")),
      list(value = c("cyl", "disp", " 0.9912808")),
      list(value = c("disp", "disp", " 1.0000000"))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "heatmap"
  )
})


test_that("e_line.echarts4rProxy plot responds", {
 test_data <-  cor(mtcars[6:9, 1:3]) |>
   round(7)

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      test_data |>
        e_charts() |>
        e_correlations(order = "hclust", visual_map = FALSE) |> e_tooltip()
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = test_data) |>
        e_correlations(order = "hclust") |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    cor_values <- do.call(rbind, json$x$opts$series$data[[1]]$value)[, 3]

    expect_all_true(as.numeric(cor_values) %in% test_data)
    session$setInputs(update = 1)
    session$flushReact()

    new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x$value[3]) |> unlist() |> as.numeric()

    expect_all_true(new_values %in% test_data)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "heatmap"
    )
  })
})

test_that("e_correlations.echarts4r() expects error when missing e", {
  expect_error(e_correlations.echarts4r(),"must pass e")
})

# e_error -----------------------------------------------------------------
test_that("e_error_bar plot has the good data structure and type", {
  df <- data.frame(
    x = factor(c(1, 2)),
    y = c(1, 5),
    upper = c(1.1, 5.3),
    lower = c(0.8, 4.3)
  )

  plot <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_error_bar(lower, upper)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c("1", "1")),
      list(value = c("2", "5"))
    )
  )
  expect_equal(
    plot$x$opts$series[[2]]$type,
    "custom"
  )
  expect_equal(
    plot$x$opts$series[[2]]$renderItem,
    htmlwidgets::JS("sessionStorage.setItem('ErrorBar.oss','[\"\",\"\",\"1\"]'); renderErrorBar2")
  )
})

test_that("e_error_bar timeline works", {

  df <- data.frame(
    x = factor(c(1, 2)),
    y = c(1, 5),
    upper = c(1.1, 5.3),
    lower = c(0.8, 4.3),
    group = c("A", "B")
  )

  plot <- df |> dplyr::group_by(group) |> e_chart(x, timeline = TRUE) |>
    e_bar(y) |>
    e_error_bar(lower, upper)

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "custom")
})

test_that("e_error_bar x and y index works", {
  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )

  plot <- df |>
    e_charts(x) |>
    e_bar(y) |>
    e_error_bar(lwr, upr,
            y_index = 10,
            x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_error_bar.echarts4rProxy plot responds", {

  set.seed(1)

  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      df |>
        e_charts(x) |>
        e_bar(y) |>
        e_error_bar(lwr, upr)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = df) |>
        e_bar(y) |>
        e_error_bar(lwr, upr) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    expect_equal(
      do.call(rbind, json$x$opts$series$data[[2]]$value),
      unname(as.matrix( df[c("x", "lwr", "upr")]))
    )

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[2]]$data |> unlist() |> unname()

    expect_all_true(new_values %in% unlist(df[c("lwr", "upr")]))
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[2]]$type,
      "custom"
    )

    expect_error(echarts4rProxy("line", data = df) |>
                   e_error_bar(), "must pass lower, or upper")
  })
})

test_that("e_error_bar.echarts4r and e_error_bar_ expects error when missing e and serie", {
  expect_error( iris |> e_charts() |> e_error_bar.echarts4r() ,
                  "must pass lower and upper")
  expect_error(e_error_bar.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_error_bar_(),
               "must pass lower and upper")
  expect_error(e_error_bar_(),
               "must pass e")
})

# e_boxplot ---------------------------------------------------------------

test_that("e_boxplot plot has the good data structure and type", {
  set.seed(1)
  df <- data.frame(
    x = c(
      round(rnorm(100), 5),
      round(runif(100, -5, 10), 5),
      round(rnorm(100, 10, 3), 5)
    ),
    grp = c(
      rep(LETTERS[1], 100),
      rep(LETTERS[2], 100),
      rep(LETTERS[3], 100)
    )
  )

  plot <- df |>
    group_by(grp) |>
    e_charts() |>
    e_boxplot(x)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  ### The decimals are problematic so instead, check that the differences between the two expressions tend towards zero

  # difference between the two expressions
  difference <- unlist(plot$x$opts$series[[1]]$data) - unlist(list(
    c(-2.214700, -0.510335, 0.113909, 0.693351, 2.401618),
    c(-4.583193, -2.199190, 0.671524, 4.607498, 9.723452),
    c(3.144293, 8.093827, 9.880848, 12.061566, 17.492985)
  ))
  difference_test <- difference < 10^-5

  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "boxplot"
  )
})

# TODO timeline should probably not be supported
# test_that("e_boxplot timeline works", {
#
#   df <- data.frame(
#     x = c(
#       round(rnorm(100), 5),
#       round(runif(100, -5, 10), 5),
#       round(rnorm(100, 10, 3), 5)
#     ),
#     grp = c(
#       rep(LETTERS[1], 100),
#       rep(LETTERS[2], 100),
#       rep(LETTERS[3], 100)
#     ),
#     group = rep("A", 100)
#   )
#   # Cant get this work. Plot breaks when group is removed from e_chart()
#   plot <- df |> dplyr::group_by(grp) |> e_chart(group, timeline = TRUE) |>
#     e_boxplot(x)
#
#   expect_true(plot$x$tl)
#
#   # Time series
#   expect_equal(
#     plot$x$opts$baseOption$timeline$data,
#     list("A", "B")
#   )
#
#   expect_equal(plot$x$opts$baseOption$series[[2]]$type, "custom")
# })

test_that("e_boxplot.echarts4rProxy plot responds", {

  set.seed(1)
  df <- data.frame(
    x = c(
      round(rnorm(100), 5),
      round(runif(100, -5, 10), 5),
      round(rnorm(100, 10, 3), 5)
    ),
    y = c(
      round(rnorm(100), 5),
      round(runif(100, -5, 10), 5),
      round(rnorm(100, 10, 3), 5)
    ),
    grp = c(
      rep(LETTERS[1], 100),
      rep(LETTERS[2], 100),
      rep(LETTERS[3], 100)
    )
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      df |>
        group_by(grp) |>
        e_charts() |>
        e_boxplot(x)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = df) |>
        e_boxplot(y) |>
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

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "boxplot"
    )

    expect_error(echarts4rProxy("line", data = df) |>
                   e_boxplot(), "must pass serie")
  })
})


test_that("e_boxplot.echarts4r and e_boxplot_ expects error when missing e and serie", {
  expect_error(iris |> e_charts() |> e_boxplot.echarts4r(),
               "must pass serie")
  expect_error(e_boxplot.echarts4r(), "must pass e")

  expect_error( iris |> e_charts() |> e_boxplot_(),
                "must pass serie")
  expect_error(e_boxplot_(), "must pass e")
})

# e_histogram -------------------------------------------------------------
test_that("e_histogram plot has the good data structure and type", {
  set.seed(1)
  df <- data.frame(
    x = 1:10,
    y = rnorm(10, 20, 12)
  )

  plot <- df |>
    e_charts() |>
    e_histogram(y) |>
    e_tooltip()

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(7.5, 1),
      list(12.5, 2),
      list(17.5, 1),
      list(22.5, 2),
      list(27.5, 3),
      list(32.5, 0),
      list(37.5, 1)
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "bar"
  )
})
test_that("e_histogram x and y index works", {
  plot <- iris |> e_chart(Species) |>
    e_histogram(Sepal.Length,
           y_index = 10,
           x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})
test_that("e_histogram.echarts4rProxy plot responds", {

  test_data <- data.frame(
    x = c(1,1,1, 1),
    z = c(2, 2, 5, 4),
    y = c(1, 1, 1, 1)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_histogram(z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = test_data) |>
        e_histogram(y) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    # 2 1 1 - those are counts
    expect_equal(
      json$x$opts$series$data[[1]] |> as.vector(),
      c(2.5, 3.5, 4.5, 2.0, 1.0, 1.0))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist()

    expect_equal(new_values,c(0.5, 4))
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "bar"
    )

    expect_error(echarts4rProxy("line", data = test_data) |>
                   e_histogram(), "must pass serie")
  })
})

test_that("e_histogram.echarts4r.echarts4r and e_histogram_ expects error when missing e and serie", {
   expect_error(iris |> e_charts() |> e_histogram.echarts4r(), "must pass serie")
   expect_error(e_histogram.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_histogram_(), "must pass serie")
  expect_error(e_histogram_(), "must pass e")
})


# e_density ---------------------------------------------------------------
test_that("e_density plot has the good data structure and type", {
  set.seed(1)
  df <- data.frame(
    x = 1:10,
    y = rnorm(10, 20, 12)
  )

  plot <- df |>
    e_charts() |>
    e_histogram(y) |>
    e_density(
      y,
      name = "density",
      areaStyle = list(opacity = .4),
      smooth = TRUE,
      y_index = 1
    ) |>
    e_tooltip()

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(7.5, 1),
      list(12.5, 2),
      list(17.5, 1),
      list(22.5, 2),
      list(27.5, 3),
      list(32.5, 0),
      list(37.5, 1)
    )
  )
  expect_equal(
    plot$x$opts$series[[2]]$type,
    "line"
  )
})

test_that("e_density timeline works", {

  df <- data.frame(
    x = 1:10,
    y = rnorm(10, 20, 12),
    group = rep(c("A", "B"), 5)
  )

  plot <- df |> dplyr::group_by(group) |> e_chart( timeline = TRUE) |>
    e_histogram(y) |>
    e_density(
      y,
      name = "density",
      areaStyle = list(opacity = .4),
      smooth = TRUE,
      y_index = 1
    )

  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("A", "B")
  )

  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "line")
})

test_that("e_density x and y index works", {
  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) |>
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )

  plot <- df |>
    e_charts(x) |>
    e_density(lwr,
            y_index = 10,
            x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_density.echarts4rProxy plot responds", {

  test_data <- data.frame(
    x = c(1,1,1, 1),
    z = c(2, 2, 5, 4),
    y = c(1, 1, 1, 1)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_density(z)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = test_data) |>
        e_density(y) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)

    #  0.50 0.25 0.25 - those are counts
    expect_equal(
      json$x$opts$series$data[[1]] |> as.vector(),
      c(2.5, 3.5, 4.5, 0.50, 0.25, 0.25))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist()

    expect_equal(new_values,c(0.5, 1))
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )

    expect_error(echarts4rProxy("line", data = test_data) |>
                   e_density(), "must pass serie")
  })
})


test_that("e_density.echarts4r and e_density_ expects error when missing e and serie", {
   expect_error(iris |> e_charts() |> e_density.echarts4r(), "must pass serie")
   expect_error(e_density.echarts4r(), "must pass e")

   expect_error(iris |> e_charts() |> e_density_(), "must pass serie")
   expect_error(e_density_() , "must pass e")
})

# e_lm --------------------------------------------------------------------

test_that("e_lm plot has the good data structure and type", {
  plot <- iris[c(1:3, 101:103), ] |>
    group_by(Species) |>
    e_charts_("Sepal.Length") |>
    e_line(Sepal.Width) |>
    e_lm(Sepal.Width ~ Sepal.Length) |>
    e_x_axis(min = 4)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(4.7, 3.2)),
      list(value = c(4.9, 3.0)),
      list(value = c(5.1, 3.5))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})

test_that("e_lm.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      iris |>
        group_by(Species) |>
        e_charts(Sepal.Length) |>
        e_lm(Sepal.Width ~ Sepal.Length)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = iris) |>
        e_lm(Sepal.Width ~ Sepal.Length) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$line)

    # There are 3 groups in the group_by
    expect_equal(
      length(json$x$opts$series$data),
      3L
    )

    session$setInputs(update = 1)
    session$flushReact()

    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    # Only 1 group and so total length is nrow of iris
    expect_length(proxy_chart()$chart$x$opts$series, 1)
    expect_length(new_values, 150)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )
  })
})

test_that("e_lm timeline works", {

  plot <- mtcars |>
    dplyr::group_by(cyl) |>
    e_charts( mpg, timeline = TRUE) |>
    e_scatter(qsec) |>
    e_lm(mpg ~ qsec)


  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("4", "6", "8")
  )

  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "line")
})

test_that("e_lm.echarts4r expects error when missing e and serie", {
  expect_error(iris |> e_charts() |> e_lm.echarts4r("Sepal.Length" ~ "Sepal.Length", name = c("one", "two")), "More groups than names")
  expect_error(e_lm.echarts4r(), "must pass e")
})

# e_glm -------------------------------------------------------------------
test_that("e_glm plot has the good data structure and type", {
  plot <- iris[c(1:3, 101:103), ] |>
    group_by(Species) |>
    e_charts_("Sepal.Length") |>
    e_line(Sepal.Width) |>
    e_glm(Sepal.Width ~ Sepal.Length) |>
    e_x_axis(min = 4)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(4.7, 3.2)),
      list(value = c(4.9, 3.0)),
      list(value = c(5.1, 3.5))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})
test_that("e_glm timeline works", {
  plot <- mtcars |>
    dplyr::group_by(cyl) |>
    e_charts( mpg, timeline = TRUE) |>
    e_scatter(qsec) |>
    e_glm(mpg ~ qsec)


  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("4", "6", "8")
  )
  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "line")
})
test_that("e_glm.echarts4rProxy plot responds", {
  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      iris |>
        group_by(Species) |>
        e_charts(Sepal.Length) |>
        e_glm(Sepal.Width ~ Sepal.Length)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = iris) |>
        e_glm(Sepal.Width ~ Sepal.Length) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$line)

    # There are 3 groups in the group_by
    expect_equal(
      length(json$x$opts$series$data),
      3L
    )

    session$setInputs(update = 1)
    session$flushReact()

    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    # Only 1 group and so total length is nrow of iris
    expect_length(proxy_chart()$chart$x$opts$series, 1)
    expect_length(new_values, 150)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )
  })
})

test_that("e_glm.echarts4r expects error when missing e and serie", {
  expect_error(e_glm.echarts4r(), "must pass e")
})


# e_loess -----------------------------------------------------------------

test_that("e_loess plot has the good data structure and type", {
  plot <- mtcars[1:7, ] |>
    e_charts(disp) |>
    e_scatter(mpg, qsec) |>
    e_loess(mpg ~ disp, name = "NAME")

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  ### same as earlier

  # expect_equal(
  #   plot$x$opts$series[[1]]$data,
  #   list(list(value = c(108.00000, 22.80000, 18.61000, 13.01598)),
  #        list(value = c(160.000000, 21.000000, 16.460000,  3.689498)),
  #        list(value = c(160.000000, 21.000000, 17.020000,  6.118721)),
  #        list(value = c(225.00, 18.10, 20.22, 20.00)),
  #        list(value = c(258.00000, 21.40000, 19.44000, 16.61644)),
  #        list(value = c(360.000000, 18.700000, 17.020000,  6.118721)),
  #        list(value = c(360.00, 14.30, 15.84,  1.00))
  #   ))


  difference <-
    unlist(plot$x$opts$series[[1]]$data) - unlist(list(
      list(value = c(108.00000, 22.80000, 18.61000, 13.01598)),
      list(value = c(
        160.000000,
        21.000000,
        16.460000,
        3.689498
      )),
      list(value = c(
        160.000000,
        21.000000,
        17.020000,
        6.118721
      )),
      list(value = c(225.00, 18.10, 20.22, 20.00)),
      list(value = c(258.00000, 21.40000, 19.44000, 16.61644)),
      list(value = c(
        360.000000,
        18.700000,
        17.020000,
        6.118721
      )),
      list(value = c(360.00, 14.30, 15.84, 1.00))
    ))
  difference_test <- difference < 10^-5

  expect_true(unique(difference_test))

  expect_equal(plot$x$opts$series[[2]]$name, "NAME")

  expect_equal(
    plot$x$opts$series[[2]]$type,
    "line"
  )
})

test_that("e_loess timeline works", {

  plot <- mtcars |>
    dplyr::group_by(cyl) |>
    e_charts( mpg, timeline = TRUE) |>
    e_scatter(qsec) |>
    e_loess(mpg ~ qsec)


  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    plot$x$opts$baseOption$timeline$data,
    list("4", "6", "8")
  )

  expect_equal(plot$x$opts$baseOption$series[[2]]$type, "line")
})

test_that("e_loess x and y index works", {
  plot <- mtcars[1:7, ] |>
    e_charts(disp) |>
    e_scatter(mpg, qsec) |>
    e_loess(mpg ~ disp,
                y_index = 10,
                x_index = 10
    )
  expect_gt(length(plot$x$opts$xAxis), 10)
  expect_gt(length(plot$x$opts$yAxis), 10)
})

test_that("e_loess.echarts4rProxy plot responds", {

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      iris |>
        group_by(Species) |>
        e_charts(Sepal.Length) |>
        e_loess(Sepal.Width ~ Sepal.Length)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line",
                              data = iris) |>
        e_loess(Sepal.Width ~ Sepal.Length) |>
        e_execute()
      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$line)

    # There are 3 groups in the group_by
    expect_equal(
      length(json$x$opts$series$data),
      3L
    )

    session$setInputs(update = 1)
    session$flushReact()

    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    # Only 1 group and so total length is nrow of iris
    expect_length(proxy_chart()$chart$x$opts$series, 1)
    expect_length(new_values, 150)
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "line"
    )
  })
})


test_that("e_loess.echarts4r expects error when missing e", {
  expect_error(e_loess.echarts4r(), "must pass e")
})
