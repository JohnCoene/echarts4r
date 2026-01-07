### Tests of the functions in the tab "Web-GL"
### https://echarts4r.john-coene.com/articles/gl.html

# e_surface ---------------------------------------------------------------

### Not possible to test the data as in other tests: subsetting to a 100 points displays almost nothing on the plot
test_that("e_surface plot has the good data structure and type", {
  data("volcano")

  surface <- as.data.frame(as.table(volcano))
  surface$Var1 <- as.numeric(surface$Var1)
  surface$Var2 <- as.numeric(surface$Var2)

  plot <- surface |>
    e_charts(Var1) |>
    e_surface(Var2, Freq) |>
    e_visual_map(Freq)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_error(surface |> dplyr::group_by(Var1) |> e_chart(timeline = TRUE) |>
                 e_surface(Var2, Freq) , "timeline not supported")
})

test_that("e_surface.echarts4rProxy plot responds", {

  surface <- as.data.frame(as.table(volcano))
  surface$Var1 <- as.numeric(surface$Var1)
  surface$Var2 <- as.numeric(surface$Var2)

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      plot <- surface |>
        e_charts(Var1) |>
        e_surface(Var2, Freq)
    })

    observeEvent(input$update, {

      chart <- echarts4rProxy("line",
                              data = surface) |>
        e_surface(Var2, Freq) |>
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

    new_values <- lapply(proxy_chart()$chart$x$opts$series[[1]]$data, \(x) x) |> unlist()
    # Updated values do not have the x value
    expected_values <- .build_data2(surface, "Var2", "Freq") |> unlist() |> unname()

    expect_identical(
      new_values, expected_values
    )

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "surface"
    )

    expect_error(echarts4rProxy("line", data = surface) |>
                   e_surface(), "must pass y and z")
  })
})

test_that("e_surface.echarts4r and e_surface_ expects error when missing e and y, z", {
  expect_error(iris |> e_charts() |> e_surface.echarts4r(), "must pass y and z")
  expect_error(e_surface.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_surface_(),
   "must pass y and z")
  expect_error(e_surface_(), "must pass e")
})


# e_scatter ---------------------------------------------------------------

test_that("e_scatter_gl plot has the good data structure and type", {
  plot <- quakes[1:5, ] |>
    e_charts(long) |>
    e_geo(
      roam = TRUE,
      boundingCoords = list(
        c(185, -10),
        c(165, -40)
      )
    ) |>
    e_scatter_gl(lat, depth) |>
    e_visual_map()

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(181.03, -20.62, 650.00)),
      list(value = c(181.62, -20.42, 562.00)),
      list(value = c(181.66, -17.97, 626.00)),
      list(value = c(181.96, -20.42, 649.00)),
      list(value = c(184.1, -26.0, 42.0))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "scatterGL"
  )
})

# TODO this render but I dont see the scatter when coord sys is cart
test_that("e_scatter_gl cartesian3D coords", {
  plot <- quakes[1:5, ] |>
    e_charts(long) |>
    e_scatter_gl(lat, depth, coord_system = "cartesian3D", symbolSize = 20)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(
      list(value = c(181.03, -20.62, 650.00)),
      list(value = c(181.62, -20.42, 562.00)),
      list(value = c(181.66, -17.97, 626.00)),
      list(value = c(181.96, -20.42, 649.00)),
      list(value = c(184.1, -26.0, 42.0))
    )
  )
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "scatterGL"
  )
})

test_that("e_scatter_gl timeline works", {

  plot <- quakes[1:5, ] |> dplyr::group_by(stations) |> e_chart(long, timeline = TRUE) |>
    e_geo(
      roam = TRUE,
      boundingCoords = list(
        c(185, -10),
        c(165, -40)
      )
    ) |>
    e_scatter_gl(lat, depth)
  expect_true(plot$x$tl)

  # Time series
  expect_equal(
    as.integer(unlist(plot$x$opts$baseOption$timeline$data)),
    quakes[1:5, "stations"] |> sort()
  )

  expect_equal(plot$x$opts$baseOption$series[[1]]$type, "scatterGL")
})

test_that("e_scatter_gl.echarts4rProxy plot responds", {

  test_data <-  data.frame(
    x = seq(3),
    y = c(1, 3, 9),
    z = c(2, 5, 4),
    w = c(3, 4, 3)
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$scatter <- renderEcharts4r({
      test_data |>
        e_charts(x) |>
        e_scatter_gl(y, z, name = "Serie 1")
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("scatter",
                              data = test_data) |>
        e_scatter_gl(y, y, name = "Serie 1") |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())
    json <- jsonlite::fromJSON(output$scatter)

    # Data matches - this is the first point
    expect_identical(
      json$x$opts$series$data[[1]]$value[[1]],
      c(1L, 1L, 2L))

    session$setInputs(update = 1)
    session$flushReact()

    # Update to line x
    new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
    expect_equal(new_values,c(1,1, 3,3,9, 9))

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "scatterGL"
    )
    expect_error(echarts4rProxy("scatterGL", data = test_data) |>
                   e_scatter_gl(), "must pass y and z")
  })
})

test_that("e_scatter_gl.echarts4r and e_scatter_gl_ expects error when missing e and y,z", {
  expect_error(iris |> e_charts() |> e_scatter_gl.echarts4r(),
                   "must pass y and z")
  expect_error( e_scatter_gl.echarts4r(),
                   "must pass e")

   expect_error(iris |> e_charts() |> e_scatter_gl_(),
                   "must pass y and z")
   expect_error(e_scatter_gl_(),
                   "must pass e")
})

# e_graph -----------------------------------------------------------------
test_that("e_graph_gl plot has the good data structure and type", {
  nodes <- data.frame(
    name = paste0(LETTERS, 1:50),
    value = rnorm(50, 10, 2),
    size = rnorm(50, 10, 2),
    grp = rep(c("grp1", "grp2", "grp3"), 100),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    source = sample(nodes$name, 400, replace = TRUE),
    target = sample(nodes$name, 400, replace = TRUE),
    stringsAsFactors = FALSE
  )

  plot <- e_charts() |>
    e_graph_gl() |>
    e_graph_nodes(nodes, name, value, size, grp) |>
    e_graph_edges(edges, source, target)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  e <- iris |> dplyr::group_by(Species) |> e_charts(timeline = TRUE)
  expect_error(e |>
                 e_graph_gl() , "timeline not supported")
})

test_that("e_graph.echarts4r and e_graph_nodes expects error when missing e ", {
  expect_error(e_graph.echarts4r(), "must pass e")
  expect_error(e_graph_gl.echarts4r(), "must pass e")
  expect_error(e_graph_nodes.echarts4r(), "must pass e")
  expect_error(iris |> e_charts() |> e_graph_nodes.echarts4r(), "missing arguments")

  expect_error(e_graph_edges.echarts4r(), "must pass e")
  expect_error(iris |> e_charts() |> e_graph_edges.echarts4r(), "must pass edges, source and target")
})

test_that("node with missing category", {
  nodes <- data.frame(
    name = paste0(LETTERS, 1:50),
    value = rnorm(50, 10, 2),
    size = rnorm(50, 10, 2),
    grp = rep(c("grp1", "grp2", "grp3"), 100),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    source = sample(nodes$name, 400, replace = TRUE),
    target = sample(nodes$name, 400, replace = TRUE),
    stringsAsFactors = FALSE
  )

  plot <- e_charts() |>
    e_graph_gl() |>
    e_graph_nodes(nodes, name, value, size) |>
    e_graph_edges(edges, source, target)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("node with missing category and size", {
  nodes <- data.frame(
    name = paste0(LETTERS, 1:50),
    value = rnorm(50, 10, 2),
    size = rnorm(50, 10, 2),
    grp = rep(c("grp1", "grp2", "grp3"), 100),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    source = sample(nodes$name, 40, replace = TRUE),
    target = sample(nodes$name, 40, replace = TRUE),
    stringsAsFactors = FALSE
  )

  plot <- e_charts() |>
    e_graph_gl() |>
    e_graph_nodes(nodes, name, value) |>
    e_graph_edges(edges, source, target)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("edges with color", {

  edges <- data.frame(
    source = sample(LETTERS, 40, replace = TRUE),
    target = sample(LETTERS, 40, replace = TRUE),
    color = "#000",
    stringsAsFactors = FALSE
  )

  plot <- e_charts() |>
    e_graph_gl() |>
    e_graph_edges(edges, source, target, color = color)
  expect_equal(plot$x$opts$series[[1]]$links[[1]]$lineStyle$color, "#000")
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("node with category and size", {
  nodes <- data.frame(
    name = paste0(LETTERS, 1:50),
    value = rnorm(50, 10, 2),
    size = rnorm(50, 10, 2),
    grp = rep(c("grp1", "grp2", "grp3"), 100),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    source = sample(nodes$name, 40, replace = TRUE),
    target = sample(nodes$name, 40, replace = TRUE),
    size = sample(nodes$name, 40, replace = TRUE),
    stringsAsFactors = FALSE
  )

  plot <- e_charts() |>
    e_graph_gl() |>
    e_graph_nodes(nodes, name, value, size = size, category = grp) |>
    e_graph_edges(edges, source, target, size = size)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_graph.echarts4r and e_graph_nodes expects error when missing e ", {
  expect_error(e_graph.echarts4r(), "must pass e")
  expect_error(e_graph_nodes.echarts4r(), "must pass e")
  expect_error(iris |> e_charts() |> e_graph_nodes.echarts4r(), "missing arguments")

  expect_error(e_graph_edges.echarts4r(), "must pass e")
  expect_error(iris |> e_charts() |> e_graph_edges.echarts4r(), "must pass edges, source and target")
})

test_that("e_graph.echarts4rProxy plot responds", {

  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    value = c("A", "B", "C", "D", "E"),
    group = c("gr1", "gr1", "gr2", "gr2", "gr3"),
    size = 3:7 * 10,
    x = c(0, 200, 400, 600, 800),
    y = c(100, 100, 200, 200, 0)
  )

  edges <- data.frame(
    source = c("A", "B", "C", "D", "E"),
    target = c("B", "C", "D", "E", "D"),
    size = rep(3, 5),
    color = c("red", "green", "blue", "yellow", "black")
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_graph(layout = "none", autoCurveness = TRUE) |>
        e_graph_nodes(nodes, name, value, size, category = group, xpos = x, ypos = y)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = nodes) |>
        e_graph(layout = "circular") |> e_execute()

      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(json$x$opts$series$layout, "none")

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_equal(proxy_chart()$chart$x$opts$series[[1]]$layout, "circular")
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "graph"
    )
  })
})

test_that("e_graph_nodes.echarts4rProxy with category and size", {

  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    value = c("A", "B", "C", "D", "E"),
    group = c("gr1", "gr1", "gr2", "gr2", "gr3"),
    size = 3:7 * 10,
    x = c(0, 200, 400, 600, 800),
    y = c(100, 100, 200, 200, 0)
  )

  edges <- data.frame(
    source = c("A", "B", "C", "D", "E"),
    target = c("B", "C", "D", "E", "D"),
    size = rep(3, 5),
    color = c("red", "green", "blue", "yellow", "black")
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_graph(layout = "none", autoCurveness = TRUE) |>
        e_graph_nodes(nodes, name, value, size, category = group, xpos = x, ypos = y)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = nodes) |>
        e_graph(layout = "circular") |>
        e_graph_nodes(nodes, name, value, size, category = group, xpos = x, ypos = y) |>
        e_execute()

      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(json$x$opts$series$layout, "none")

    session$setInputs(update = 1)
    session$flushReact()

    expect_equal(proxy_chart()$chart$x$opts$series[[1]]$layout, "circular")
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "graph"
    )
  })
})

test_that("e_graph_nodes.echarts4rProxy with missing category", {

  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    value = c("A", "B", "C", "D", "E"),
    group = c("gr1", "gr1", "gr2", "gr2", "gr3"),
    size = 3:7 * 10,
    x = c(0, 200, 400, 600, 800),
    y = c(100, 100, 200, 200, 0)
  )

  edges <- data.frame(
    source = c("A", "B", "C", "D", "E"),
    target = c("B", "C", "D", "E", "D"),
    size = rep(3, 5),
    color = c("red", "green", "blue", "yellow", "black")
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_graph(layout = "none", autoCurveness = TRUE) |>
        e_graph_nodes(nodes, name, value, size, xpos = x, ypos = y)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = nodes) |>
        e_graph(layout = "circular") |>
        e_graph_nodes(nodes, name, value, size, xpos = x, ypos = y) |>
        e_execute()

      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(json$x$opts$series$layout, "none")

    session$setInputs(update = 1)
    session$flushReact()

    expect_equal(proxy_chart()$chart$x$opts$series[[1]]$layout, "circular")
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "graph"
    )
  })
})

test_that("e_graph_edges.echarts4rProxy plot responds", {
  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    value = c("A", "B", "C", "D", "E"),
    group = c("gr1", "gr1", "gr2", "gr2", "gr3"),
    size = 3:7 * 10,
    x = c(0, 200, 400, 600, 800),
    y = c(100, 100, 200, 200, 0)
  )

  edges <- data.frame(
    source = c("A", "B", "C", "D", "E"),
    target = c("B", "C", "D", "E", "D"),
    size = rep(3, 5),
    color = c("red", "green", "blue", "yellow", "black")
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_graph(layout = "none", autoCurveness = TRUE) |>
        # e_graph_nodes(nodes, name, value, size, category = group, xpos = x, ypos = y) |>
        e_graph_edges(edges, source, target, size = size, color = color)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = nodes) |>
        e_graph(layout = "circular") |>
        e_graph_edges(edges, source, target, size = size) |>
        e_execute()

      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(json$x$opts$series$layout, "none")

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_equal(proxy_chart()$chart$x$opts$series[[1]]$layout, "circular")
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "graph"
    )
    expect_error(echarts4rProxy("line") |>
                   e_graph_edges(), "must pass edges, source and target")
  })
})

test_that("e_graph_gl.echarts4rProxy plot responds", {
  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    value = c("A", "B", "C", "D", "E"),
    group = c("gr1", "gr1", "gr2", "gr2", "gr3"),
    size = 3:7 * 10,
    x = c(0, 200, 400, 600, 800),
    y = c(100, 100, 200, 200, 0)
  )

  edges <- data.frame(
    source = c("A", "B", "C", "D", "E"),
    target = c("B", "C", "D", "E", "D"),
    size = rep(3, 5),
    color = c("red", "green", "blue", "yellow", "black")
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_graph_gl(layout = "none", autoCurveness = TRUE) |>
        e_graph_nodes(nodes, name, value, size, category = group, xpos = x, ypos = y)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = nodes) |>
        e_graph_gl(layout = "circular")

      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(json$x$opts$series$layout, "none")

    session$setInputs(update = 1)
    session$flushReact()

    expect_equal(proxy_chart()$chart$x$opts$series[[1]]$layout, "circular")
    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "graphGL"
    )
  })
})

test_that("e_graph_nodes.echarts4rProxy plot responds", {
  nodes <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    value = c("A", "B", "C", "D", "E"),
    group = c("gr1", "gr1", "gr2", "gr2", "gr3"),
    size = 3:7 * 10,
    x = c(0, 200, 400, 600, 800),
    y = c(100, 100, 200, 200, 0)
  )

  edges <- data.frame(
    source = c("A", "B", "C", "D", "E"),
    target = c("B", "C", "D", "E", "D"),
    size = rep(3, 5),
    color = c("red", "green", "blue", "yellow", "black")
  )

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$line <- renderEcharts4r({
      e_charts() |>
        e_graph_gl(layout = "none", autoCurveness = TRUE) |>
        e_graph_nodes(nodes, name, value, size, category = group, xpos = x, ypos = y)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("line", data = nodes) |>
        e_graph_gl() |>
        e_graph_nodes(nodes = nodes, names = name, value = value)

      proxy_chart(chart)

      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {

    expect_false(proxy_called())

    json <- jsonlite::fromJSON(output$line)
    expect_equal(json$x$opts$series$layout, "none")

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "graphGL"
    )

    expect_error(echarts4rProxy("line") |>
                   e_graph_nodes(), "missing arguments")
  })

})


# e_flow ------------------------------------------------------------------

test_that("e_flow_gl plot has the good data structure and type", {
  set.seed(1)
  vectors <- expand.grid(x = -3:3, y = -3:3)
  mu <- 1
  vectors$sx <- vectors$y
  vectors$sy <- mu * (1 - vectors$x^2) * vectors$y - vectors$x
  vectors$color <- log10(runif(nrow(vectors), 1, 10))

  # increase the number to better visualize the plot
  plot <- vectors[1:10, ] |>
    dplyr::mutate(color = round(color, 6)) |>
    e_charts(x) |>
    e_flow_gl(y, sx, sy, color) |>
    e_visual_map(
      min = 0,
      max = 1,
      # log 10
      dimension = 4,
      # x = 0, y = 1, sx = 3, sy = 4
      show = FALSE,
      # hide
      inRange = list(
        color = c(
          "#313695",
          "#4575b4",
          "#74add1",
          "#abd9e9",
          "#e0f3f8",
          "#ffffbf",
          "#fee090",
          "#fdae61",
          "#f46d43",
          "#d73027",
          "#a50026"
        )
      )
    ) |>
    e_x_axis(
      splitLine = list(show = FALSE)
    ) |>
    e_y_axis(
      splitLine = list(show = FALSE)
    )

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")

  # expect_equal(
  #   plot$x$opts$series[[1]]$data,
  #   list(list(value = c(-3.000000, -3.000000, -3.000000, 27.000000,  0.530146),
  #        list(value = c(-3.000000, -2.000000, -2.000000, 19.000000,  0.841809)),
  #        list(value = c(-2.000000, -3.000000, -3.000000, 11.000000,  0.638401)),
  #        list(value = c(-2.000000, -2.000000, -2.000000, 8.000000,  0.823606)),
  #        list(value = c(-1.000000, -3.000000, -3.000000, 1.000000,  0.789276)),
  #        list(value = c(-1.000000,-2.000000,-2.000000, 1.000000,  0.192031)),
  #        list(value = c(0.000000, -3.000000, -3.000000, -3.000000,  0.962553)),
  #        list(value = c(1.000000, -3.000000, -3.000000, -1.000000,  0.4495)),
  #        list(value = c(2.000000, -3.000000, -3.000000, 7.000000,  0.958349)),
  #        list(value = c(3.000000, -3.000000, -3.000000, 21.000000,  0.977819))))
  # )

  # test that difference is near 0 because decimals are problematic
  difference <-
    unlist(plot$x$opts$series[[1]]$data) - unlist(list(
      list(
        value = c(-3.000000, -3.000000, -3.000000, 27.000000, 0.530146),
        list(value = c(
          -3.000000,
          -2.000000,
          -2.000000,
          19.000000,
          0.841809
        )),
        list(value = c(
          -2.000000,
          -3.000000,
          -3.000000,
          11.000000,
          0.638401
        )),
        list(value = c(
          -2.000000,
          -2.000000,
          -2.000000,
          8.000000,
          0.823606
        )),
        list(value = c(
          -1.000000,
          -3.000000,
          -3.000000,
          1.000000,
          0.789276
        )),
        list(value = c(
          -1.000000,
          -2.000000,
          -2.000000,
          1.000000,
          0.192031
        )),
        list(value = c(
          0.000000,
          -3.000000,
          -3.000000,
          -3.000000,
          0.962553
        )),
        list(value = c(
          1.000000,
          -3.000000,
          -3.000000,
          -1.000000,
          0.4495
        )),
        list(value = c(
          2.000000,
          -3.000000,
          -3.000000,
          7.000000,
          0.958349
        )),
        list(value = c(
          3.000000,
          -3.000000,
          -3.000000,
          21.000000,
          0.977819
        ))
      )
    ))
  difference_test <- difference < 10^-5

  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type,
    "flowGL"
  )

  expect_error(vectors[1:10, ] |> dplyr::group_by(color) |> e_chart(timeline = TRUE) |>
                 e_flow_gl(y, sx, sy, color) , "timeline not supported")
})

test_that("e_flow_gl.echarts4rProxy plot responds", {
  vectors <- expand.grid(0:9, 0:9)
  names(vectors) <- c("x", "y")
  vectors$sx <- rnorm(10)
  vectors$sy <- rnorm(10)

  server <- function(input, output, session) {
    proxy_called <- shiny::reactiveVal(FALSE)
    proxy_chart <- shiny::reactiveVal(NULL)

    output$candle <- renderEcharts4r({
      vectors |>
        e_charts(x) |>
        e_flow_gl(y, sx, sy)
    })

    observeEvent(input$update, {
      chart <- echarts4rProxy("candle",
                              data = vectors) |>
        e_flow_gl(y, sx, sx) |>
        e_execute()

      proxy_chart(chart)
      proxy_called(TRUE)
    })
  }

  shiny::testServer(server, {
    expect_false(proxy_called())

    session$setInputs(update = 1)
    session$flushReact()

    # Proxy was called with no errors
    expect_true(proxy_called())

    expect_equal(
      proxy_chart()$chart$x$opts$series[[1]]$type,
      "flowGL"
    )
    expect_error(echarts4rProxy("candle", data = vectors) |>
                   e_flow_gl(), "must pass y and z")
  })
})

test_that("e_flow_gl.echarts4r and e_flow_gl_ expects error when missing e and y,sx,sy", {
  expect_error(iris |> e_charts() |> e_flow_gl.echarts4r(),
               "must pass y, sx, and sy")
  expect_error(e_flow_gl.echarts4r(), "must pass e")

  expect_error(iris |> e_charts() |> e_flow_gl_(), "must pass y, sx, and sy")
  expect_error(e_flow_gl_() , "must pass e")
})

# e_lines_gl --------------------------------------------------------------
# TODO cant get function to display
# test_that("e_lines_gl plot has the good data structure and type", {
#   # Not sure on format of data so trying different things
#   # list( c(-74.006, 40.7128), c(2.3522, 48.8566))
#   # plot <-
#     e_charts() |>
#     e_lines_gl(data = data, lineStyle = list(width = 20))
#
#   expect_s3_class(plot, "echarts4r")
#   expect_s3_class(plot, "htmlwidget")
#
#   expect_equal(
#     plot$x$opts$series[[1]]$data,
#     list(
#       list(value = c(181.03, -20.62, 650.00)),
#       list(value = c(181.62, -20.42, 562.00)),
#       list(value = c(181.66, -17.97, 626.00)),
#       list(value = c(181.96, -20.42, 649.00)),
#       list(value = c(184.1, -26.0, 42.0))
#     )
#   )
#   expect_equal(
#     plot$x$opts$series[[1]]$type,
#     "linesGL"
#   )
# })

# test_that("e_lines_gl.echarts4rProxy plot responds", {
#
#   test_data <-  data.frame(
#     x = seq(3),
#     y = c(1, 3, 9),
#     z = c(2, 5, 4),
#     w = c(3, 4, 3)
#   )
#
#   server <- function(input, output, session) {
#     proxy_called <- shiny::reactiveVal(FALSE)
#     proxy_chart <- shiny::reactiveVal(NULL)
#
#     output$scatter <- renderEcharts4r({
#       test_data |>
#         e_charts(x) |>
#         e_lines_gl(y, z, name = "Serie 1")
#     })
#
#     observeEvent(input$update, {
#       chart <- echarts4rProxy("scatter",
#                               data = test_data) |>
#         e_lines_gl(y, y, name = "Serie 1") |>
#         e_execute()
#
#       proxy_chart(chart)
#       proxy_called(TRUE)
#     })
#   }
#
#   shiny::testServer(server, {
#     expect_false(proxy_called())
#     json <- jsonlite::fromJSON(output$scatter)
#
#     # Data matches - this is the first point
#     expect_identical(
#       json$x$opts$series$data[[1]]$value[[1]],
#       c(1L, 1L, 2L))
#
#     session$setInputs(update = 1)
#     session$flushReact()
#
#     # Update to line x
#     new_values <- proxy_chart()$chart$x$opts$series[[1]]$data |> unlist() |> unname()
#     expect_equal(new_values,c(1,1, 3,3,9, 9))
#
#     # Proxy was called with no errors
#     expect_true(proxy_called())
#
#     expect_equal(
#       proxy_chart()$chart$x$opts$series[[1]]$type,
#       "scatterGL"
#     )
#     expect_error(echarts4rProxy("scatterGL", data = test_data) |>
#                    e_lines_gl(), "must pass y and z")
#   })
# })
#
# test_that("e_lines_gl.echarts4r and e_lines_gl_ expects error when missing e and y,z", {
#   expect_error(iris |> e_charts() |> e_lines_gl.echarts4r(),
#                "missing data")
#   expect_error( e_lines_gl.echarts4r(),
#                 "must pass e")
#
#   expect_error(iris |> e_charts() |> e_lines_gl_(),
#                "missing data")
#   expect_error(e_lines_gl_(),
#                "must pass e")
# })
