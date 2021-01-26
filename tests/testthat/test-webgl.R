### Tests of the functions in the tab "Web-GL"
### https://echarts4r.john-coene.com/articles/gl.html



### Not possible to test the data as in other tests: subsetting to a 100 points displays almost nothing on the plot
test_that("e_surface plot has the good data structure and type", {
  data("volcano")

  surface <- as.data.frame(as.table(volcano))
  surface$Var1 <- as.numeric(surface$Var1)
  surface$Var2 <- as.numeric(surface$Var2)

  plot <- surface %>%
    e_charts(Var1) %>%
    e_surface(Var2, Freq) %>%
    e_visual_map(Freq)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_scatter_gl plot has the good data structure and type", {
  plot <- quakes[1:5, ] %>%
    e_charts(long) %>%
    e_geo(
      roam = TRUE,
      boundingCoords = list(
        c(185, -10),
        c(165, -40)
      )
    ) %>%
    e_scatter_gl(lat, depth) %>%
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


# couldn't visualize the plot so no possibility to check it's fine
test_that("e_graph_gl plot has the good data structure and type", {
  nodes <- data.frame(
    name = paste0(LETTERS, 1:300),
    value = rnorm(300, 10, 2),
    size = rnorm(300, 10, 2),
    grp = rep(c("grp1", "grp2", "grp3"), 100),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    source = sample(nodes$name, 400, replace = TRUE),
    target = sample(nodes$name, 400, replace = TRUE),
    stringsAsFactors = FALSE
  )

  plot <- e_charts() %>%
    e_graph_gl() %>%
    e_graph_nodes(nodes, name, value, size, grp) %>%
    e_graph_edges(edges, source, target)

  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_flow_gl plot has the good data structure and type", {
  set.seed(1)
  vectors <- expand.grid(x = -3:3, y = -3:3)
  mu <- 1
  vectors$sx <- vectors$y
  vectors$sy <- mu * (1 - vectors$x^2) * vectors$y - vectors$x
  vectors$color <- log10(runif(nrow(vectors), 1, 10))

  # increase the number to better visualize the plot
  plot <- vectors[1:10, ] %>%
    dplyr::mutate(color = round(color, 6)) %>%
    e_charts(x) %>%
    e_flow_gl(y, sx, sy, color) %>%
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
    ) %>%
    e_x_axis(
      splitLine = list(show = FALSE)
    ) %>%
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
})
