df <- data.frame(
  x = seq(50),
  y = rnorm(50, 10, 3),
  z = rnorm(50, 11, 2),
  w = rnorm(50, 9, 2)
)

test_that("e_line plot has the good classes", {
  plot <- df %>% 
    e_charts(x) %>% 
    e_line(z)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_area plot has the good classes", {
  plot <- df %>% 
    e_charts(x) %>% 
    e_area(w) 
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_bar plot has the good classes", {
  plot <- df %>% 
    e_charts(x) %>% 
    e_bar(y, name = "Serie 1")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_step plot has the good classes", {
  plot <- df %>% 
    e_charts(x) %>% 
    e_step(z, name = "Serie 2")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_scatter plot has the good classes", {
  plot <- df %>% 
    e_charts(x) %>% 
    e_scatter(y, z)
  
  plot2 <- df %>% 
    e_charts(x) %>% 
    e_scatter(y, z) %>% 
    e_visual_map(z, scale = e_scale)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})

test_that("e_effect_scatter plot has the good classes", {
  plot <- df %>% 
    e_charts(x) %>% 
    e_effect_scatter(y, z)
  
  plot2 <- df %>% 
    e_charts(x) %>% 
    e_effect_scatter(y, z) %>% 
    e_visual_map(z, scale = e_scale)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})

test_that("e_polar plot has the good classes", {
  plot <- plot <- df %>% 
    e_charts(x) %>% 
    e_polar()
  
  plot2 <- df %>% 
    e_charts(x) %>% 
    e_polar() %>% 
    e_angle_axis(x) %>% # angle = x
    e_radius_axis() %>% 
    e_bar(y, coord_system = "polar")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})

test_that("e_radius plot has the good classes", {
  plot <- df %>% 
    head(10) %>% 
    e_charts(x) %>% 
    e_radius_axis(x)
  
  plot2 <- df %>% 
    head(10) %>% 
    e_charts(x) %>% 
    e_radius_axis(x) %>% 
    e_bar(y, coord_system = "polar") %>% 
    e_scatter(z, coord_system = "polar")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})

test_that("e_candle plot has the good classes", {
  date <- c("2017-01-01", "2017-01-02", "2017-01-03", "2017-01-04", "2017-03-05", 
            "2017-01-06", "2017-01-07")
  
  stock <- data.frame(
    date = date,
    opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
    closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
    low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
    high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
  )
  
  plot <- stock %>% 
    e_charts(date) %>% 
    e_candle(opening, closing, low, high) %>% 
    e_y_axis(min = 190, max = 220)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_funnel plot has the good classes", {
  funnel <- data.frame(stage = c("View", "Click", "Purchase"), value = c(80, 30, 20))
  
  plot <- funnel %>% 
    e_charts() %>% 
    e_funnel(value, stage) %>% 
    e_title("Funnel")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_sankey plot has the good classes", {
  sankey <- data.frame(
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e"),
    value = ceiling(rnorm(5, 10, 1)),
    stringsAsFactors = FALSE
  )
  
  plot <- sankey %>% 
    e_charts() %>% 
    e_sankey(source, target, value) %>% 
    e_title("Sankey chart")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_heatmap plot has the good classes", {
  v <- LETTERS[1:10]
  matrix <- data.frame(
    x = sample(v, 300, replace = TRUE), 
    y = sample(v, 300, replace = TRUE), 
    z = rnorm(300, 10, 1),
    stringsAsFactors = FALSE
  ) %>% 
    dplyr::group_by(x, y) %>% 
    dplyr::summarise(z = sum(z)) %>% 
    dplyr::ungroup()

  plot <- matrix %>% 
    e_charts(x) %>% 
    e_heatmap(y, z) %>% 
    e_visual_map(z) %>% 
    e_title("Heatmap")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_parallel plot has the good classes", {
  df <- data.frame(
    price = rnorm(5, 10),
    amount = rnorm(5, 15),
    letter = LETTERS[1:5]
  )
  
  plot <- df %>% 
    e_charts() %>% 
    e_parallel(price, amount, letter) %>% 
    e_title("Parallel chart")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_pie plot has the good classes", {
  plot <- mtcars %>% 
    head() %>% 
    dplyr::mutate(model = row.names(.)) %>% 
    e_charts(model) %>% 
    e_pie(carb) %>% 
    e_title("Pie chart")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_donut plot has the good classes", {
  plot <- mtcars %>% 
    head() %>% 
    dplyr::mutate(model = row.names(.)) %>% 
    e_charts(model) %>% 
    e_title("Donut chart")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_rosetype plot has the good classes", {
  plot <- mtcars %>% 
    head() %>% 
    dplyr::mutate(model = row.names(.)) %>% 
    e_charts(model) %>% 
    e_pie(hp, roseType = "radius")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_sunburst plot has the good classes", {
  library(dplyr)
  
  df <- tibble(
    name = c("earth", "mars", "venus"), value = c(30, 40, 30),        # 1st level
    itemStyle = tibble(color = c(NA, 'red', 'blue')),
    children = list(
      tibble(name = c("land", "ocean"), value = c(10,20),             # 2nd level
             children = list(
               tibble(name = c("forest", "river"), value = c(3,7)),   # 3rd level 
               tibble(name = c("fish", "kelp"), value = c(10,5),
                      children = list(
                        tibble(name = c("shark", "tuna"), value = c(2,6)),  # 4th level 
                        NULL  # kelp
                      ))
             )),
      tibble(name = c("crater", "valley"), value = c(20,20)),
      NULL  # venus
    )
  )
  
  plot <- df %>% 
    e_charts() %>% 
    e_sunburst() %>% 
    e_title("Sunburst")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_tree plot has the good classes", {
  library(dplyr)
  
  tree <- tibble(
    name = "earth",        # 1st level
    children = list(
      tibble(name = c("land", "ocean"),             # 2nd level
             children = list(
               tibble(name = c("forest", "river")),   # 3rd level 
               tibble(name = c("fish", "kelp"),
                      children = list(
                        tibble(name = c("shark", "tuna"),  # 4th level 
                               NULL  # kelp
                        ))
               )
             ))
    )
  )
  
  plot <- tree %>% 
    e_charts() %>% 
    e_tree() %>% 
    e_title("Tree graph")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_treemap plot has the good classes", {
  library(dplyr)
  
  df <- tibble(
    name = c("earth", "mars", "venus"), value = c(30, 40, 30),        # 1st level
    itemStyle = tibble(color = c(NA, 'red', 'blue')),
    children = list(
      tibble(name = c("land", "ocean"), value = c(10,20),             # 2nd level
             children = list(
               tibble(name = c("forest", "river"), value = c(3,7)),   # 3rd level 
               tibble(name = c("fish", "kelp"), value = c(10,5),
                      children = list(
                        tibble(name = c("shark", "tuna"), value = c(2,6)),  # 4th level 
                        NULL  # kelp
                      ))
             )),
      tibble(name = c("crater", "valley"), value = c(20,20)),
      NULL  # venus
    )
  )
  
  plot <- df %>% 
    e_charts() %>% 
    e_treemap() %>% 
    e_title("Treemap chart")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_river plot has the good classes", {
  dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")
  
  river <- data.frame(
    dates = dates,
    apples = runif(length(dates)),
    bananas = runif(length(dates)),
    pears = runif(length(dates))
  )
  
  plot <- river %>% 
    e_charts(dates) %>% 
    e_river(apples) %>% 
    e_river(bananas) %>% 
    e_river(pears) %>% 
    e_tooltip(trigger = "axis") %>% 
    e_title("River charts", "(Streamgraphs)")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_calendar plot has the good classes", {
  dates <- seq.Date(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
  values <- rnorm(length(dates), 20, 6)
  
  year <- data.frame(date = dates, values = values)
  
  plot <- year %>% 
    e_charts(date) %>% 
    e_calendar(range = "2018") %>% 
    e_heatmap(values, coord_system = "calendar") %>% 
    e_visual_map(max = 30) %>% 
    e_title("Calendar", "Heatmap")
  
  plot2 <- year %>% 
    dplyr::mutate(year = format(date, "%Y")) %>% # get year from date
    group_by(year) %>% 
    e_charts(date) %>% 
    e_calendar(range = "2017",top="40") %>% 
    e_calendar(range = "2018",top="260") %>% 
    e_heatmap(values, coord_system = "calendar") %>% 
    e_visual_map(max = 30) %>% 
    e_title("Calendar", "Heatmap")%>%
    e_tooltip("item") 
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})

test_that("e_gauge plot has the good classes", {
  plot <- e_charts() %>% 
    e_gauge(41, "PERCENT") %>% 
    e_title("Gauge")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_radar plot has the good classes", {
  df <- data.frame(
    x = LETTERS[1:5],
    y = runif(5, 1, 5),
    z = runif(5, 3, 7)
  )
  
  plot <- df %>% 
    e_charts(x) %>% 
    e_radar(y, max = 7, name = "radar") %>%
    e_radar(z, max = 7, name = "chart") %>%
    e_tooltip(trigger = "item")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})

test_that("e_cloud plot has the good classes", {
  words <- function(n = 5000) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }
  
  tf <- data.frame(terms = words(100), 
                   freq = rnorm(100, 55, 10)) %>% 
    dplyr::arrange(-freq)
  
  plot <- tf %>% 
    e_color_range(freq, color) %>% 
    e_charts() %>% 
    e_cloud(terms, freq, color, shape = "circle", sizeRange = c(3, 15)) %>% 
    e_title("Wordcloud", "Random strings")
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_liquid plot has the good classes", {
  liquid <- data.frame(val = c(0.6, 0.5, 0.4))
  
  plot <- liquid %>% 
    e_charts() %>% 
    e_liquid(val) 
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})


test_that("e_surface plot has the good classes", {
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


test_that("e_scatter_gl plot has the good classes", {
  plot <- quakes %>% 
    e_charts(long) %>% 
    e_geo(
      roam = TRUE,
      boundingCoords = list(
        c(185, - 10),
        c(165, -40)
      )
    ) %>% 
    e_scatter_gl(lat, depth) %>% 
    e_visual_map()
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
})



test_that("e_graph_gl plot has the good classes", {
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


test_that("e_flow_gl plot has the good classes", {
  vectors <- expand.grid(x = -3:3, y = -3:3)
  mu <- 1
  vectors$sx <- vectors$y
  vectors$sy <- mu * (1 - vectors$x^2) * vectors$y - vectors$x
  vectors$color <- log10(runif(nrow(vectors), 1, 10))
  
  plot <- vectors %>% 
    e_charts(x) %>% 
    e_flow_gl(y, sx, sy, color) %>% 
    e_visual_map(
      min = 0, max = 1, # log 10
      dimension = 4, # x = 0, y = 1, sx = 3, sy = 4
      show = FALSE, # hide
      inRange = list(
        color = c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8',
                  '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026')
      )
    ) %>% 
    e_x_axis(
      splitLine = list(show = FALSE)
    ) %>% 
    e_y_axis(
      splitLine = list(show = FALSE)
    ) 
  
  latlong <- seq(-180, 180, by = 5)
  wind = expand.grid(lng = latlong, lat = latlong)
  wind$slng <- rnorm(nrow(wind), 0, 200)
  wind$slat <- rnorm(nrow(wind), 0, 200)
  wind$color <- abs(wind$slat) - abs(wind$slng)
  
  trans <- list(opacity = 0.5) # transparency
  
  plot2 <- wind %>% 
    e_charts(lng, backgroundColor = '#333') %>% 
    e_geo(
      itemStyle = list(
        normal = list(
          areaColor = "#323c48",
          borderColor = "#111"
        )
      )
    ) %>% 
    e_flow_gl(lat, slng, slat, color, 
              coord_system = "geo", 
              itemStyle = trans,
              particleSize = 2
    ) %>% 
    e_visual_map(
      color, # range
      dimension = 4, # lng = 0, lat = 1, slng = 2, slat = 3, color = 4
      show = FALSE, # hide
      inRange = list(
        color = c('#313695', '#4575b4', '#74add1', '#abd9e9', '#e0f3f8', 
                  '#ffffbf', '#fee090', '#fdae61', '#f46d43', '#d73027', '#a50026')
      )
    )
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_s3_class(plot2, "echarts4r")
  expect_s3_class(plot2, "htmlwidget")
})









  
 
