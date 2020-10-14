### Tests of the functions in the tab "Statistical"
### https://echarts4r.john-coene.com/articles/stats.html


test_that("e_band plot has the good data structure and type", {
  set.seed(1)
  
  df <- data.frame(
    x = 1:5,
    y = round(runif(5, 5, 10), 6)
  ) %>% 
    dplyr::mutate(
      lwr = round(y - runif(5, 1, 3), 6),
      upr = y + round(runif(5, 2, 4), 6)
    )
  
  plot <- df %>% 
    e_charts(x) %>% 
    e_line(y) %>% 
    e_band(lwr, upr)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_equal(
    plot$x$opts$series[[1]]$data, 
    list(list(value = c(1.000000, 6.327543)),
         list(value = c(2.000000, 6.860619)),
         list(value = c(3.000000, 7.864267)),
         list(value = c(4.000000, 9.541039)),
         list(value = c(5.00000, 6.00841)))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "line"
  )
  
})


test_that("e_correlations plot has the good data structure and type", {
  plot <- cor(mtcars[6:9, 1:3]) %>% 
    round(7) %>%
    e_charts() %>% 
    e_correlations(order = "hclust") %>% 
    e_tooltip()
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_equal(
    plot$x$opts$series[[1]]$data, 
    list(list(value = c("mpg", "mpg", " 1.0000000")),
         list(value = c("cyl", "mpg", "-0.9856266")),
         list(value = c("disp", "mpg", "-0.9618611")),
         list(value = c("mpg", "cyl", "-0.9856266")),
         list(value = c("cyl", "cyl", " 1.0000000")),
         list(value = c("disp", "cyl", " 0.9912808")),
         list(value = c("mpg", "disp", "-0.9618611")),
         list(value = c("cyl", "disp", " 0.9912808")),
         list(value = c("disp", "disp", " 1.0000000")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "heatmap"
  )
  
})

test_that("e_error_bar plot has the good data structure and type", {
  df <- data.frame(
    x = factor(c(1, 2)),
    y = c(1, 5),
    upper = c(1.1, 5.3),
    lower = c(0.8, 4.3)
  )
  
  plot <- df %>% 
    e_charts(x) %>% 
    e_bar(y) %>% 
    e_error_bar(lower, upper)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_equal(
    plot$x$opts$series[[1]]$data, 
    list(list(value = c("1", "1")),
         list(value = c("2", "5")))
  )
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "bar"
  )
  
})


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
  
  plot <- df %>% 
    group_by(grp) %>% 
    e_charts() %>% 
    e_boxplot(x)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  ### The decimals are problematic so instead, check that the differences between the two expressions tend towards zero
  
  # expect_equal(
  #   plot$x$opts$series[[1]]$data, 
  #   list(c(-2.214700, -0.510335, 0.113909, 0.693351, 2.401618),
  #        c(-4.583193, -2.199190, 0.671524, 4.607498, 9.723452),
  #        c(3.144293, 8.093827, 9.880848, 12.061566, 17.492985))
  # )
  
  # difference between the two expressions
  difference <- unlist(plot$x$opts$series[[1]]$data) - unlist(list(
    c(-2.214700,-0.510335, 0.113909, 0.693351, 2.401618),
    c(-4.583193, -2.199190, 0.671524, 4.607498, 9.723452),
    c(3.144293, 8.093827, 9.880848, 12.061566, 17.492985)))
  difference_test <- difference < 10^-5
  
  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "boxplot"
  )
  
})


test_that("e_histogram plot has the good data structure and type", {
  set.seed(1)
  df <- data.frame(
    x = 1:10,
    y = rnorm(10, 20, 12)
  )
  
  plot <- df %>% 
    e_charts() %>% 
    e_histogram(y) %>% 
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
  ))
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "bar"
  )
  
})



test_that("e_density plot has the good data structure and type", {
  set.seed(1)
  df <- data.frame(
    x = 1:10,
    y = rnorm(10, 20, 12)
  )
  
  plot <- df %>%
    e_charts() %>% 
    e_histogram(y) %>% 
    e_density(y, name = "density", areaStyle = list(opacity = .4), 
              smooth = TRUE, y_index = 1) %>% 
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
    ))
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "bar"
  )
  
})


test_that("e_lm plot has the good data structure and type", {
  plot <- iris[c(1:3, 101:103), ] %>% 
    group_by(Species) %>% 
    e_charts(Sepal.Length) %>% 
    e_line(Sepal.Width) %>% 
    e_lm(Sepal.Width ~ Sepal.Length) %>% 
    e_x_axis(min = 4)
  
  expect_s3_class(plot, "echarts4r")
  expect_s3_class(plot, "htmlwidget")
  
  expect_equal(
    plot$x$opts$series[[1]]$data,
    list(list(value = c(4.7, 3.2)),
         list(value = c(4.9, 3.0)),
         list(value = c(5.1, 3.5))
    ))
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "line"
  )
  
})



test_that("e_loess plot has the good data structure and type", {
  plot <- mtcars[1:7, ] %>% 
    e_charts(disp) %>% 
    e_scatter(mpg, qsec) %>% 
    e_loess(mpg ~ disp)
  
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
        160.000000, 21.000000, 16.460000,  3.689498
      )),
      list(value = c(
        160.000000, 21.000000, 17.020000,  6.118721
      )),
      list(value = c(225.00, 18.10, 20.22, 20.00)),
      
      list(value = c(258.00000, 21.40000, 19.44000, 16.61644)),
      list(value = c(
        360.000000, 18.700000, 17.020000,  6.118721
      )),
      list(value = c(360.00, 14.30, 15.84,  1.00))
    ))
  difference_test <- difference < 10^-5
  
  expect_true(unique(difference_test))
  expect_equal(
    plot$x$opts$series[[1]]$type, 
    "scatter"
  )
  
})
