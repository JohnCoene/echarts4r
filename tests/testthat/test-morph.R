test_that("e_line plot has the good data structure and type", {

  plot <- mtcars |> e_charts(cyl) |> e_line(mpg)
  plot2 <- mtcars |> e_charts(cyl) |> e_line(wt)

  cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 3000);
}"
  morph <- e_morph(plot, plot2, callback = cb) |> suppressWarnings()

  expect_s3_class(morph, "echarts4r")
  expect_s3_class(morph, "htmlwidget")

  expect_equal(
    plot$x$opts$series[[1]]$type,
    "line"
  )
})

test_that("e_line.echarts4r and e_line_ expects error when missing e or serie", {
  plot <- mtcars |> e_charts(cyl) |> e_line(mpg)

  cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 3000);
}"

  expect_warning(e_morph(plot, plot, callback = cb), "This is experimental")

  expect_error(e_morph(plot, callback = cb), "Must pass at least two charts") |> suppressWarnings()

  expect_error(e_morph(plot, plot), "Missing callback") |> suppressWarnings()

})
