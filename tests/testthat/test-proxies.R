test_that("e_highlight_p errors informatively without echarts4rProxy object", {
  expect_error(e_highlight_p(list()), "must pass echarts4rProxy object")
  expect_error(e_highlight_p(mtcars), "must pass echarts4rProxy object")
})

test_that("e_highlight_p returns proxy object", {
  mock_session <- list(sendCustomMessage = function(...) {})
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )

  result <- e_highlight_p(proxy)
  expect_s3_class(result, "echarts4rProxy")
  expect_equal(result$id, "test_chart")
})

test_that("e_highlight_p adds series_index to data", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_highlight_p")
    expect_equal(data$seriesIndex, 0)
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )

  e_highlight_p(proxy, series_index = 0)
})

# test_that("e_highlight_p adds series_name to data", {
#   mock_session <- list(sendCustomMessage = function(type, data) {
#     expect_equal(type, "e_highlight_p")
#     expect_equal(data$seriesName, "HP")
#   })
#   proxy <- structure(
#     list(id = "test_chart", session = mock_session),
#     class = "echarts4rProxy"
#   )
#
#   e_highlight_p(proxy, series_name = "HP")
# })
#
# test_that("e_highlight_p adds both series_index and series_name when provided", {
#   mock_session <- list(sendCustomMessage = function(type, data) {
#     expect_equal(type, "e_highlight_p")
#     expect_equal(data$seriesIndex, 1)
#     expect_equal(data$seriesName, "disp")
#   })
#   proxy <- structure(
#     list(id = "test_chart", session = mock_session),
#     class = "echarts4rProxy"
#   )
#
#   e_highlight_p(proxy, series_index = 1, series_name = "disp")
# })
#
# test_that("e_highlight_p only includes id when no series specified", {
#   mock_session <- list(sendCustomMessage = function(type, data) {
#     expect_equal(type, "e_highlight_p")
#     expect_equal(data$id, "test_chart")
#     expect_null(data$seriesIndex)
#     expect_null(data$seriesName)
#   })
#   proxy <- structure(
#     list(id = "test_chart", session = mock_session),
#     class = "echarts4rProxy"
#   )
#
#   e_highlight_p(proxy)
# })
