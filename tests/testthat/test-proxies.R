test_that("e_highlight_p errors informatively without echarts4rProxy object", {
  expect_error(e_highlight_p(list()), "must pass echarts4rProxy object")
  expect_error(e_highlight_p(mtcars), "must pass echarts4rProxy object")
  expect_error(e_downplay_p(mtcars), "must pass echarts4rProxy object")
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

test_that("e_highlight_p adds series_name to data", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_highlight_p")
    expect_equal(data$seriesName, "HP")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )

  e_highlight_p(proxy, series_name = "HP")
})

test_that("e_highlight_p adds both series_index and series_name when provided", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_highlight_p")
    expect_equal(data$seriesIndex, 1)
    expect_equal(data$seriesName, "disp")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )

  e_highlight_p(proxy, series_index = 1, series_name = "disp")
})

test_that("e_downplay_p only includes id when no series specified", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_downplay_p")
    expect_equal(data$id, "test_chart")
    expect_null(data$seriesIndex)
    expect_null(data$seriesName)
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )
  e_downplay_p(proxy)
})

test_that("e_highlight_p adds both series_index and series_name when provided", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_highlight_p")
    expect_equal(data$seriesIndex, 1)
    expect_equal(data$seriesName, "disp")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )

  e_highlight_p(proxy, series_index = 1, series_name = "disp")
})

# e_showtip ---------------------------------------------------------------
test_that("e_showtip_p runs", {
  expect_error(e_showtip_p(mtcars), "must pass echarts4rProxy object")
})

test_that("e_showtip_p can send message", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_showtip_p")
    expect_equal(data$id, "test_chart")
    expect_equal(data$opts$type, "showTip")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )
  e_showtip_p(proxy)
})
# e_hidetip ---------------------------------------------------------------

test_that("e_hidetip_p runs", {
  expect_error(e_hidetip_p(mtcars), "must pass echarts4rProxy object")
})

test_that("e_hidetip_p can send message", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_hidetip_p")
    expect_equal(data$id, "test_chart")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )
  e_hidetip_p(proxy)
})

# e_dispatch --------------------------------------------------------------
test_that("e_dispatch_action_p runs", {
  expect_error(e_dispatch_action_p(), "must pass proxy and type")
  expect_error(e_dispatch_action_p(mtcars, "TYPE"), "must pass echarts4rProxy object")
})

test_that("e_dispatch_action_p can send message", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_dispatch_action_p")
    expect_equal(data$id, "test_chart")
    expect_equal(data$opts$type, "A TYPE")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )
  e_dispatch_action_p(proxy, type = "A TYPE")
})

# e_capture ---------------------------------------------------------------
test_that("e_capture can send capture", {
  e <- e_capture(e_charts(), "AN EVENT")
  expect_equal(e$x$capture, "AN EVENT")
})

# e_draw ------------------------------------------------------------------
test_that("e_draw_p errors", {
  expect_error(e_draw_p(), "must pass proxy and type")
  expect_error(e_draw_p(mtcars), "must pass echarts4rProxy object")
})

test_that("e_draw_p can send message", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_draw_p")
    expect_equal(data$id, "test_chart")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )
  e_draw_p(proxy)
})

# e_resize ----------------------------------------------------------------

test_that("e_resize can send message", {
  mock_session <- list(sendCustomMessage = function(type, data) {
    expect_equal(type, "e_resize")
    expect_equal(data$id, "test_chart")
  })
  proxy <- structure(
    list(id = "test_chart", session = mock_session),
    class = "echarts4rProxy"
  )
  e_resize(proxy)
})
