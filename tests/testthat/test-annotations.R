test_that("errors informatively without e", {
  expect_error(e_annotations(), "must pass e")
})

test_that("errors informatively without annotations", {
  expect_snapshot(mtcars |> e_charts(mpg) |> e_annotations(), error = TRUE)
})

test_that("errors informatively with non-list annotations", {
  expect_snapshot(
    mtcars |> e_charts(mpg) |> e_annotations("not a list"),
    error = TRUE
  )
  expect_snapshot(
    mtcars |> e_charts(mpg) |> e_annotations(list("not nested")),
    error = TRUE
  )
})

test_that("processes single annotation correctly", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  anno <- list(list(x = 15, y = 3, text = "Test"))
  result <- e_annotations(e, anno)
  expect_length(result$x$annotations, 1)
  expect_equal(result$x$annotations[[1]]$x, 15)
  expect_equal(result$x$annotations[[1]]$y, 3)
  expect_equal(result$x$annotations[[1]]$text, "Test")
})

test_that("appends to existing annotations", {
  e <- mtcars |> e_charts(mpg) |> e_scatter(wt)
  anno1 <- list(list(x = 15, y = 3, text = "First"))
  result1 <- e_annotations(e, anno1)
  anno2 <- list(list(x = 20, y = 4, text = "Second"))
  result2 <- e_annotations(result1, anno2)
  expect_length(result2$x$annotations, 2)
  expect_equal(result2$x$annotations[[1]]$text, "First")
  expect_equal(result2$x$annotations[[2]]$text, "Second")
})

test_that("styling works", {
  e <- mtcars |>
    e_charts(mpg) |>
    e_scatter(wt) |>
    e_annotations(
      default_color = "red",
      annotations = list(
        list(
          id = 0,
          x = 15,
          y = 3,
          text = 'An annotation',
          offsetX = 0,
          offsetY = -50,
          # Using the styles
          textStyle = list(
            fontSize = 14,
            fontWeight = 'bold'
          ),
          rectStyle = list(
            lineDash = 'dashed',
            lineWidth = 2,
            shape = list(width = 105, height = 35, r = 0)
          ),
          lineStyle = list(`stroke-width` = 2),
          arrowStyle = list(size = 12)
        )
      )
    )
  anno <- e$x$annotations[[1]]

  expect_equal(anno$draggable, TRUE)

  expect_equal(anno$textStyle$fontSize, 14)
  expect_equal(anno$textStyle$fontWeight, "bold")

  expect_equal(anno$rectStyle$lineDash, 'dashed')
  expect_equal(anno$rectStyle$lineWidth, 2)
  expect_equal(anno$rectStyle$shape$width, 105)

  expect_equal(anno$lineStyle$`stroke-width`, 2)

  expect_equal(anno$arrowStyle$size, 12)
})

test_that("`%||%` returns b when a is NULL", {
  expect_equal(NULL %||% "default", "default")
})

test_that("`%||%` returns a when a is not NULL", {
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
})

# find_text_position ---------------------------------------------------------
test_that("find_text_position returns middle position by default", {
  box_shape <- list(x = 0, y = 0, width = 100, height = 50)
  result <- find_text_position(box_shape)
  expect_equal(result$x, 50)
  expect_equal(result$y, 25)
})

test_that("find_text_position handles non-zero box origin", {
  box_shape <- list(x = -40, y = -25, width = 80, height = 50)
  result <- find_text_position(box_shape)
  expect_equal(result$x, 0)
  expect_equal(result$y, 0)
})

# process_single_annotation -----------------------------------------------
test_that("process_single_annotation errors informatively without required fields", {
  expect_snapshot(process_single_annotation(list(x = 1, y = 2)), error = TRUE)
  expect_snapshot(
    process_single_annotation(list(x = 1, text = "test")),
    error = TRUE
  )
  expect_snapshot(
    process_single_annotation(list(y = 2, text = "test")),
    error = TRUE
  )
})

test_that("process_single_annotation processes minimal annotation", {
  ann <- list(x = 15, y = 3, text = "Test")
  result <- process_single_annotation(ann)

  expect_equal(result$x, 15)
  expect_equal(result$y, 3)
  expect_equal(result$text, "Test")
  expect_equal(result$id, 0)
  expect_equal(result$offsetX, 0)
  expect_equal(result$offsetY, 0)
})

test_that("process_single_annotation uses default color", {
  ann <- list(x = 15, y = 3, text = "Test")
  result <- process_single_annotation(ann)

  expect_equal(result$textStyle$fill, "#738DE4")
  expect_equal(result$rectStyle$stroke, "#738DE4")
  expect_equal(result$lineStyle$stroke, "#738DE4")
  expect_equal(result$arrowStyle$fill, "#738DE4")
})

test_that("process_single_annotation uses default color", {
  ann <- list(x = 15, y = 3, text = "Test")
  result <- process_single_annotation(ann, default_color = "#000")

  expect_equal(result$textStyle$fill, "#000")
  expect_equal(result$rectStyle$stroke, "#000")
  expect_equal(result$lineStyle$stroke, "#000")
  expect_equal(result$arrowStyle$fill, "#000")
})

test_that("process_single_annotation handles lineStyle none", {
  ann <- list(x = 15, y = 3, text = "Test", lineStyle = "none")
  result <- process_single_annotation(ann)

  expect_equal(result$lineStyle, list())
})

test_that("process_single_annotation handles similar names", {
  ann <- list(x = 15, y = 3, text = "Test", lineStyle = list(stroke = "red", `stroke-opacity` = 0.3))
  result <- process_single_annotation(ann)

  expect_equal(result$lineStyle[["stroke"]], "red")
  expect_equal(result$lineStyle[["stroke-opacity"]], 0.3)
})

test_that("process_single_annotation handles arrowStyle none", {
  ann <- list(x = 15, y = 3, text = "Test", arrowStyle = "none")
  result <- process_single_annotation(ann)

  expect_equal(result$arrowStyle$size, 0)
})

test_that("process_single_annotation handles rectStyle none", {
  ann <- list(x = 15, y = 3, text = "Test", rectStyle = "none")
  result <- process_single_annotation(ann)

  expect_equal(result$rectStyle$fill, "rgba(255, 255, 255, 0)")
})


test_that("process_single_annotation handles incorrect padding_trbl", {
  ann <- list(x = 15, y = 3, text = "Test", textStyle = list(padding_trbl = "123"))
  expect_error(process_single_annotation(ann))

  ann <- list(x = 15, y = 3, text = "Test", textStyle = list(padding_trbl = list(1,2,3)))
  expect_error(process_single_annotation(ann))
})

test_that("process_single_annotation uses custom text style", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    textStyle = list(fontSize = 14, fontWeight = "bold", color = "green")
  )
  result <- process_single_annotation(ann)

  expect_equal(result$textStyle$fontSize, 14)
  expect_equal(result$textStyle$fontWeight, "bold")
  expect_equal(result$textStyle$fill, "green")
})

test_that("process_single_annotation uses custom box dimensions", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    rectStyle = list(shape = list(width = 105, height = 35, r = 0))
  )
  result <- process_single_annotation(ann)

  expect_equal(result$rectShape$width, 105)
  expect_equal(result$rectShape$height, 35)
  expect_equal(result$rectShape$r, 0)
})

test_that("process_single_annotation uses custom arrow size", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    arrowStyle = list(size = 12)
  )
  result <- process_single_annotation(ann)

  expect_equal(result$arrowStyle$size, 12)
  expect_equal(result$arrowTip, -12)
})

test_that("process_single_annotation uses custom offsets", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    offsetX = 10,
    offsetY = -50
  )
  result <- process_single_annotation(ann)

  expect_equal(result$offsetX, 10)
  expect_equal(result$offsetY, -50)
})

test_that("process_single_annotation uses custom id", {
  ann <- list(x = 15, y = 3, text = "Test", id = 5)
  result <- process_single_annotation(ann)

  expect_equal(result$id, 5)
})

test_that("process_single_annotation overwrites default options", {
  ann <- list(x = 15, y = 3, text = "Test")
  result <- process_single_annotation(ann, draggable = FALSE)

  expect_equal(result$draggable, FALSE)
})

test_that("process_single_annotation calculates arrow vertices", {
  ann <- list(x = 15, y = 3, text = "Test", arrowStyle = list(size = 8))
  result <- process_single_annotation(ann)

  expect_equal(result$arrowVertices[[1]], c(-4, -8))
  expect_equal(result$arrowVertices[[2]], c(4, -8))
  expect_equal(result$arrowVertices[[3]], c(0, 0))
})

test_that("process_single_annotation uses custom text align and padding", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    textStyle = list(textAlign = "middle", padding = 5)
  )
  result <- process_single_annotation(ann)

  expect_equal(result$textStyle$textAlign, "middle")
  expect_equal(result$textStyle$padding, 5)
})

test_that("process_single_annotation merges custom line style", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    lineStyle = list(`stroke-width` = 3, stroke = "blue")
  )
  result <- process_single_annotation(ann)

  expect_equal(result$lineStyle$`stroke-width`, 3)
  expect_equal(result$lineStyle$stroke, "blue")
})

test_that("process_single_annotation merges custom rect style", {
  ann <- list(
    x = 15,
    y = 3,
    text = "Test",
    rectStyle = list(lineDash = "dashed", lineWidth = 3)
  )
  result <- process_single_annotation(ann)

  expect_equal(result$rectStyle$lineDash, "dashed")
  expect_equal(result$rectStyle$lineWidth, 3)
})

test_that("if_style_is_not_none returns correct booleons", {
  style <- list(x = "A")
  style2 <- list(shape = list(width = 100))
  isNone <- list("none")

  expect_true(if_style_is_not_none(style$B))
  expect_true(if_style_is_not_none(style))
  expect_true(if_style_is_not_none(style2))

  expect_false(if_style_is_not_none(isNone))
})
