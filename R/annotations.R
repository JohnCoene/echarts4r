`%||%` <- function(a, b) if (is.null(a)) b else a

#' Add annotations to a chart
#'
#' @description
#'
#' Apache ECharts does not include a native annotation system. This is a custom
#' function that creates annotations using SVG. This currently only works in a 'cartesian2d' coordinate
#' system.
#'
#' Each annotation must be in a list with an x, y, and text. Styling can be
#' added - see @details.
#'
#' In Shiny, to output an annotation position after dragging the box, use
#' \code{input$id_dragged_annotation} or see \link{echarts4r-shiny}. This
#' captures the annotation parameters - not any of the styles.
#'
#' @details annotations can take the following styles to change the defaults. To
#'   remove any element use "none".
#'
#' - \strong{group}: Controls the box and text elements. color was added as an option. This color colors the text, box border, line and arrow - unless specified in that particular style argument.
#'
#' - \strong{rectStyle} Styles the annotation box using SVG. It also takes \strong{shape} that uses \hrek{https://echarts.apache.org/en/option.html#graphic.elements-rect.shape}{echarts graphic.elements-rect.shape}.
#'
#' - \strong{textStyle}, Styles the annotation text. This support limited HTML tags using tspan. SUpported tags are: \code{b, strong, i, em, u, br, span}. Styling inside \code{span} can use \code{color, fontSize, fontWeight, fontStyle}.
#'
#' - \strong{lineStyle} Styles the line that connects the annotation box to the arrow using \href{https://www.w3schools.com/graphics/svg_stroking.asp}{SVG stroke attributes}
#'
#' - \strong{arrowStyle}: Styles the arrow. size was added.
#'
#' @param e An echarts4r object
#' @param annotations list of annotations to plot
#' @inheritParams e_bar
#'
#' @examples
#' mtcars |>
#'   e_charts(mpg) |>
#'   e_scatter(wt) |>
#'   e_annotations(
#'     annotations = list(
#'       list(
#'         id = 0,
#'         x = 15,
#'         y = 3,
#'         text = 'An annotation<br>with styles',
#'         offsetX = 0,
#'         offsetY = -50,
#'         # Using the styles
#'         group = list(draggable = FALSE, color = "red"),
#'         textStyle = list(
#'           "font-size" = 14,
#'           "font-weight" = 'bold',
#'           color = "green"
#'         ),
#'         rectStyle = list(
#'           `stroke-dasharray` = c(35, 10),
#'           `stroke-width` = 2,
#'               # Shadow can be added!
#'                shadow = list(
#'                  dx= 0,
#'                  dy= 2,
#'                  blur= 4,
#'                  color= '#000',
#'                  opacity= 0.3
#'                ),
#'           shape = list(width = 105, height = 50, r = 0)
#'         ),
#'         lineStyle = list(`stroke-width` = 2),
#'         arrowStyle = list(size = 8)
#'       ),
#'       # No styles, will just show text
#'       list(
#'         id = 0,
#'         x = 25,
#'         y = 1,
#'         text = 'No styles',
#'         lineStyle = "none",
#'         rectStyle = "none",
#'         arrowStyle = "none"
#'       ),
#'       # Style text using rich text
#'       list(
#'         id = 2,
#'         x = 25,
#'         y = 4.5,
#'         text = "Default style",
#'         offsetX = 0,
#'         offsetY = -40
#'       )
#'     ))
#' @seealso
#' - \href{https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute}{Additional arguments for SVG attributes}
#'
#' - \href{https://www.w3schools.com/graphics/svg_text.asp}{Additional arguments for textStyle}
#'
#' - \href{https://www.w3schools.com/graphics/svg_stroking.asp}{Additional arguments for rectStyle}
#'
#' - \href{https://www.w3schools.com/graphics/svg_stroking.asp}{Additional
#' arguments for lineStyle}
#'
#' - \href{https://www.w3schools.com/graphics/svg_fill.asp}{Additional
#' arguments for arrowStyle}
#'
#' @rdname e_annotations
#' @export
e_annotations <- function(
  e,
  annotations,
  # facet_number = NULL,
  grid = NULL,
  series = NULL,
  legend = TRUE,
  name = "Annotations",
  legend_color = "#333333"
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (missing(annotations)) {
    stop("must provide list of annotations")
  }

  if (!is.list(annotations) || !is.list(annotations[[1]])) {
    stop("annotations must be a list")
  }

  # Determine grid index
  if (!is.null(series)) {
    # Convert R's 1-based series index to 0-based
    series_idx <- series - 1

    # Check if series exists
    if (is.null(e$x$opts$series[[series]])) {
      stop(paste("Series", series, "not found"))
    }

    # Get grid index from series
    # Series are mapped to grids via xAxisIndex/yAxisIndex
    series_obj <- e$x$opts$series[[series]]

    # Get xAxisIndex (defaults to 0)
    x_axis_idx <- series_obj$xAxisIndex %||% 0

    # Get which grid this xAxis is on
    if (!is.null(e$x$opts$xAxis)) {
      if (is.list(e$x$opts$xAxis) && length(e$x$opts$xAxis) > x_axis_idx + 1) {
        grid_idx <- e$x$opts$xAxis[[x_axis_idx + 1]]$gridIndex %||% x_axis_idx
      } else {
        grid_idx <- x_axis_idx
      }
    } else {
      grid_idx <- 0
    }
  } else if (!is.null(grid)) {
    # Grid specified directly (convert from R's 1-based to 0-based)
    grid_idx <- grid - 1
  } else {
    # Default to first grid
    grid_idx <- 0
  }

  new_annos <- lapply(annotations, function(ann) {
    processed <- process_single_annotation(ann)
    processed$gridIndex <- ann$gridIndex %||% grid_idx
    processed$legend_name <- name
    processed
  })

  # Get existing annotations from chart object
  existing_annos <- e$x$annotations %||% list()

  # Add new annotation to list
  if (is.null(existing_annos) || length(existing_annos) == 0) {
    all_annos <- new_annos
  } else {
    all_annos <- existing_annos
    for (new_anno in new_annos) {
      all_annos[[length(all_annos) + 1]] <- new_anno
    }
  }

  # Store back in chart object
  e$x$annotations <- all_annos

  # ADD DUMMY SERIES FOR LEGEND
  if (legend) {
    # Extract unique legend groups
    legend_info <- list()

    for (i in seq_along(all_annos)) {
      ann <- all_annos[[i]]

      # Get color
      color <- legend_color

      # Get grid
      grid_idx <- if (!is.null(ann$gridIndex)) {
        ann$gridIndex
      } else {
        0
      }
      # Store first occurrence
      if (is.null(legend_info[[name]])) {
        legend_info[[name]] <- list(
          color = color,
          gridIndex = grid_idx
        )
      }
    }

    # Add a dummy series for each unique legend group
    if (is.null(e$x$opts$series)) {
      e$x$opts$series <- list()
    }

    for (group_name in names(legend_info)) {
      info <- legend_info[[group_name]]

      dummy_series <- list(
        type = "scatter",
        name = group_name,
        data = list(), # Empty data
        symbol = "rect",
        symbolSize = 8,
        itemStyle = list(
          color = info$color
        ),
        gridIndex = info$gridIndex,
        xAxisIndex = info$gridIndex,
        yAxisIndex = info$gridIndex,
        tooltip = list(show = FALSE),
        silent = TRUE,
        animation = FALSE,
        isAnnotationLegend = TRUE
      )

      if (isTRUE(e$x$tl)) {
        e$x$opts$baseOption$series[[
          length(e$x$opts$baseOption$series) + 1
        ]] <- dummy_series
      } else {
        # Append to series
        e$x$opts$series[[length(e$x$opts$series) + 1]] <- dummy_series
      }
    }
  }

  # Adding this solves the issue for the SVG line to get messed up when
  # `shiny::fluidRow(echarts4r::echarts4rOutput("chart"))` with no e_tooltip().
  # I don't understand why this fixes it.
  if (!e$x$tl & is.null(e$x$opts$tooltip)) {
    e$x$opts$tooltip <- list(trigger = 'fake')
  } else if ((e$x$tl & is.null(e$x$opts$tooltip))) {
    e$x$opts$baseOption$tooltip <- list(trigger = 'fake')
  }

  annotation_dependency <- function() {
    htmltools::htmlDependency(
      name = "echarts-annotations",
      version = "1.0.0",
      src = system.file("htmlwidgets/lib/echarts-6.0.0/plugins/", package = "echarts4r"),
      script = "echarts-annotations.js"
      )
  }
  e$dependencies <- append(e$dependencies, list(annotation_dependency()))

  e |>
    htmlwidgets::onRender(
      "function(el, x, data) {
      function initWhenReady() {
        if (typeof EChartsAnnotations === 'undefined') {
          setTimeout(initWhenReady, 50);
          return;
        }

        EChartsAnnotations.initialize(el, x, data);
      }

      initWhenReady();
    }"
    )

}
# R function with multiple positioning options
#' @keywords internal
find_text_position <- function(
  box_shape,
  position = "right", # middle, top, bottom, left, right
  padding = 5
) {
  padding = 0
  # Calculate base positions
  x_left <- box_shape$x + padding
  x_middle <- box_shape$x + (box_shape$width / 2)
  # x_right <- box_shape$width- padding
  x_right <- box_shape$x + box_shape$width - padding

  y_top <- box_shape$y + padding
  y_middle <- box_shape$y + (box_shape$height / 2)
  y_bottom <- box_shape$y + box_shape$height - padding

  # Choose position
  pos <- switch(
    position,
    "middle" = list(
      x = x_middle,
      y = y_middle,
      align = 'middle',
      valign = 'middle'
    ),
    "top" = list(x = x_middle, y = y_top, align = 'middle', valign = 'top'),
    "bottom" = list(
      x = x_middle,
      y = y_bottom,
      align = 'middle',
      valign = 'bottom'
    ),
    "left" = list(x = x_left, y = y_middle, align = 'left', valign = 'middle'),
    "right" = list(
      x = x_right,
      y = y_middle,
      align = 'right',
      valign = 'middle'
    ),
    # default to middle
    list(x = x_middle, y = y_middle, align = 'middle', valign = 'middle')
  )
  pos
}

# Process a single annotation with all styles so js can read it.
#' @keywords internal
process_single_annotation <- function(
  ann
) {
  # Required fields
  if (is.null(ann$x) || is.null(ann$y) || is.null(ann$text)) {
    stop("Annotation must have x, y, and text fields")
  }

  # Default Properties ------------------------------------------------------
  # For these, better to use ann$arrowStyle[["fill"]] evaluation as opposed to ann$arrowStyle$fill so it does do a partial match.

  a_color <- '#738DE4'
  default_color <- ann$group$color %||% a_color

  default_text_style <- list(
    "font-size" = 11,
    "font-weight" = 'bold',
    "text-anchor" = ann$textStyle[["text-anchor"]] %||% "left",
    "dominant-baseline" = ann$textStyle[["dominant-baseline"]] %||% "middle"
    # padding = ann$textStyle[["padding"]] %||% 2
  )

  if (if_style_is_not_none(ann$lineStyle)) {
    default_line_style <- list(
      `stroke-width` = 2,
      stroke = ann$lineStyle[["stroke"]] %||% default_color
    )
  } else {
    default_line_style <- list()
    ann$lineStyle <- NULL
  }

  if (if_style_is_not_none(ann$arrowStyle)) {
    default_arrow_style <- list(
      fill = ann$arrowStyle[["fill"]] %||% default_color,
      size = ann$arrowStyle[["size"]] %||% 8
    )
  } else {
    default_arrow_style <- list(size = 0)
    ann$arrowStyle <- NULL
  }

  if (if_style_is_not_none(ann$rectStyle)) {
    default_box_style <- list(
      stroke = ann$rectStyle[["stroke"]] %||% default_color,
      fill = '#ffffff',
      "stroke-width" = 2
    )
  } else {
    # Fully transparent
    default_box_style <- list(fill = "rgba(255, 255, 255, 0)")
    ann$rectStyle <- NULL
  }

  default_group <- list(draggable = TRUE, z = 10)
  default_box_width <- 80
  default_box_height <- 40
  default_box_radius <- 3

  # Find box position -------------------------------------------------------

  # None of these should ever be NA
  box_width <- ann$rectStyle$shape[["width"]] %||% default_box_width
  box_height <- ann$rectStyle$shape[["height"]] %||% default_box_height
  box_radius <- ann$rectStyle$shape[["r"]] %||% default_box_radius
  box_x <- ann$rectStyle$shape[["x"]] %||% -box_width / 2
  box_y <- ann$rectStyle$shape[["y"]] %||% -box_height / 2

  # Box shape.
  # x and y make the line connect in the middle.
  box_shape <- list(
    x = box_x,
    y = box_y,
    width = box_width,
    height = box_height,
    r = box_radius
  )

  # TODO edit this - only need x / y
  # Calculate text position (centered)
  text_pos <- find_text_position(
    box_shape,
    position = default_text_style[["text-anchor"]],
    padding = default_text_style[["padding"]]
  )
  print(text_pos)
  final_text_style <- utils::modifyList(
    default_text_style,
    c(
      ann$textStyle %||% list(),
      list(
        x = text_pos$x,
        y = text_pos$y,
        text = as.character(ann$text),
        fill = ann$textStyle[["color"]] %||% default_color
      )
    )
  )

  final_box_style <- utils::modifyList(
    default_box_style,
    c(
      ann$rectStyle %||% list()
    )
  )

  final_line_style <- utils::modifyList(
    default_line_style,
    c(
      ann$lineStyle %||% list()
    )
  )

  final_arrow_style <- utils::modifyList(
    default_arrow_style,
    c(
      ann$arrowStyle %||% list()
    )
  )

  final_group <- utils::modifyList(
    default_group,
    c(
      ann$group %||% list()
    )
  )

  arrow_size <- final_arrow_style$size

  arrow_vertices <- list(
    c(-arrow_size / 2, -arrow_size),
    c(arrow_size / 2, -arrow_size),
    c(0, 0)
  )

  list(
    id = ann$id %||% 0,
    x = ann$x,
    y = ann$y,
    text = ann$text,
    offsetX = ann$offsetX %||% 0,
    offsetY = ann$offsetY %||% 0,
    rectShape = box_shape,
    rectStyle = final_box_style,
    textStyle = final_text_style,
    lineStyle = final_line_style,
    arrowStyle = final_arrow_style,
    arrowVertices = arrow_vertices,
    arrowTip = -arrow_size,
    group = final_group
  )
}

#' Determine if first element is none, then no style
#'The first element could be NULL, a style, or a vector so we must account for all these.
#' style = NULL
#' style = list(lineWidth = 5)
#' style = list(shape = list(width = 100))
#' @keywords internal
if_style_is_not_none <- function(style) {
  is.null(style) ||
    !(is.character(style[[1]]) && style[[1]] == "none")
}
