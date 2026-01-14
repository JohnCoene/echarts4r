#' Add annotations to a chart
#'
#' @description
#' Each annotation requires an x, y, and text.
#'  In Shiny, to output an annotation position, use `input$annotation_positions`
#'  For rectStyle if x and y are ignored then line is connected on the horizontal bottom of the box.
#' For textStyle, rich text does work and will be applied to annotation text. textAlign and padding was also added.
#' For arrowStyle, size was added.
#' Make any "none" to remove element.
#' If color is provided in group, the text, box border, line and arrow will use that color.
#'
#' @details
#' - for textStyle list of [text.style](https://echarts.apache.org/en/option.html#graphic.elements-text.style)
#'
#' - list of [group](https://echarts.apache.org/en/option.html#graphic.elements-group)
#'
#' - rectStyle list of [rect.style](https://echarts.apache.org/en/option.html#graphic.elements-rect.style)
#'
#' - lineStyle SVG list for [lines](https://www.w3schools.com/graphics/svg_stroking.asp)
#'
#' - arrowStyle list of [polygon.style](https://echarts.apache.org/en/option.html#graphic.elements-polygon.style)
#'
#' @param e e
#' @param annotations list of annotation to plot

#'
#' @export
#'
#' @examples
#' mtcars |>
#' e_charts(mpg) |>
#'   e_scatter(wt) |>
#'   e_annotations(
#'     annotations = list(
#'       list(
#'         id = 0,
#'         x = 15,
#'         y = 3,
#'         text = 'An annotation\nwith styles',
#'         offsetX = 0,
#'         offsetY = -50,
#'         # Using the styles
#'         group = list(draggable = FALSE, color = "red"),
#'         textStyle = list(
#'           fontSize = 14,
#'           fontWeight = 'bold',
#'           color = "green"
#'         ),
#'         rectStyle = list(
#'           lineDash = 'solid',
#'           lineWidth = 2,
#'           shape = list(width = 105, height = 35, r = 0)
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
#'         offsetX = 0,
#'         offsetY = -40,
#'         color = "red",
#'         lineStyle = "none",
#'         rectStyle = "none",
#'         arrowStyle = "none"
#'       ),
#'       # Style text using rich text
#'       list(
#'         id = 2,
#'         x = 25,
#'         y = 4.5,
#'         text = "{bold|Text using}\n{red|rich text!}",
#'         offsetX = 0, # box position
#'         offsetY = -40,
#'         textStyle = list(
#'           rich = list(
#'             bold = list(fontWeight = 'bold'),
#'             red = list(fill = 'red')
#'           )
#'         ),
#'         rectStyle = list(
#'           lineDash = 'solid'
#'         )
#'       )
#'     ))
e_annotations <- function(
    e,
    annotations
) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }

  if (is.null(annotations)) {
    stop("must provide list of annotations")
  }

  if (!is.list(annotations) || !is.list(annotations[[1]])) {
    stop("annotations must be a list")
  }

  new_annos <- lapply(annotations, process_single_annotation)

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

  e |>
    htmlwidgets::onRender(
      paste0(
        "
    function(el, x, data) {
      var chart = echarts.getInstanceByDom(el);

      // Helper to set multiple SVG attributes
      function setAttrs(element, attrs) {
        for (var key in attrs) {
          if (attrs.hasOwnProperty(key) && attrs[key] != null) {
            element.setAttribute(key, attrs[key].toString());
          }
        }
      }

      // Load annotations from R
      var annotations = x.annotations || [];  // Get ALL annotations from e$x$annotations

      // Get or create SVG overlay
      var svg = el.querySelector('#annotation-svg-' + el.id);
      if (!svg) {
        svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.setAttribute('id', 'annotation-svg-' + el.id);
        svg.style.position = 'absolute';
        svg.style.top = '0';
        svg.style.left = '0';
        svg.style.width = '100%';
        svg.style.height = '100%';
        svg.style.pointerEvents = 'none';
        // svg.style.zIndex = z.toString();
        el.appendChild(svg);
      }

      // Initialize storage
       if (!window.annotationData) {
    window.annotationData = {};
       }

      if (!el._annotationData) {
          el._annotationData = {};
        }

      var graphics = [];
      var lines = [];

      function clearSvgLines() {
        while (svg.firstChild) {
          svg.removeChild(svg.firstChild);
        }
      }

    // function is here!
      function updateAnnotations() {
        clearSvgLines();
        graphics = [];
        lines = [];

        annotations.forEach(function(ann, index) {
          var anchorPos = chart.convertToPixel('grid', [ann.x, ann.y]);
      // Initialize annotation data",
        initialize_annotation_data(),
        " var annoData = el._annotationData[index];
         var boxPos = [
              anchorPos[0] + annoData.offsetX,
              anchorPos[1] + annoData.offsetY
            ];

          var arrowTip = ann.arrowTip;
              // SMART EDGE DETECTION
    var isAbove = boxPos[1] < anchorPos[1];
    var boxEdge;

    if (isAbove) {
      // Box is above anchor
      // Connect to BOTTOM edge of box, with gap going DOWN (positive)
      boxEdge = ann.rectShape.y + ann.rectShape.height ;
    } else {
      // Box is below anchor
      // Connect to TOP edge of box, with gap going UP (negative)
      boxEdge = ann.rectShape.y;
    }

      // SVG line
          var line = document.createElementNS('http://www.w3.org/2000/svg', 'line');

          setAttrs(line, {
              id: 'line_' + index,
              x1: anchorPos[0],
              y1: anchorPos[1] + arrowTip,
              x2: boxPos[0],
              y2: boxPos[1] + boxEdge
            });

          // Apply line style (annotation-specific or default)
           setAttrs(line, ann.lineStyle);

          svg.appendChild(line);

          lines.push({
            element: line,
            anchorPos: anchorPos,
            arrowTip: arrowTip,
            boxEdge: boxEdge,
            index: index
          });

          // Arrow point
          graphics.push({
              type: 'polygon',
              id: 'arrow_' + index,
              position: anchorPos,
              z: ann.group.z,
              shape: { points: ann.arrowVertices },
              style: ann.arrowStyle,
              silent: true
              });

           graphics.push({
              type: 'group',
             // coordinateSystem: 'cartesian2d',
              id: 'box_' + index,
              position: boxPos,
              ...ann.group,
              cursor: ann.draggable ? 'move' : 'default',
              children: [
                {
                  type: 'rect',
                  z: ann.group.z,
                  shape: ann.rectShape,
                  style: ann.rectStyle
                },
                {
                  type: 'text',
                  z: ann.group.z,
                  style: ann.textStyle
                }
              ]
            });
          });

          chart.setOption({ graphic: graphics });
        }

        // Initial render
        updateAnnotations();

        // Update on zoom/restore
        chart.on('dataZoom', updateAnnotations);
        chart.on('restore', updateAnnotations);

       // Setup drag handling",
        setup_drag_handler(),
        "
    }
  "
      )
    )
}

# Helper function to initialize annotation data
# this is the output of  input$id_dragged_annotation
initialize_annotation_data <- function() {
  "
   if (!el._annotationData[index]) {
          el._annotationData[index] = {
            row_index: index,
            box_id: 'box_' + index,
            offsetX: ann.offsetX,
            offsetY: ann.offsetY,
            text: ann.text,
            x: ann.x,
            y: ann.y,
            id: ann.id
          };
        }
  "
}

# Helper function to handle drag events
# cHECKS IF DRAGGED TARGET starts with box_ (annotation)
# takes box ID -> now index
setup_drag_handler <- function() {
  "
  chart.getZr().on('drag', function(e) {
    if (e.target &&
        e.target.id != null &&
        String(e.target.id).startsWith('box_')) {

      var index = parseInt(e.target.id.replace('box_', ''));
      var lineData = lines.find(l => l.index === index);

      if (lineData) {
        var boxPos = e.target.position;
        var ann = annotations[index];

        // SAME LOGIC as updateAnnotations
        var isAbove = boxPos[1] < lineData.anchorPos[1];
        var boxEdge;

        if (isAbove) {
          // Box above: connect to bottom edge
          boxEdge = ann.rectShape.y + ann.rectShape.height;
        } else {
          // Box below: connect to top edge
          boxEdge = ann.rectShape.y;
        }

        // Update line
        lineData.element.setAttribute('x2', boxPos[0]);
        lineData.element.setAttribute('y2', boxPos[1] + boxEdge);

        // Store offset
        el._annotationData[index].offsetX = boxPos[0] - lineData.anchorPos[0];
        el._annotationData[index].offsetY = boxPos[1] - lineData.anchorPos[1];

        // Update stored boxEdge for future drags
        lineData.boxEdge = boxEdge;
      }
    }
  });
  "
}


`%||%` <- function(a, b) if (is.null(a)) b else a

# R function with multiple positioning options
auto_text_style <- function(
    box_shape,
    position = "center", # center, top, bottom, left, right
    padding = 5
) {
  # Calculate base positions
  x_left <- box_shape$x + padding
  x_center <- box_shape$x + (box_shape$width / 2)
  x_right <- box_shape$x + box_shape$width - padding

  y_top <- box_shape$y + padding
  y_center <- box_shape$y + (box_shape$height / 2)
  y_bottom <- box_shape$y + box_shape$height - padding

  # Choose position
  pos <- switch(
    position,
    "center" = list(
      x = x_center,
      y = y_center,
      align = 'center',
      valign = 'middle'
    ),
    "top" = list(x = x_center, y = y_top, align = 'center', valign = 'top'),
    "bottom" = list(
      x = x_center,
      y = y_bottom,
      align = 'center',
      valign = 'bottom'
    ),
    "left" = list(x = x_left, y = y_center, align = 'left', valign = 'middle'),
    "right" = list(
      x = x_right,
      y = y_center,
      align = 'right',
      valign = 'middle'
    ),
    # default to center
    list(x = x_center, y = y_center, align = 'center', valign = 'middle')
  )
  pos
}


# Process a single annotation with all styles
process_single_annotation <- function(
    ann
) {
  # Required fields
  if (is.null(ann$x) || is.null(ann$y) || is.null(ann$text)) {
    stop("Annotation must have x, y, and text fields")
  }
  # Default Properties ------------------------------------------------------
  default_color <- '#738DE4'

  default_text_style <- list(
    fontSize = 11,
    fontWeight = 'bold',
    textAlign = ann$textStyle$textAlign %||% "left",
    padding = ann$textStyle$padding %||% 2
  )

  if(is.null(ann$lineStyle) || ann$lineStyle[[1]] != "none"){
    default_line_style <- list(
      `stroke-width` = 2,
      stroke = ann$group$color %||% default_color
    )
  } else {
    default_line_style <- list()
    ann$lineStyle <- NULL
  }

  if(is.null(ann$arrowStyle) || ann$arrowStyle[[1]] != "none"){
    default_arrow_style <- list(
      fill = ann$group$color %||% default_color,
      size = ann$arrowStyle$size %||% 8
    ) } else {
      default_arrow_style <- list(size = 0)
      ann$arrowStyle <- NULL
    }

  if(is.null(ann$rectStyle) || ann$rectStyle[[1]] != "none"){
    default_box_style <- list(
      stroke = ann$group$color %||% default_color,
      fill = '#ffffff',
      lineWidth = 2
    )} else {
      # Fully transparent
      default_box_style <- list(fill = "rgba(255, 255, 255, 0)")
      ann$rectStyle <- NULL
    }

  default_group <- list(draggable = TRUE, z = 10)
  default_box_width = 80
  default_box_height = 40
  default_box_radius = 3

  # Find box position -------------------------------------------------------

  box_width <- ann$rectStyle$shape$width %||% default_box_width
  box_height <- ann$rectStyle$shape$height %||% default_box_height
  box_radius <- ann$rectStyle$shape$r %||% default_box_radius
  box_x <- ann$rectStyle$shape$x %||% -box_width / 2
  box_y <- ann$rectStyle$shape$y %||% -box_height / 2

  # Box shape.
  # x and y make the line connect in the middle.
  box_shape <- list(
    x = box_x,
    y = box_y,
    width = box_width,
    height = box_height,
    r = box_radius
  )

  # Calculate text position (centered)
  text_pos <- auto_text_style(box_shape, position = default_text_style$textAlign, padding = default_text_style$padding)

  final_text_style <- modifyList(
    default_text_style,
    c(
      ann$textStyle %||% list(),
      list(
        x = text_pos$x,
        y = text_pos$y,
        text = as.character(ann$text),
        fill = ann$textStyle$color %||% default_color,
        textAlign = text_pos$align,
        textVerticalAlign = text_pos$valign
      )
    )
  )

  final_box_style <- modifyList(
    default_box_style,
    c(
      ann$rectStyle %||% list()
    )
  )

  final_line_style <- modifyList(
    default_line_style,
    c(
      ann$lineStyle %||% list()
    )
  )

  final_arrow_style <- modifyList(
    default_arrow_style,
    c(
      ann$arrowStyle %||% list()
    )
  )

  final_group <- modifyList(
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
