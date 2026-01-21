`%||%` <- function(a, b) if (is.null(a)) b else a

#' Add annotations to a chart
#'
#' @description
#'
#' Apache ECharts does not include a native annotation system. This is a custom
#' function that creates annotations using ECharts’ low-level graphic components
#' and SVG for the line. This currently only works in a 'cartesian2d' coordinate
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
#' - \strong{rectStyle} Styles the annotation box.
#'
#' - \strong{textStyle}, Styles the annotation text. textAlign and padding was added.
#'
#' - \strong{lineStyle} Styles the line that connects the annotation box to the arrow using \href{https://www.w3schools.com/graphics/svg_stroking.asp}{SVG stroke attributes}
#'
#' - \strong{arrowStyle}: Styles the arrow. size was added.
#'
#' @param e An echarts4r object
#' @param annotations list of annotations to plot
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
#'           "font-size" = 14,
#'           "font-weight" = 'bold',
#'           color = "green"
#'         ),
#'         rectStyle = list(
#'           lineDash = 'dashed',
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
#'         lineStyle = "none",
#'         rectStyle = "none",
#'         arrowStyle = "none",
#'         textStyle = list(color = "red")
#'       ),
#'       # Style text using rich text
#'       list(
#'         id = 2,
#'         x = 25,
#'         y = 4.5,
#'         text = "{bold|Text using}\n{red|rich text!}",
#'         offsetX = 0,
#'         offsetY = -40,
#'         textStyle = list(
#'           rich = list(
#'             bold = list(fontWeight = 'bold'),
#'             red = list(fill = 'red')
#'           )
#'         )
#'       )
#'     ))
#'
#' @seealso
#' - \href{https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute}{Additional arguments for SVG attributes}
#'
#' - \href{https://echarts.apache.org/en/option.html#graphic.elements-text.style}{Additional arguments for textStyle}
#'
#' - \href{https://www.w3schools.com/graphics/svg_stroking.asp}{Additional arguments for rectStyle}
#'
#' - \href{https://www.w3schools.com/graphics/svg_stroking.asp}{Additional
#' arguments for lineStyle}
#'
#' - \href{https://echarts.apache.org/en/option.html#graphic.elements-polygon.style}{Additional
#' arguments for arrowStyle}
#'
#' @rdname e_annotations
#' @export
e_annotations <- function(
    e,
    annotations,
    # facet_number = NULL,
    grid = NULL,
    series = NULL
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

    # cat("Series", series, "→ xAxisIndex", x_axis_idx, "→ gridIndex", grid_idx, "\n")

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

  # Adding this solves the issue for the SVG line to get messed up when
  # `shiny::fluidRow(echarts4r::echarts4rOutput("chart"))` with no e_tooltip().
  # I don't understand why this fixes it.
  if (!e$x$tl & is.null(e$x$opts$tooltip)) {
    e$x$opts$tooltip <-  list(trigger = 'fake')
  } else if ((e$x$tl & is.null(e$x$opts$tooltip)) ) {
    e$x$opts$baseOption$tooltip <-  list(trigger = 'fake')
  }

  e |>
    htmlwidgets::onRender(
      paste0(
        "
  function(el, x, data) {
    var chart = echarts.getInstanceByDom(el);

    function setAttrs(element, attrs) {
      for (var key in attrs) {
        if (attrs.hasOwnProperty(key) && attrs[key] != null) {
          element.setAttribute(key, attrs[key].toString());
        }
      }
    }

    var annotations = x.annotations || [];
    var svgs = {};
    var linesByGrid = {};
    var grids = [];

    function getOrCreateSVG(gridIndex) {
      var svgId = 'annotation-svg-' + el.id + '-grid-' + gridIndex;
      var svg = document.getElementById(svgId);

      if (!svg) {
        svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.setAttribute('id', svgId);
        svg.style.position = 'absolute';
        svg.style.pointerEvents = 'none';  // ← Changed back to 'none'
        svg.style.zIndex = '10';

        var clipPathId = 'clip-grid-' + gridIndex;
        var clipPath = document.createElementNS('http://www.w3.org/2000/svg', 'clipPath');
        clipPath.setAttribute('id', clipPathId);

        var clipRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        clipRect.setAttribute('x', '0');
        clipRect.setAttribute('y', '0');
        clipRect.setAttribute('width', '100%');
        clipRect.setAttribute('height', '100%');

        clipPath.appendChild(clipRect);
        svg.appendChild(clipPath);

        var group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        group.setAttribute('id', 'annotations-group-' + gridIndex);
        group.setAttribute('clip-path', 'url(#' + clipPathId + ')');
        group.style.pointerEvents = 'none';  // ← Also set group to none
        svg.appendChild(group);

        el.appendChild(svg);
      }

      return svg;
    }

    if (!el._annotationData) {
      el._annotationData = {};
    }

    function clearAllSvgLines() {
      Object.keys(svgs).forEach(function(gridIndex) {
        var svg = svgs[gridIndex];
        if (svg) {
          var group = svg.querySelector('#annotations-group-' + gridIndex);
          if (group) {
            while (group.firstChild) {
              group.removeChild(group.firstChild);
            }
          }
        }
      });
    }

    function updateAnnotations() {
      var option = chart.getOption();
      if (!option || !option.grid || option.grid.length === 0) {
        setTimeout(updateAnnotations, 100);
        return;
      }

      clearAllSvgLines();
      linesByGrid = {};
      grids = [];

      for (var i = 0; i < option.grid.length; i++) {
        var gridModel = chart.getModel().getComponent('grid', i);

        if (gridModel && gridModel.coordinateSystem) {
          var gridRect = gridModel.coordinateSystem.getRect();
          grids[i] = {
            x: gridRect.x,
            y: gridRect.y,
            width: gridRect.width,
            height: gridRect.height
          };

          var svg = getOrCreateSVG(i);
          svg.style.left = gridRect.x + 'px';
          svg.style.top = gridRect.y + 'px';
          svg.style.width = gridRect.width + 'px';
          svg.style.height = gridRect.height + 'px';

          svgs[i] = svg;
          linesByGrid[i] = [];
        }
      }

      annotations.forEach(function(ann, index) {
        var gridIndex = ann.gridIndex || 0;

        if (!grids[gridIndex]) {
          return;
        }

        var grid = grids[gridIndex];
        var svg = svgs[gridIndex];
        var group = svg.querySelector('#annotations-group-' + gridIndex);

        var containerPixel = chart.convertToPixel({gridIndex: gridIndex}, [ann.x, ann.y]);

        if (!containerPixel || containerPixel.length !== 2) {
          return;
        }

        var anchorPos = [
          containerPixel[0] - grid.x,
          containerPixel[1] - grid.y
        ];

        ",
        initialize_annotation_data(),
        "

        var annoData = el._annotationData[index];

        // Store box_id
        var boxId = 'box-group-' + index;
        annoData.box_id = boxId;

        var boxPos = [
          anchorPos[0] + annoData.offsetX,
          anchorPos[1] + annoData.offsetY
        ];

        var arrowTip = ann.arrowTip;
        var isAbove = boxPos[1] < anchorPos[1];
        var boxEdge = isAbove ?
          ann.rectShape.y + ann.rectShape.height :
          ann.rectShape.y;

        // SVG LINE
        var line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        setAttrs(line, {
          id: 'line_' + index,
          x1: anchorPos[0],
          y1: anchorPos[1] + arrowTip,
          x2: boxPos[0],
          y2: boxPos[1] + boxEdge
        });
        setAttrs(line, ann.lineStyle);
        group.appendChild(line);

        // SVG ARROW
        var arrow = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
        var arrowPointsStr = ann.arrowVertices.map(function(p) {
          return (anchorPos[0] + p[0]) + ',' + (anchorPos[1] + p[1]);
        }).join(' ');
        setAttrs(arrow, {
          id: 'arrow_' + index,
          points: arrowPointsStr,
        });
        setAttrs(arrow, ann.arrowStyle);
        group.appendChild(arrow);

        // DRAGGABLE GROUP
        var boxGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        boxGroup.setAttribute('id', 'box-group-' + index);
        boxGroup.setAttribute('data-index', index);
        boxGroup.setAttribute('data-grid', gridIndex);
        boxGroup.style.pointerEvents = 'none';  // ← Group is none by default",
        svg_add_shadow(),
        svg_HTML_to_tspan(),
        "
        // SVG RECT - THIS needs pointer-events: all
        var rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        setAttrs(rect, {
          id: 'box_' + index,
          x: ann.rectShape.x,
          y: ann.rectShape.y,
          width: ann.rectShape.width,
          height: ann.rectShape.height,
          rx: ann.rectShape.r
        });

        // split shadow from normal attrs
        var { shadow, ...rectStyle } = ann.rectStyle || {};
        setAttrs(rect, rectStyle);

        // apply shadow if present
        if (shadow) {
          var filterId = ensureShadowFilter(svg, shadow);
          rect.setAttribute('filter', `url(#${filterId})`);
        }

        // CRITICAL: Enable pointer events on rect ONLY
       // if (ann.draggable) {
          rect.style.cursor = 'move';
          rect.style.pointerEvents = 'all';  // ← Only the rect gets events
          rect.setAttribute('data-draggable', 'true');
          rect.setAttribute('data-index', index);
          rect.setAttribute('data-grid', gridIndex);
       // }

        boxGroup.appendChild(rect);

        // SVG TEXT
        var text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        setAttrs(text, {
          id: 'text_' + index,
          x: ann.textStyle.x,
          y: ann.textStyle.y
        });
        var tspans = htmlToTspans(ann.textStyle.text, ann.textStyle.x);
        tspans.forEach(t => text.appendChild(t));
        setAttrs(text, ann.textStyle);

        boxGroup.appendChild(text);

        // Set transform
        boxGroup.setAttribute('transform', 'translate(' + boxPos[0] + ',' + boxPos[1] + ')');

        group.appendChild(boxGroup);

        linesByGrid[gridIndex].push({
          line: line,
          arrow: arrow,
          boxGroup: boxGroup,
          rect: rect,
          anchorPos: anchorPos,
          arrowTip: arrowTip,
          index: index,
          gridIndex: gridIndex,
          ann: ann
        });
      });
    }

    setTimeout(updateAnnotations, 200);

    chart.on('dataZoom', updateAnnotations);
    chart.on('timelinechanged', updateAnnotations);
    chart.on('restore', updateAnnotations);

    // Resize observor
    if (!el._resizeHandlerAttached) {
      if (typeof ResizeObserver !== 'undefined') {
        var resizeObserver = new ResizeObserver(function(entries) {
          clearTimeout(el._resizeTimeout);
          el._resizeTimeout = setTimeout(function() {
            chart.resize();
            setTimeout(updateAnnotations, 150);
          }, 100);
        });
        resizeObserver.observe(el);
      }
      el._resizeHandlerAttached = true;
    }

    // DRAG STATE
    var isDragging = false;
    var currentDrag = null;

    // MOUSEDOWN on document
    document.addEventListener('mousedown', function(e) {
      console.log('Mousedown on:', e.target.tagName, e.target.id, 'draggable:', e.target.getAttribute('data-draggable'));

      // Check if clicking on a draggable rect
      if (e.target.tagName === 'rect' && e.target.getAttribute('data-draggable') === 'true') {
        var annIndex = parseInt(e.target.getAttribute('data-index'));
        var gridIdx = parseInt(e.target.getAttribute('data-grid'));

        console.log('✓ Starting drag - annotation:', annIndex, 'grid:', gridIdx);

        var lineData = linesByGrid[gridIdx].find(l => l.index === annIndex);
        if (!lineData) {
          console.warn('No line data found');
          return;
        }

        var svg = svgs[gridIdx];
        var svgRect = svg.getBoundingClientRect();
        var boxGroup = lineData.boxGroup;

        // Get current transform
        var transform = boxGroup.getAttribute('transform');
        var match = transform.match(/translate\\(([^,]+),([^)]+)\\)/);
        var currentX = parseFloat(match[1]);
        var currentY = parseFloat(match[2]);

        isDragging = true;
        currentDrag = {
          boxGroup: boxGroup,
          lineData: lineData,
          gridIndex: gridIdx,
          annIndex: annIndex,
          svg: svg,
          startX: e.clientX - svgRect.left - currentX,
          startY: e.clientY - svgRect.top - currentY
        };

        e.preventDefault();
        e.stopPropagation();
      }
    });

    // MOUSEMOVE
     document.addEventListener('mousemove', function(e) {
      if (!isDragging || !currentDrag) return;

      var svgRect = currentDrag.svg.getBoundingClientRect();
      var grid = grids[currentDrag.gridIndex];
      var ann = currentDrag.lineData.ann;

      // Calculate desired position
      var desiredX = e.clientX - svgRect.left - currentDrag.startX;
      var desiredY = e.clientY - svgRect.top - currentDrag.startY;

      // Calculate box boundaries
      var boxLeft = desiredX + ann.rectShape.x;
      var boxRight = desiredX + ann.rectShape.x + ann.rectShape.width;
      var boxTop = desiredY + ann.rectShape.y;
      var boxBottom = desiredY + ann.rectShape.y + ann.rectShape.height;

      // CONSTRAIN to grid bounds
      var newX = desiredX;
      var newY = desiredY;

      // Horizontal constraints
      if (boxLeft < 0) {
        newX = -ann.rectShape.x;
      } else if (boxRight > grid.width) {
        newX = grid.width - ann.rectShape.x - ann.rectShape.width;
      }

      // Vertical constraints
      if (boxTop < 0) {
        newY = -ann.rectShape.y;
      } else if (boxBottom > grid.height) {
        newY = grid.height - ann.rectShape.y - ann.rectShape.height;
      }

      // Update position
      currentDrag.boxGroup.setAttribute('transform', 'translate(' + newX + ',' + newY + ')');

      // Update line
      var isAbove = newY < currentDrag.lineData.anchorPos[1];
      var boxEdge = isAbove ?
        ann.rectShape.y + ann.rectShape.height :
        ann.rectShape.y;

      currentDrag.lineData.line.setAttribute('x2', newX);
      currentDrag.lineData.line.setAttribute('y2', newY + boxEdge);

      // Store constrained offset
      el._annotationData[currentDrag.annIndex].offsetX = newX - currentDrag.lineData.anchorPos[0];
      el._annotationData[currentDrag.annIndex].offsetY = newY - currentDrag.lineData.anchorPos[1];

      e.preventDefault();
    });

document.addEventListener('mouseup', function(e) {
  if (isDragging) {
    console.log('Drag ended, sending all annotation positions');

    if (typeof Shiny !== 'undefined') {
      // Send individual dragged annotation
      Shiny.onInputChange(
        el.id + '_dragged_annotation' + ':echarts4rParse',
        el._annotationData[currentDrag.annIndex]
      );

      // Also send all positions
      Shiny.setInputValue('annotation_positions', el._annotationData, {
        priority: 'event'
      });
    }

    isDragging = false;
    currentDrag = null;
  }
});

  }
"
      )
    )
}

# Helper function to initialize annotation data
# this is the output to input$id_dragged_annotation
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

svg_HTML_to_tspan <- function(){
  "
  const svgNS = 'http://www.w3.org/2000/svg';

function htmlToTspans(html, baseX) {
  const container = document.createElement('div');
  container.innerHTML = html;

  const tspans = [];
  let dy = 0;

  function walk(node, inheritedStyle = {}) {
    if (node.nodeType === Node.TEXT_NODE) {
      if (!node.textContent.trim()) return;

      const tspan = document.createElementNS(svgNS, 'tspan');
      tspan.textContent = node.textContent;

      Object.entries(inheritedStyle).forEach(([k, v]) =>
        tspan.setAttribute(k, v)
      );

      if (dy) {
        tspan.setAttribute('x', baseX);
        tspan.setAttribute('dy', dy);
        dy = 0;
      }

      tspans.push(tspan);
      return;
    }

    if (node.nodeType !== Node.ELEMENT_NODE) return;

    let style = { ...inheritedStyle };

    switch (node.tagName.toLowerCase()) {
      case 'b':
      case 'strong':
        style['font-weight'] = 'bold';
        break;
      case 'i':
      case 'em':
        style['font-style'] = 'italic';
        break;
      case 'u':
        style['text-decoration'] = 'underline';
        break;
      case 'br':
        dy = '1.2em';
        return;
      case 'span':
        if (node.style.color) style.fill = node.style.color;
        if (node.style.fontSize) style['font-size'] = node.style.fontSize;
        if (node.style.fontWeight) style['font-weight'] = node.style.fontWeight;
        if (node.style.fontStyle) style['font-style'] = node.style.fontStyle;
        break;
    }

    [...node.childNodes].forEach(child => walk(child, style));
  }

  [...container.childNodes].forEach(n => walk(n));
  return tspans;
}

"
}

svg_add_shadow <- function(){
  "
  function ensureShadowFilter(svg, shadow) {
  var defs = svg.querySelector('defs') ||
    svg.insertBefore(
      document.createElementNS('http://www.w3.org/2000/svg', 'defs'),
      svg.firstChild
    );

  // Create a stable id so identical shadows reuse filters
  var id = 'shadow_' + btoa(JSON.stringify(shadow)).replace(/=/g, '');

  if (svg.querySelector('#' + id)) {
    return id;
  }

  var filter = document.createElementNS('http://www.w3.org/2000/svg', 'filter');
  setAttrs(filter, {
    id: id,
    x: '-50%',
    y: '-50%',
    width: '200%',
    height: '200%'
  });

  var feDropShadow = document.createElementNS(
    'http://www.w3.org/2000/svg',
    'feDropShadow'
  );

  setAttrs(feDropShadow, {
    dx: shadow.dx ?? 0,
    dy: shadow.dy ?? 0,
    stdDeviation: shadow.blur ?? 0,
    'flood-color': shadow.color ?? '#000',
    'flood-opacity': shadow.opacity ?? 1
  });

  filter.appendChild(feDropShadow);
  defs.appendChild(filter);

  return id;
}"
}
# R function with multiple positioning options
#' @keywords internal
find_text_position <- function(
    box_shape,
    position = "middle", # middle, top, bottom, left, right
    padding = 5
) {
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
    padding = ann$textStyle[["padding"]] %||% 2
  )


  if(if_style_is_not_none(ann$lineStyle)){
    default_line_style <- list(
      `stroke-width` = 2,
      stroke = ann$lineStyle[["stroke"]] %||% default_color
    )
  } else {
    default_line_style <- list()
    ann$lineStyle <- NULL
  }

  if(if_style_is_not_none(ann$arrowStyle)){
    default_arrow_style <- list(
      fill = ann$arrowStyle[["fill"]] %||% default_color,
      size = ann$arrowStyle[["size"]] %||% 8
    ) } else {
      default_arrow_style <- list(size = 0)
      ann$arrowStyle <- NULL
    }

  if(if_style_is_not_none(ann$rectStyle)){
    default_box_style <- list(
      stroke = ann$rectStyle[["stroke"]] %||% default_color,
      fill = '#ffffff',
      "stroke-width" = 2
    )} else {
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

  # Calculate text position (centered)
  text_pos <- find_text_position(box_shape, position = default_text_style[["text-anchor"]], padding = default_text_style[["padding"]])

  final_text_style <- utils::modifyList(
    default_text_style,
    c(
      ann$textStyle %||% list(),
      list(
        x = text_pos$x,
        y = text_pos$y,
        text = as.character(ann$text),
        fill = ann$textStyle[["color"]] %||% default_color
        # "text-anchor" = text_pos$align,
        # "dominant-baseline" = text_pos$valign
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
if_style_is_not_none <- function(style){
  is.null(style) ||
    !(is.character(style[[1]]) && style[[1]] == "none")
}
