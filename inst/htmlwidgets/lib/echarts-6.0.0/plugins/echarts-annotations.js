(function() {
  window.EChartsAnnotations = {

  // FUNCTIONS ///
    // Helper function to set R attributes from a list into JS
    setAttrs: function(element, attrs) {
      for (var key in attrs) {
        if (attrs.hasOwnProperty(key) && attrs[key] != null) {
          element.setAttribute(key, attrs[key].toString());
        }
      }
    },

// Used to create each SVG element - line, rect, polygon, text and set their respective attributes (i.e. ann.lineStyle)
    createSVGElement: function(type, attrs) {
      var el = document.createElementNS('http://www.w3.org/2000/svg', type);
      if (attrs) this.setAttrs(el, attrs);
      return el;
    },

    // Remove anno groups. Clears based on what's actually in the DOM under
    // `el`, NOT a passed-in svgs map — on a Shiny re-render `initialize` runs
    // again with a fresh empty svgs map, but the SVG groups from the previous
    // render persist in the DOM; clearing the map would miss them and new
    // elements would stack on top of stale ones.
    clearAllSvgLines: function(el) {
      var groups = el.querySelectorAll('[id^="annotations-group-"]');
      groups.forEach(function(group) {
        while (group.firstChild) {
          group.removeChild(group.firstChild);
        }
      });
    },

  // Uses a grid system so annos are bounded by the grid
    getOrCreateSVG: function(el, gridIndex) {
      var svgId = 'annotation-svg-' + el.id + '-grid-' + gridIndex;
      var svg = document.getElementById(svgId);

      if (!svg) {
        // el must be the containing block for the absolutely-positioned
        // overlay; in Shiny the output div is position:static by default
        if (getComputedStyle(el).position === 'static') {
          el.style.position = 'relative';
        }

        svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.setAttribute('id', svgId);
        svg.setAttribute('aria-hidden', 'true');
        svg.style.position = 'absolute';
        svg.style.pointerEvents = 'none';
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
        group.style.pointerEvents = 'none';
        svg.appendChild(group);
        el.appendChild(svg);
      }

      return svg;
    },

    // Apply HTML to text using tspan
    htmlToTspans: function(html, baseX) {
      const container = document.createElement('div');
      const svgNS = 'http://www.w3.org/2000/svg';
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

        // The HTML tags that are supported
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
    },

    // Add shadow to annotation box
     ensureShadowFilter: function(svg, shadow) {
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
      this.setAttrs(filter, {
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

      this.setAttrs(feDropShadow, {
        dx: shadow.dx ?? 0,
        dy: shadow.dy ?? 0,
        stdDeviation: shadow.blur ?? 0,
        'flood-color': shadow.color ?? '#000',
        'flood-opacity': shadow.opacity ?? 1
      });

      filter.appendChild(feDropShadow);
      defs.appendChild(filter);

      return id;
    },

    // Keep the annotation box inside the grid rect (render-time and drag-time)
    clampBoxToGrid: function(x, y, annotation, grid) {
      var s = annotation.rectShape;
      if (x + s.x < 0) x = -s.x;
      else if (x + s.x + s.width > grid.width) x = grid.width - s.x - s.width;
      if (y + s.y < 0) y = -s.y;
      else if (y + s.y + s.height > grid.height) y = grid.height - s.y - s.height;
      return [x, y];
    },

    calculateAnnotationPosition: function (el, annotation, chart, index, grids, gridIndex) {
      var grid = grids[gridIndex];
      var containerPixel = chart.convertToPixel(
        {gridIndex: gridIndex},
        [annotation.x, annotation.y]
      );

      if (!containerPixel || containerPixel.length !== 2) {
        return null;
      }

      // anchor is outside the current zoom window — don't render;
      // it comes back automatically when the window includes it again
      if (!chart.containPixel({gridIndex: gridIndex}, containerPixel)) {
        return null;
      }

      var anchorPos = [
        containerPixel[0] - grid.x,
        containerPixel[1] - grid.y
      ];

       // This is what gets returned to Shiny in the input handler.
      if (!el._annotationData[index]) {
        el._annotationData[index] = {
          row_index: index,
          box_id: 'box_' + index,
          offsetX: annotation.offsetX || 0,
          offsetY: annotation.offsetY || 0,
          text: annotation.text || '',
          x: annotation.x,
          y: annotation.y,
          id: annotation.id
        };
      }

      var annoData = el._annotationData[index];
      annoData.box_id = 'box-group-' + index;

      // Clamp at render time only — do NOT write the clamped values back
      // into annoData.offsetX/Y, so the user's placement is restored when
      // the grid grows again (zoom out / window widened)
      var boxPos = this.clampBoxToGrid(
        anchorPos[0] + annoData.offsetX,
        anchorPos[1] + annoData.offsetY,
        annotation,
        grid
      );

      var isAbove = boxPos[1] < anchorPos[1];
      var boxEdge = isAbove
        ? annotation.rectShape.y + annotation.rectShape.height
        : annotation.rectShape.y;

      return {
        anchorPos: anchorPos,
        boxPos: boxPos,
        boxEdge: boxEdge
      };
    },

// MAIN FUNCTION

    initialize: function(el, x, data) {
      var DEBOUNCE_DELAY = 150;
      var LEGEND_INIT_DELAY = 100;
      var ANNOTATION_UPDATE_DELAY = 200;
      var RESIZE_DEBOUNCE  = 100;

      var chart = echarts.getInstanceByDom(el);
      var self = this;

      // Grabs annotations from e$x$annotations
      var annotations = x.annotations || [];
      var svgs = {};
      var linesByGrid = {};
      var grids = [];
      var annotationVisibility = {};

      // Reset on every data render: R's annotation data (including any
      // offsetX/offsetY the app fed back) is the source of truth. Drags are
      // ephemeral. initialize only runs on data renders — zoom/resize go
      // through updateAnnotations, which does NOT reset — so drags still
      // survive zoom and resize within a render.
      el._annotationData = {};

      // This adds annontations to the legend so they can toggled off/on
      function addAnnotationLegendItems() {
        var option = chart.getOption();
        var legendNames = {};

        annotations.forEach(function(ann) {
        legendNames[ann.legend_name || 'Annotation'] = true;
      });

        // Add to existing legend data (don't create series)
        if (!option.legend || !option.legend[0]) {
          option.legend = [{ show: true, data: [] }];
        }

        if (!option.legend[0].data) {
          option.legend[0].data = [];
        }

        // Add annotation items to legend
        Object.keys(legendNames).forEach(function(name) {
          if (option.legend[0].data.indexOf(name) === -1) {
            option.legend[0].data.push(name);
          }
          if (option.legend[0].selected[name] === undefined) {
            option.legend[0].selected[name] = true;
          }
        });

        // Initialize selection state
        if (!option.legend[0].selected) {
          option.legend[0].selected = {};
        }

        Object.keys(legendNames).forEach(function(name) {
          if (option.legend[0].selected[name] === undefined) {
            option.legend[0].selected[name] = true;
          }
        });

        chart.setOption(option);
      }

      setTimeout(addAnnotationLegendItems, LEGEND_INIT_DELAY);


      // Initialize visibility tracking
      annotations.forEach(function(ann, index) {
        var legendName = ann.legend_name || 'Annotation';
        if (annotationVisibility[legendName] === undefined) {
          annotationVisibility[legendName] = true;
        }
      });

      function updateAnnotations() {
        var option = chart.getOption();
        if (!option || !option.grid || option.grid.length === 0) {
          clearTimeout(el._annoInitTimeout);
          el._annoInitTimeout = setTimeout(function() {
            if (el._anno) el._anno.update();
          }, ANNOTATION_UPDATE_DELAY);
          return;
        }

        self.clearAllSvgLines(el);

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

            var svg = self.getOrCreateSVG(el, i);

            // Belt-and-braces: empty this grid's group before we redraw,
            // in case it survived from a previous render.
            var existingGroup = svg.querySelector('#annotations-group-' + i);
            if (existingGroup) {
              while (existingGroup.firstChild) {
                existingGroup.removeChild(existingGroup.firstChild);
              }
            }

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
          var svg = svgs[gridIndex];
          var group = svg ? svg.querySelector('#annotations-group-' + gridIndex) : null;

          // CHECK VISIBILITY AGAINST LEGEND
          var legendName = ann.legend_name || 'Annotation';
          var isVisible = annotationVisibility[legendName] !== false;
          if (!isVisible || !grids[gridIndex] || !group) {
            return;
          }

          // Calculate annotation position (null = anchor outside zoom window)
          var annoPos = self.calculateAnnotationPosition(el, ann, chart, index, grids, gridIndex);

          if (!annoPos) return;

          // ann.lineStyle, etc. is a list from R. These create the SVG element and apply styling.
          var rect = self.createSVGElement('rect', ann.rectStyle);
          var text = self.createSVGElement('text', ann.textStyle);
          var line = self.createSVGElement('line', ann.lineStyle);
          var arrow = self.createSVGElement('polygon', ann.arrowStyle);
          var arrowTip = ann.arrowTip;

          // This group consists of: box and text. Outside the box, there's
          // an arrow and line
          var boxGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');

          // SVG LINE
          self.setAttrs(line, {
            id: 'line_' + index,
            x1: annoPos.anchorPos[0],
            y1: annoPos.anchorPos[1] + arrowTip,
            x2: annoPos.boxPos[0],
            y2: annoPos.boxPos[1] + annoPos.boxEdge
          });

          // SVG ARROW
          var arrowPointsStr = ann.arrowVertices.map(function(p) {
            return (annoPos.anchorPos[0] + p[0]) + ',' + (annoPos.anchorPos[1] + p[1]);
          }).join(' ');

          self.setAttrs(arrow, {
            id: 'arrow_' + index,
            points: arrowPointsStr,
          });

          // DRAGGABLE GROUP
          boxGroup.setAttribute('id', 'box-group-' + index);
          boxGroup.setAttribute('data-index', index);
          boxGroup.setAttribute('data-grid', gridIndex);
          boxGroup.style.pointerEvents = 'none';

          // SVG RECT
          self.setAttrs(rect, {
            id: 'box_' + index,
            x: ann.rectShape.x,
            y: ann.rectShape.y,
            width: ann.rectShape.width,
            height: ann.rectShape.height,
            rx: ann.rectShape.r
          });

          // Split shadow from normal attrs
          var rectStyle = Object.assign({}, ann.rectStyle || {});
          var shadow = rectStyle.shadow;
          delete rectStyle.shadow;

          // Apply shadow if present
          if (shadow && self.ensureShadowFilter) {
            var filterId = self.ensureShadowFilter(svg, shadow);
            rect.setAttribute('filter', 'url(#' + filterId + ')');
          }

          // Check if this annotation is draggable (default true when omitted)
          var isDraggable = ann.draggable !== false;

          if (isDraggable) {
            rect.style.cursor = 'move';
            rect.style.pointerEvents = 'all';
            rect.setAttribute('data-draggable', 'true');
          } else {
            rect.style.cursor = 'default';
            rect.style.pointerEvents = 'none';
            rect.setAttribute('data-draggable', 'false');
          }

          rect.setAttribute('data-index', index);
          rect.setAttribute('data-grid', gridIndex);

          // SVG TEXT
          self.setAttrs(text, {
            id: 'text_' + index,
            x: ann.textStyle.x,
            y: ann.textStyle.y
          });

          var tspans = self.htmlToTspans(ann.textStyle.text, ann.textStyle.x);

          tspans.forEach(function(t) {
            text.appendChild(t);
          });

          boxGroup.setAttribute('transform', 'translate(' + annoPos.boxPos[0] + ',' + annoPos.boxPos[1] + ')');

          // GROUP SVG ELEMENTS
          group.appendChild(line);
          group.appendChild(arrow);
          group.appendChild(boxGroup);

          boxGroup.appendChild(rect);
          boxGroup.appendChild(text);

          linesByGrid[gridIndex].push({
            line: line,
            arrow: arrow,
            boxGroup: boxGroup,
            rect: rect,
            anchorPos: annoPos.anchorPos,
            arrowTip: arrowTip,
            index: index,
            gridIndex: gridIndex,
            ann: ann
          });
        });
      }

      // Expose this render's state for the once-attached global handlers.
      // Re-renders overwrite this, so the handlers always see fresh state.
      el._anno = {
        update: updateAnnotations,
        linesByGrid: linesByGrid,
        grids: grids,
        svgs: svgs
      };

      // Cancel any deferred draw left over from a previous render, then
      // schedule this one through the live el._anno.update pointer so a
      // stale closure can never redraw old annotations over the new ones.
      clearTimeout(el._annoInitTimeout);
      el._annoInitTimeout = setTimeout(function() {
        if (el._anno) el._anno.update();
      }, ANNOTATION_UPDATE_DELAY);

// CHART EVENTS — off/on so Shiny re-renders don't stack handlers
      if (el._annoHandlers) {
        chart.off('dataZoom', el._annoHandlers.zoom);
        chart.off('timelinechanged', el._annoHandlers.timeline);

        chart.off('restore', el._annoHandlers.restore);
        chart.off('legendselectchanged', el._annoHandlers.legend);
      }

      el._annoHandlers = {
        zoom: updateAnnotations,
        timeline: updateAnnotations,
        restore: function() {
          Object.keys(annotationVisibility).forEach(function(name) {
      annotationVisibility[name] = true;
    });
          addAnnotationLegendItems();
          // give setOption a tick before recalculating pixel positions,
          // since restore also resets grid layout
          setTimeout(updateAnnotations, LEGEND_INIT_DELAY);
        },
        legend: function(params) {
          var clicked = params.name;
          var isOn = params.selected[clicked];

          var isAnnotationLegend = annotations.some(function (ann) {
            return ann.legend_name === clicked;
          });

          if (isAnnotationLegend) {
            annotationVisibility[clicked] = isOn;
            updateAnnotations();
          } else {
            // Wait for chart to finish rescaling, then update annotations
            setTimeout(function() {
              if (el._anno) el._anno.update();
            }, LEGEND_INIT_DELAY);
          }
        }
      };

      chart.on('dataZoom', el._annoHandlers.zoom);
      chart.on('timelinechanged', el._annoHandlers.timeline);
      chart.on('restore', el._annoHandlers.restore);
      chart.on('legendselectchanged', el._annoHandlers.legend);

// RESIZE — single observer, attached once, delegates through el._anno
      if (!el._annoResizeObserver && typeof ResizeObserver !== 'undefined') {
        el._annoResizeObserver = new ResizeObserver(function() {
          clearTimeout(el._annoResizeTimeout);
          el._annoResizeTimeout = setTimeout(function() {
            var c = echarts.getInstanceByDom(el); // re-fetch, don't close over
            if (!c || !el._anno) return;
            c.resize();
            setTimeout(el._anno.update, DEBOUNCE_DELAY);
          }, RESIZE_DEBOUNCE);
        });
        el._annoResizeObserver.observe(el);
      }

// DRAG — attached once per element; reads current state via el._anno
      if (!el._annoDragAttached) {
        el._annoDragAttached = true;

        var dragState = { dragging: false, current: null };

        // MOUSEDOWN
        document.addEventListener('mousedown', function(e) {
          if (e.target.tagName !== 'rect' ||
              e.target.getAttribute('data-draggable') !== 'true' ||
              !el.contains(e.target) ||
              !el._anno) {
            return;
          }

          var annIndex = parseInt(e.target.getAttribute('data-index'));
          var gridIdx = parseInt(e.target.getAttribute('data-grid'));

          var lines = el._anno.linesByGrid[gridIdx];
          var lineData = lines ? lines.find(function(l) {
            return l.index === annIndex;
          }) : null;

          if (!lineData) {
            return;
          }

          var svg = el._anno.svgs[gridIdx];
          var svgRect = svg.getBoundingClientRect();
          var boxGroup = lineData.boxGroup;

          var transform = boxGroup.getAttribute('transform');
          var match = transform.match(/translate\(([^,]+),([^)]+)\)/);
          var currentX = parseFloat(match[1]);
          var currentY = parseFloat(match[2]);

          dragState.dragging = true;
          dragState.current = {
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
        });

        // MOUSEMOVE — clamp the box to the grid while dragging
        document.addEventListener('mousemove', function(e) {
          if (!dragState.dragging || !dragState.current || !el._anno) return;

          var cur = dragState.current;
          var svgRect = cur.svg.getBoundingClientRect();
          var grid = el._anno.grids[cur.gridIndex];
          var ann = cur.lineData.ann;

          var desiredX = e.clientX - svgRect.left - cur.startX;
          var desiredY = e.clientY - svgRect.top - cur.startY;

          var clamped = self.clampBoxToGrid(desiredX, desiredY, ann, grid);
          var newX = clamped[0], newY = clamped[1];

          cur.boxGroup.setAttribute('transform', 'translate(' + newX + ',' + newY + ')');

          var isAbove = newY < cur.lineData.anchorPos[1];
          var boxEdge = isAbove ?
            ann.rectShape.y + ann.rectShape.height :
            ann.rectShape.y;

          cur.lineData.line.setAttribute('x2', newX);
          cur.lineData.line.setAttribute('y2', newY + boxEdge);

          el._annotationData[cur.annIndex].offsetX = newX - cur.lineData.anchorPos[0];
          el._annotationData[cur.annIndex].offsetY = newY - cur.lineData.anchorPos[1];

          e.preventDefault();
        });

        // MOUSEUP
        document.addEventListener('mouseup', function(e) {
          if (!dragState.dragging) return;

          if (typeof Shiny !== 'undefined' && dragState.current) {
            Shiny.onInputChange(
              el.id + '_dragged_annotation' + ':echarts4rParse',
              el._annotationData[dragState.current.annIndex]
            );
          }

          dragState.dragging = false;
          dragState.current = null;
        });
      }
    }
  };
})();
