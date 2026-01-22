(function() {
  window.EChartsAnnotations = {

    setAttrs: function(element, attrs) {
      for (var key in attrs) {
        if (attrs.hasOwnProperty(key) && attrs[key] != null) {
          element.setAttribute(key, attrs[key].toString());
        }
      }
    },

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

    // Main initialization function
    initialize: function(el, x, data, helpers) {
      var chart = echarts.getInstanceByDom(el);
      var self = this;

      setTimeout(function() {
        var option = chart.getOption();

        // Get unique annotation legend names
        var annotations = x.annotations || [];
        var legendItems = {};

        annotations.forEach(function(ann) {
          var name = ann.legend_name || ann.text || 'Annotation';
          if (!legendItems[name]) {
            legendItems[name] = {
              name: name,
              icon: 'rect',
              textStyle: { color: '#333' }
            };
          }
        });

        // Add to existing legend data (don't create series)
        if (!option.legend || !option.legend[0]) {
          option.legend = [{ show: true, data: [] }];
        }

        if (!option.legend[0].data) {
          option.legend[0].data = [];
        }

        // Add annotation items to legend
        Object.keys(legendItems).forEach(function(name) {
          option.legend[0].data.push(name);
        });

        // Initialize selection state
        if (!option.legend[0].selected) {
          option.legend[0].selected = {};
        }

        Object.keys(legendItems).forEach(function(name) {
          if (option.legend[0].selected[name] === undefined) {
            option.legend[0].selected[name] = true;
          }
        });

        chart.setOption(option);
      }, 100);

      var annotations = x.annotations || [];
      var svgs = {};
      var linesByGrid = {};
      var grids = [];
      var annotationVisibility = {};

      // Initialize visibility tracking
      annotations.forEach(function(ann, index) {
        var legendName = ann.legend_name || ann.text || 'Annotation';
        if (annotationVisibility[legendName] === undefined) {
          annotationVisibility[legendName] = true;
        }
      });

      function getOrCreateSVG(gridIndex) {
        var svgId = 'annotation-svg-' + el.id + '-grid-' + gridIndex;
        var svg = document.getElementById(svgId);

        if (!svg) {
          svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
          svg.setAttribute('id', svgId);
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

          // CHECK VISIBILITY AGAINST LEGEND
          var legendName = ann.legend_name || ann.text || 'Annotation';
          var isVisible = annotationVisibility[legendName] !== false;

          if (!isVisible) {
            return;
          }

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

          // Initialize annotation data
          if (!el._annotationData[index]) {
            el._annotationData[index] = {
              row_index: index,
              box_id: 'box_' + index,
              offsetX: ann.offsetX || 0,
              offsetY: ann.offsetY || 0,
              text: ann.text || '',
              x: ann.x,
              y: ann.y,
              id: ann.id
            };
          }

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
          self.setAttrs(line, {
            id: 'line_' + index,
            x1: anchorPos[0],
            y1: anchorPos[1] + arrowTip,
            x2: boxPos[0],
            y2: boxPos[1] + boxEdge
          });
          self.setAttrs(line, ann.lineStyle);
          group.appendChild(line);

          // SVG ARROW
          var arrow = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
          var arrowPointsStr = ann.arrowVertices.map(function(p) {
            return (anchorPos[0] + p[0]) + ',' + (anchorPos[1] + p[1]);
          }).join(' ');
          self.setAttrs(arrow, {
            id: 'arrow_' + index,
            points: arrowPointsStr,
          });
          self.setAttrs(arrow, ann.arrowStyle);
          group.appendChild(arrow);

          // DRAGGABLE GROUP
          var boxGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
          boxGroup.setAttribute('id', 'box-group-' + index);
          boxGroup.setAttribute('data-index', index);
          boxGroup.setAttribute('data-grid', gridIndex);
          boxGroup.style.pointerEvents = 'none';

          // SVG RECT
          var rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
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

          self.setAttrs(rect, rectStyle);

          // Apply shadow if present
          if (shadow && self.ensureShadowFilter) {
            var filterId = self.ensureShadowFilter(svg, shadow);
            rect.setAttribute('filter', 'url(#' + filterId + ')');
          }

          rect.style.cursor = 'move';
          rect.style.pointerEvents = 'all';
          rect.setAttribute('data-draggable', 'true');
          rect.setAttribute('data-index', index);
          rect.setAttribute('data-grid', gridIndex);

          boxGroup.appendChild(rect);

          // SVG TEXT
          var text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
          self.setAttrs(text, {
            id: 'text_' + index,
            x: ann.textStyle.x,
            y: ann.textStyle.y
          });

          var tspans = self.htmlToTspans(ann.textStyle.text, ann.textStyle.x);
          tspans.forEach(function(t) {
            text.appendChild(t);
          });

          self.setAttrs(text, ann.textStyle);

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

      // ADD LEGEND HANDLER
      chart.on('legendselectchanged', function(params) {
        if (params.selected) {
          Object.keys(params.selected).forEach(function(name) {
            var isAnnotationLegend = annotations.some(function(ann) {
              return (ann.legend_name || ann.text) === name;
            });

            if (isAnnotationLegend) {
              annotationVisibility[name] = params.selected[name];
            }
          });

          updateAnnotations();
        }
      });

      // Resize observer
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

      // MOUSEDOWN
      document.addEventListener('mousedown', function(e) {
        if (e.target.tagName === 'rect' && e.target.getAttribute('data-draggable') === 'true') {
          var annIndex = parseInt(e.target.getAttribute('data-index'));
          var gridIdx = parseInt(e.target.getAttribute('data-grid'));

          var lineData = linesByGrid[gridIdx] ? linesByGrid[gridIdx].find(function(l) {
            return l.index === annIndex;
          }) : null;

          if (!lineData) {
            return;
          }

          var svg = svgs[gridIdx];
          var svgRect = svg.getBoundingClientRect();
          var boxGroup = lineData.boxGroup;

          var transform = boxGroup.getAttribute('transform');
          var match = transform.match(/translate\(([^,]+),([^)]+)\)/);
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

        var desiredX = e.clientX - svgRect.left - currentDrag.startX;
        var desiredY = e.clientY - svgRect.top - currentDrag.startY;

        var boxLeft = desiredX + ann.rectShape.x;
        var boxRight = desiredX + ann.rectShape.x + ann.rectShape.width;
        var boxTop = desiredY + ann.rectShape.y;
        var boxBottom = desiredY + ann.rectShape.y + ann.rectShape.height;

        var newX = desiredX;
        var newY = desiredY;

        if (boxLeft < 0) {
          newX = -ann.rectShape.x;
        } else if (boxRight > grid.width) {
          newX = grid.width - ann.rectShape.x - ann.rectShape.width;
        }

        if (boxTop < 0) {
          newY = -ann.rectShape.y;
        } else if (boxBottom > grid.height) {
          newY = grid.height - ann.rectShape.y - ann.rectShape.height;
        }

        currentDrag.boxGroup.setAttribute('transform', 'translate(' + newX + ',' + newY + ')');

        var isAbove = newY < currentDrag.lineData.anchorPos[1];
        var boxEdge = isAbove ?
          ann.rectShape.y + ann.rectShape.height :
          ann.rectShape.y;

        currentDrag.lineData.line.setAttribute('x2', newX);
        currentDrag.lineData.line.setAttribute('y2', newY + boxEdge);

        el._annotationData[currentDrag.annIndex].offsetX = newX - currentDrag.lineData.anchorPos[0];
        el._annotationData[currentDrag.annIndex].offsetY = newY - currentDrag.lineData.anchorPos[1];

        e.preventDefault();
      });

      // MOUSEUP
      document.addEventListener('mouseup', function(e) {
        if (isDragging) {
          if (typeof Shiny !== 'undefined') {
            Shiny.onInputChange(
              el.id + '_dragged_annotation' + ':echarts4rParse',
              el._annotationData[currentDrag.annIndex]
            );

            Shiny.setInputValue('annotation_positions', el._annotationData, {
              priority: 'event'
            });
          }

          isDragging = false;
          currentDrag = null;
        }
      });
    }
  };
})();
