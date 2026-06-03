HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {

    var initialized = false;

    var chart,opts;

    const evalFun = (sourceOpts) => {
      let opts = Object.assign({}, sourceOpts);
      Object.keys(opts).forEach((key) => {
        if (opts[key] !== null) {
          if (typeof opts[key] === 'object') {
            evalFun(opts[key]);
            return;
          }
          if (typeof opts[key] === 'string') {
            try {
              opts[key] = eval('(' + opts[key] + ')');
            } catch { }
          }
        }
      });
      return(opts);
    }

    return {

      renderValue: function(x) {

        if(x.dispose === true){
          chart = echarts.init(document.getElementById(el.id));
          chart.dispose();
        }

        if (!initialized) {
          initialized = true;
          if(x.theme2 === true){
            var th = JSON.parse(x.customTheme);
            echarts.registerTheme(x.theme_name, th);
          }

        }

        if(x.hasOwnProperty('registerMap')){
          for( var map = 0; map < x.registerMap.length; map++){
            if(x.registerMap[map].extra)
              echarts.registerMap(x.registerMap[map].mapName, x.registerMap[map].geoJSON, x.registerMap[map].extra);
            else
              echarts.registerMap(x.registerMap[map].mapName, x.registerMap[map].geoJSON);
          }
        }

        if(x.hasOwnProperty('mapboxToken')){
          mapboxgl.accessToken = x.mapboxToken;
        }

        if(!x.mainOpts)
          x.mainOpts = [];
        x.mainOpts.renderer = x.renderer;

        chart = echarts.init(document.getElementById(el.id), x.theme, x.mainOpts);

        opts = evalFun(x.opts);

        if(x.morphed){
          opts = x.opts[0][x.morphed.default]
        }

        if(x.draw === true)
          chart.setOption(opts);

  // ── ADD CROSSTALK ──────────────────────
  // this does use internal echarts APIs: model.getData(), data.get(), chart.getModel().getSeriesByIndex()
  // {echarty} was used for guidance: https://github.com/helgasoft/echarty/blob/main/inst/htmlwidgets/echarty.js
        if (x.settings.crosstalk_group) {

        var isTimeline = x.tl;
        var tmp = -1;

        // select idx of series by finding which one has 'Xtalk' as id: e$x$opts$series[[1]]$datasetId
        if (opts.series) {
          tmp = opts.series.findIndex(x => x.datasetId && x.datasetId.startsWith('Xtalk'))
          // for timeline
        } else if (opts.options && opts.options[0] && opts.options[0].series) {
          tmp = opts.options[0].series.findIndex(x => x.datasetId && x.datasetId.startsWith('Xtalk'));
        }

        if (tmp === -1) tmp = 0;  // fallback to first series

      	var sel_handle = new crosstalk.SelectionHandle();
      	var ct_filter =  new crosstalk.FilterHandle();

      	sel_handle.setGroup(x.settings.crosstalk_group);
      	ct_filter.setGroup(x.settings.crosstalk_group);

      	// store all keys on chart for lookup
        chart.akeys = x.settings.crosstalk_key;

        // store original timeline options for filter/highlight.
        // Everything works different in timeline charts bc data is not found via 'XkeyX'
        if (isTimeline && opts.options) {
          chart._tlOriginal = JSON.parse(JSON.stringify(opts.options));
          chart._currentFilter = [];
        }

        // re-apply filter when timeline frame changes
        if (isTimeline) {
          chart.on('timelinechanged', function(e) {
            applyTimelineFilter(chart, e.currentIndex, chart._currentFilter || []);
          });
        }

        // Gets keys from brush
        chart.on("brushselected", function(params) {
          if (!params.batch || !params.batch[0] || !params.batch[0].areas || params.batch[0].areas.length === 0) {
            sel_handle.set([]);
            return;
          }
          var selectedKeys = [];
          var hasSelected = params.batch[0].selected && params.batch[0].selected.some(function(s) {
            return s.dataIndex && s.dataIndex.length > 0;
          });
          if (hasSelected) {
            params.batch[0].selected.forEach(function(s) {
              var model = chart.getModel().getSeriesByIndex(s.seriesIndex);
              if (!model) return;
              var data = model.getData();
              s.dataIndex.forEach(function(di) {
                var key = data.get('XkeyX', di);
                if (key !== undefined) selectedKeys.push(String(key));
              });
            });
          } else {
            // fallback: manual pixel hit testing for dataset-driven series
            var area = params.batch[0].areas[0];
            var range = area.range;
            var brushType = area.brushType;
            var opt = chart.getOption();
            var series = opt.series || [];
            series.forEach(function(s, si) {
              var model = chart.getModel().getSeriesByIndex(si);
              if (!model) return;
              var data = model.getData();
              for (var i = 0; i < data.count(); i++) {
                if (!s.encode) continue;
                var xVal = data.get(s.encode.x, i);
                var yVal = data.get(s.encode.y, i);
                var pt = chart.convertToPixel({ seriesIndex: si }, [xVal, yVal]);
                if (!pt) continue;
                var hit = false;
                if (brushType === 'rect') {
                  hit = pt[0] >= range[0][0] && pt[0] <= range[0][1] &&
                        pt[1] >= range[1][0] && pt[1] <= range[1][1];
                } else {
                  hit = pointInPolygon(pt, range);
                }
                if (hit) {
                  var key = data.get('XkeyX', i);
                  if (key !== undefined) selectedKeys.push(String(key));
                }
              }
            });
          }
          sel_handle.set(selectedKeys);
        });


        // Reset keys from brush
        chart.on("brushEnd", function(params) {
          if (params.areas.length === 0) sel_handle.set([]);
        });

      // Keys from echarts to crosstalk
        chart.on("selectchanged", function(keys) {
          if (!keys.isFromClick) return;
          var selectedKeys = [];

          if (isTimeline) {
            keys.selected.forEach(function(s) {
              s.dataIndex.forEach(function(di) {
                var model = chart.getModel().getSeriesByIndex(s.seriesIndex);
                if (!model) return;
                var rawData = model.option.data;
                if (rawData && rawData[di]) {
                  selectedKeys.push(String(rawData[di].value[0]));
                }
              });
            });
          } else {
            keys.selected.forEach(function(s) {
              var model = chart.getModel().getSeriesByIndex(s.seriesIndex);
              if (!model) return;
              var data = model.getData();
              s.dataIndex.forEach(function(di) {
                var key = data.get('XkeyX', di);
                if (key !== undefined) selectedKeys.push(String(key));
              });
            });
          }

        /// sel_handle or ct_filter - this is the behavior when something is selected and what happens in the other plot. sel_handle - it will highlight, not filter
          if (selectedKeys.length === 0) {
            sel_handle.clear();
          } else {
            sel_handle.set(selectedKeys);
          }
        });

        // inbound selection handler
        sel_handle.on("change", function(e) {
          if (e.sender == sel_handle) return;
          var inKeys = e.value ? e.value.map(String) : [];

          if (isTimeline) {
            chart._currentFilter = inKeys;
            var idx = chart.getModel().getComponent('timeline') &&
                      chart.getModel().getComponent('timeline').getCurrentIndex
                      ? chart.getModel().getComponent('timeline').getCurrentIndex()
                      : 0;
            applyTimelineFilter(chart, idx, inKeys);
          } else {
            var opt = chart.getOption();
            var series = opt.series || [];
            var highlights = [];
            series.forEach(function(s, si) {
              var model = chart.getModel().getSeriesByIndex(si);
              if (!model) return;
              var data = model.getData();
              var matchIdx = [];
              for (var i = 0; i < data.count(); i++) {
                var key = String(data.get('XkeyX', i));
                if (inKeys.indexOf(key) > -1) matchIdx.push(i);
              }
              highlights.push({ si: si, matchIdx: matchIdx });
            });
            highlights.forEach(function(h) {
              chart.dispatchAction({ type: 'downplay', seriesIndex: h.si });
            });
            if (inKeys.length > 0) {
              highlights.forEach(function(h) {
                if (h.matchIdx.length > 0) {
                  chart.dispatchAction({ type: 'highlight', seriesIndex: h.si, dataIndex: h.matchIdx });
                }
              });
            }
          }
        });

      // inbound filter handler
        ct_filter.on('change', function(e) {
          if (e.sender == ct_filter) return;
          if (e.value == undefined) e.value = chart.akeys;

          if (isTimeline) {
            chart._currentFilter = e.value.map(String);
            var idx = chart.getModel().getComponent('timeline') &&
                      chart.getModel().getComponent('timeline').getCurrentIndex
                      ? chart.getModel().getComponent('timeline').getCurrentIndex()
                      : 0;
            applyTimelineFilter(chart, idx, chart._currentFilter);
          } else {
            var rexp = (e.value.length == chart.akeys.length)
              ? '^' : '^(' + e.value.join('|') + ')$';
            var opt = chart.getOption();
            if (!opt.dataset) return;
            opt.dataset.forEach(function(d) {
              if (d.id && d.id.startsWith('Xtalk_')) {
                if (d.transform && d.transform[1]) d.transform[1].config.reg = rexp;
              } else if (d.id === 'Xtalk') {
                if (d.transform) d.transform.config.reg = rexp;
              }
            });
            chart.setOption(opt, false);
            }
              });

      }
              // ── END CROSSTALK ────────────────────────────────────────────

              // shiny callbacks
              if (HTMLWidgets.shinyMode) {

                chart.on("brushselected", function(e){
                  Shiny.onInputChange(el.id + '_brush' + ":echarts4rParse", e);
                });

                chart.on("brush", function(e){
                  Shiny.onInputChange(el.id + '_brush_released' + ":echarts4rParse", e);
                });

                chart.on("legendselectchanged", function(e){
                  Shiny.onInputChange(el.id + '_legend_change' + ":echarts4rParse", e.name);
                  Shiny.onInputChange(el.id + '_legend_selected' + ":echarts4rParse", e.selected);
                });

                chart.on("globalout", function(e){
                  Shiny.onInputChange(el.id + '_global_out' + ":echarts4rParse", e, {priority: 'event'});
                });

                if(x.hasOwnProperty('zr')){
                  chart.getZr().on("click", function(e){
                    delete e.stop;
                    delete e.topTarget;
                    delete e.target
                    delete e.event.path;
                    Shiny.setInputValue(el.id + '_clicked_zr' + ":echarts4rParse", e);
                  });
                }

                if(x.hasOwnProperty('capture')){
                  chart.on(x.capture, function(e){
                    Shiny.onInputChange(el.id + '_' + x.capture + ":echarts4rParse", e, {priority: 'event'});
                  });
                }

              chart.getZr().on('dragend', function(e) {
                if (e.target &&
                    e.target.id != null &&
                    String(e.target.id).startsWith('box_')) {

                  var annotationData = null;
                  for (var index in el._annotationData) {
                    if (el._annotationData[index].box_id === e.target.id) {
                      annotationData = el._annotationData[index];
                      break;
                    }
                  }

                  if (annotationData) {
                    Shiny.onInputChange(
                      el.id + "_dragged_annotation" + ":echarts4rParse",
                      annotationData
                    );
                  }
                }
              });

          chart.on("click", function(e){
            Shiny.onInputChange(el.id + '_clicked_data' + ":echarts4rParse", e.data, {priority: 'event'});
            Shiny.onInputChange(el.id + '_clicked_row' + ":echarts4rParse", e.dataIndex + 1, {priority: 'event'});
            Shiny.onInputChange(el.id + '_clicked_serie' + ":echarts4rParse", e.seriesName, {priority: 'event'});
          });

          chart.on("mouseover", function(e){
            Shiny.onInputChange(el.id + '_mouseover_data' + ":echarts4rParse", e.data);
            Shiny.onInputChange(el.id + '_mouseover_row' + ":echarts4rParse", e.dataIndex + 1);
            Shiny.onInputChange(el.id + '_mouseover_serie' + ":echarts4rParse", e.seriesName);
          });

          $(document).on('shiny:recalculating', function() {

            if(x.hideWhite === true){
              var css = '.recalculating {opacity: 1.0 !important; }',
                  head = document.head || document.getElementsByTagName('head')[0],
                  style = document.createElement('style');

              style.type = 'text/css';
              if (style.styleSheet){
                style.styleSheet.cssText = css;
              } else {
                style.appendChild(document.createTextNode(css));
              }
              head.appendChild(style);
            }

            if(x.loading === true){
              chart.showLoading('default', x.loadingOpts);
            } else if(x.loading === false) {
              chart.hideLoading();
            }

          });

          $(document).on('shiny:value', function() {
            chart.hideLoading();
          });
        }

        if(x.hasOwnProperty('connect')){
          var connections = [];
          for(var c = 0; c < x.connect.length; c++){
            connections.push(get_e_charts(x.connect[c]));
          }
          connections.push(chart);
          echarts.connect(connections);
        }

        // actions
        if(x.events.length >= 1){
          for(var i = 0; i < x.events.length; i++){
            chart.dispatchAction(x.events[i].data);
          }
        }

        // buttons
        var buttons = x.buttons;
        Object.keys(buttons).map( function(buttonId){
          document.getElementById(buttonId).addEventListener('click',
            (function(id) {
              const scoped_id = id;
              return function(e){
                buttons[scoped_id].forEach(function(el){
                  chart.dispatchAction(el.data);
                });
              };
            }
            )(buttonId)
          );
        });

        if(x.hasOwnProperty('on')){
          for(var e = 0; e < x.on.length; e++){
            chart.on(x.on[e].event, x.on[e].query, x.on[e].handler);
          }
        }

        if(x.hasOwnProperty('off')){
          for(var ev = 0; ev < x.off.length; ev++){
            chart.off(x.off[ev].event, x.off[ev].query, x.off[ev].handler);
          }
        }

        if(x.hasOwnProperty('chartGroup')){
          chart.group = x.chartGroup;
        }

        if(x.hasOwnProperty('groupConnect')){
          echarts.connect(x.groupConnect);
        }

        if(x.hasOwnProperty('groupDisconnect')){
          echarts.disconnect(x.groupDisconnect);
        }

        if(x.morphed){
          opts = x.opts[0];
          console.log(x.morphed);
          let fn = eval(x.morphed.callback);
          fn();
        }

      },

      getChart: function(){
        return chart;
      },

      getOpts: function(){
        return opts;
      },

      resize: function(width, height) {
        if(!chart)
          return;

        chart.resize({width: width, height: height});
      }

    };
  }
});

function get_e_charts(id){

  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var echarts;

  if (typeof htmlWidgetsObj != 'undefined') {
    echarts = htmlWidgetsObj.getChart();
  }

  return(echarts);
}

function get_e_charts_opts(id){

  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var echarts;

  if (typeof htmlWidgetsObj != 'undefined') {
    echarts = htmlWidgetsObj.getOpts();
  }

  return(echarts);
}

function distinct(value, index, self) {
  return self.indexOf(value) === index;
}

function rm_undefined(el){
  return el != undefined;
}

// apply filter to a single timeline frame in crosstalk
function applyTimelineFilter(chart, frameIndex, inKeys) {
  if (!chart._tlOriginal) return;
  var frame = chart._tlOriginal[frameIndex];
  if (!frame || !frame.series) return;
  frame.series.forEach(function(s) {
    if (!s.data) return;
    var newData = s.data.map(function(d) {
      if (!d) return d;
      var key = String(d.value[0]);
      if (inKeys.length === 0 || inKeys.indexOf(key) > -1) {
        return Object.assign({}, d, { itemStyle: { opacity: 1 } });
      } else {
        return Object.assign({}, d, { itemStyle: { opacity: 0.1 } });
      }
    });
    var update = { options: [] };
    for (var p = 0; p < frameIndex; p++) update.options.push({});
    update.options.push({ series: [{ data: newData }] });
    chart.setOption(update, false);
  });
}

// find points selected by brush for crosswalk
function pointInPolygon(point, polygon) {
  var x = point[0], y = point[1];
  var inside = false;
  for (var i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
    var xi = polygon[i][0], yi = polygon[i][1];
    var xj = polygon[j][0], yj = polygon[j][1];
    var intersect = ((yi > y) != (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
    if (intersect) inside = !inside;
  }
  return inside;
}

if (HTMLWidgets.shinyMode) {

  // DRAW
  Shiny.addCustomMessageHandler('e_draw_p',
    function(data) {
      var chart = get_e_charts(data.id);
      var opts = get_e_charts_opts(data.id);
      if (typeof chart != 'undefined') {
        chart.setOption(opts);
      }
  });

  // HIGHLIGHT AND DOWNPLAY

  Shiny.addCustomMessageHandler('e_highlight_p',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.dispatchAction({
          type: 'highlight',
          seriesIndex: data.seriesIndex,
          seriesName: data.seriesName
        });
      }
  });

  Shiny.addCustomMessageHandler('e_downplay_p',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.dispatchAction({
          type: 'downplay',
          seriesIndex: data.seriesIndex,
          seriesName: data.seriesName
        });
      }
  });

  // TOOLTIP

  Shiny.addCustomMessageHandler('e_showtip_p',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.dispatchAction(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('e_hidetip_p',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.dispatchAction({
          type: 'hideTip'
        });
      }
  });

  Shiny.addCustomMessageHandler('e_append_p',
    function(opts) {
      var chart = get_e_charts(opts.id);
      if (typeof chart != 'undefined') {
        chart.appendData({
          seriesIndex: opts.seriesIndex,
          data: opts.data
        });
      }
  });

  Shiny.addCustomMessageHandler('e_focus_node_adjacency_p',
    function(data) {
      console.log(data);
      var chart = get_e_charts(data[0].id);
      if (typeof chart != 'undefined') {
        data.forEach(function(highlight){
          chart.dispatchAction(highlight.opts);
        })
      }
  });

  Shiny.addCustomMessageHandler('e_unfocus_node_adjacency_p',
    function(data) {
      var chart = get_e_charts(data[0].id);
      if (typeof chart != 'undefined') {
        data.forEach(function(highlight){
          chart.dispatchAction(highlight.opts);
        })
      }
  });

  Shiny.addCustomMessageHandler('e_dispatch_action_p',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.dispatchAction(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('e_register_map',
    function(data) {
      if (typeof chart != 'undefined') {
        $.ajax({
          url: data.geoJSON,
          dataType: 'json',
          async: data.mapAsync,
          success: function(json){
            echarts.registerMap(data.mapName, json);
          }
        });

      }
  });

  Shiny.addCustomMessageHandler('e_resize',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.resize();
      }
  });

  Shiny.addCustomMessageHandler('e_send_p',
    function(data) {
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        let opts = chart.getOption();

        // add series
        if(!opts.series)
          opts.series = [];

        if(data.opts.series)
          data.opts.series.forEach(function(serie){
            opts.series.push(serie);
          });

        if(data.opts.color){
          if(data.opts.appendColor)
            data.opts.color.forEach(function(color){
              opts.color.push(color);
            });
          else
            opts.color = data.opts.color;
        }

        if(data.opts.backgroundColor)
          opts.color = data.opts.backgroundColor;

        // legend
        if(data.opts.legend && opts.legend.length > 0)
          if(data.opts.legend.data)
            opts.legend[0].data = opts.legend[0].data.concat(data.opts.legend.data);

        // x Axis
        if(opts.xAxis){
          if(opts.xAxis[0].data){
            let xaxis = opts.xAxis[0].data.concat(data.opts.xAxis[0].data);
            xaxis = xaxis.filter(distinct).filter(rm_undefined);
            opts.xAxis[0].data = xaxis;
          }
        }

        // y Axis
        if(opts.yAxis){
          if(opts.yAxis[0].data){
            let yaxis = opts.yAxis[0].data.concat(data.opts.yAxis[0].data);
            yaxis = yaxis.filter(distinct).filter(rm_undefined);
            opts.yAxis[0].data = yaxis;
          }
        }

        chart.setOption(opts, true);
      }
  });

  Shiny.addCustomMessageHandler('e_remove_serie_p',
    function(data) {

      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        let opts = chart.getOption();

        if(data.serie_name){
          let series = opts.series;
          series.forEach(function(s, index){
            if (typeof s.name == "undefined"){
             if (s.data[[0]].name == data.serie_name){
               this.splice(index, 1);
             }
            } else {
            if(s.name == data.serie_name){
              this.splice(index, 1);
            }
            }
          }, series)
          opts.series = series;
        }

        if(data.serie_index != null){
          opts.series = opts.series.splice(data.serie_index, 1);
        }

        chart.setOption(opts, true);
      }
  });

  Shiny.addCustomMessageHandler('e_merge_p',
    function(data) {
      // called by e_merge, add marks to serie
      var chart = get_e_charts(data.id);
      if (typeof chart != 'undefined') {
        chart.setOption(data.opts);
      }
  });

}
