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
            echarts.registerMap(x.registerMap[map].mapName, x.registerMap[map].geoJSON);
          }
        }
        
        if(x.hasOwnProperty('mapboxToken')){
          mapboxgl.accessToken = x.mapboxToken;
        }
        
        chart = echarts.init(document.getElementById(el.id), x.theme, {renderer: x.renderer});
        
        opts = evalFun(x.opts);
        
        if(x.draw === true)
          chart.setOption(opts);
        
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
            Shiny.onInputChange(el.id + '_global_out' + ":echarts4rParse", e);
          });
          
          if(x.hasOwnProperty('capture')){
            chart.on(x.capture, function(e){
              Shiny.onInputChange(el.id + '_' + x.capture + ":echarts4rParse", e, {priority: 'event'});
            });
          }
          
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

      },
      
      getChart: function(){
        return chart;
      },
      
      getOpts: function(){
        return opts;
      },

      resize: function(width, height) {

        if(chart){
          chart.resize({width: width, height: height});
        }

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
          url: x.geoJSON, 
          dataType: 'json', 
          async: x.mapAsync,
          success: function(json){ 
            echarts.registerMap(x.mapName, json);
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

        data.opts.series.forEach(function(serie){
          opts.series.push(serie);
        })

        // legend
        if(opts.legend.length > 0)
          if(data.opts.legend.data)
            opts.legend[0].data = opts.legend[0].data.concat(data.opts.legend.data);

        // x Axis
        if(opts.xAxis){
          if(opts.xAxis[0].data){
            let xaxis = opts.xAxis[0].data.concat(data.opts.xAxis[0].data);
            xaxis = xaxis.filter(distinct);
            opts.xAxis[0].data = xaxis;
          }
        }

        // y Axis
        if(opts.yAxis){
          if(opts.yAxis[0].data){
            let yaxis = opts.yAxis[0].data.concat(data.opts.yAxis[0].data);
            yaxis = yaxis.filter(distinct);
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
            if(s.name == data.serie_name){
              this.splice(index, 1);
            }
          }, series)
          opts.series = series;
        }

        if(data.serie_index)
          opts.series = opts.series.splice(data.index, 1);

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
