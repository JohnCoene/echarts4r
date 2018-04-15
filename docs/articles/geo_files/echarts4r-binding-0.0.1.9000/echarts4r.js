HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {
    
    var initialized = false;

    var chart;
    
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
            echarts.registerTheme(x.theme, th);
          }
          
          if(x.registerMap === true){
            echarts.registerMap(x.mapName, x.geoJSON);
          }
        }
        
        chart = echarts.init(document.getElementById(el.id), x.theme);
        chart.setOption(x.opts);

        $(document).on('shiny:recalculating', function() {
          if(x.loading === true){
            chart.showLoading('default', x.loadingOpts);
          } else if(x.loading === false) {
            chart.hideLoading();
          }
        });
        
        $(document).on('shiny:value', function() {
          chart.hideLoading();
        });
        
        if (HTMLWidgets.shinyMode) {
          chart.on("brushselected", function(e){
            Shiny.onInputChange(el.id + '_brush' + ":echarts4rParse", e.batch[0].selected);
          });
          
          chart.on("legendselectchanged", function(e){
            Shiny.onInputChange(el.id + '_legend_change' + ":echarts4rParse", e.name);
          });
          
          chart.on("click", function(e){
            console.log(e);
            Shiny.onInputChange(el.id + '_clicked_data' + ":echarts4rParse", e.data.value);
            Shiny.onInputChange(el.id + '_clicked_row' + ":echarts4rParse", e.dataIndex + 1);
            Shiny.onInputChange(el.id + '_clicked_serie' + ":echarts4rParse", e.seriesName);
          });
        }

      },
      
      getChart: function(){
        return chart;
      },

      resize: function(width, height) {

        if(chart){
          chart.resize();
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

if (HTMLWidgets.shinyMode) {
  
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
        chart.dispatchAction({
          type: 'showTip',
          seriesIndex: data.seriesIndex,
          name: data.name,
          position: data.position
        });
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
  
}
