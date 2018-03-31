HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {

    var chart;
    
    return {

      renderValue: function(x) {
        
        chart = echarts.init(document.getElementById(el.id), x.theme);
        chart.setOption(x.opts);
        
        if(x.loading === true){
          chart.showLoading('default', x.loadingOpts);
        } else {
          chart.hideLoading();
        }
        
        $(document).on('shiny:recalculating', function(event) {
          chart.showLoading();
        });

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
        console.log(opts);
        chart.appendData({
          seriesIndex: opts.seriesIndex,
          data: opts.data
        });
      }
  });
  
}
