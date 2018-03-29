HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {

    var chart;
    
    return {

      renderValue: function(x) {
        
        chart = echarts.init(document.getElementById(el.id), x.theme);
        chart.setOption(x.opts);

      },
      
      getChart: function(){
        return chart;
      },

      resize: function(width, height) {

        if(myChart){
          chart.resize();
        }

      }

    };
  }
});

function get_e_charts(id){

  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  // Use the getChart method we created to get the underlying C3 chart
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
  
}
