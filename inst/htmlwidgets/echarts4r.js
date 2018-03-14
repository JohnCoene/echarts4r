HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {

    var myChart = null;

    return {

      renderValue: function(x) {
        
        if(myChart === null){
          myChart = echarts.init(document.getElementById(el.id));
          var option = x.opts;
          myChart.setOption(option);
        }

      },
      
      getChart: function(){
        return myChart;
      },

      resize: function(width, height) {

        if(myChart){
          myChart.resize();
        }

      }

    };
  }
});
