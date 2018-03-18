HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {

    var myChart;
    
    var sel_handle = new crosstalk.SelectionHandle();
    
    return {

      renderValue: function(x) {
      
        myChart = echarts.init(document.getElementById(el.id));
        var option = x.opts;
        myChart.setOption(option);
        
        myChart.on("brush", function(keys) {
          // sel_handle.set(keys);
          console.log(keys);
        });
    
        sel_handle.on("change", function(e) {
          if (e.sender !== sel_handle) {
            myChart.clearBrush();
          }
          myChart.selection(e.value);
        });
        
        sel_handle.setGroup(x.settings.crosstalk_group);

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
