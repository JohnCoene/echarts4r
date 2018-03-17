HTMLWidgets.widget({

  name: 'echarts4r',

  type: 'output',

  factory: function(el, width, height) {
    
    var firstRun = true;

    var sel_handle = new crosstalk.SelectionHandle();
    var filter_handle = new crosstalk.FilterHandle();

    sel_handle.on("change", function(e) {
      if (e.sender !== sel_handle) {
        myChart.clearBrush();
      }
      scatter.selection(e.value);
    });
    filter_handle.on("change", function(e) {
      myChart.filter(e.value);
    });
    var myChart;

    return {

      renderValue: function(x) {
      
        myChart = echarts.init(document.getElementById(el.id));
        var option = x.opts;
        myChart.setOption(option);

        scatter.on("brush", function(keys) {
          sel_handle.set(keys);
        });

        sel_handle.setGroup(value.group);
        filter_handle.setGroup(value.group);

        scatter(!firstRun);
          firstRun = false;
  
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
