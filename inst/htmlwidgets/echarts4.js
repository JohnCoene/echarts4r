HTMLWidgets.widget({

  name: 'echarts4',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var myChart = echarts.init(document.getElementById(el.id));
        var option = x.opts;
         myChart.setOption(option);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
