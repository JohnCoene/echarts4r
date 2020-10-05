var boxxyBinding = new Shiny.OutputBinding();

$.extend(boxxyBinding, {
  find: function(scope) {
    return $(scope).find(".echarts4rBox");
  },
  renderValue: function(el, data) {
    let container = '#echarts4r-box-container-' + el.id;

    // dependencies
    Shiny.renderDependencies(data.deps);

    // bg
    $(container).css("background-color", data.bgColor);

    // create chart
    var chart = echarts.init(el);

    chart.setOption(data.options);
  }
});

Shiny.outputBindings.register(boxxyBinding, "echarts4r.boxes");
