# e_tooltip_item_formatter sets formatter for item_formatter

    Code
      fmt
    Output
      [1] "function(params, ticket, callback) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"decimal\",\"minimumFractionDigits\":2,\"maximumFractionDigits\":2,\"currency\":\"USD\"});\n        var idx = 0;\n        if (params.name == params.value[0]) {\n            idx = 1;\n        }\n        return params.marker + ' ' +\n               params.seriesName + ': ' + fmt.format(parseFloat(params.value[idx]));\n    }"
      attr(,"class")
      [1] "JS_EVAL"        "item_formatter"

# e_tooltip_choro_formatter sets formatter for item_formatter

    Code
      fmt
    Output
      [1] "function(params, ticket, callback) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"decimal\",\"minimumFractionDigits\":2,\"maximumFractionDigits\":2,\"currency\":\"USD\"});\n        var value = parseFloat(params.value) || 0;\n        return params.marker + ' ' +\n               params.name + ': ' + fmt.format(value);\n    }"
      attr(,"class")
      [1] "JS_EVAL"        "item_formatter"

# e_tooltip_pie_formatter sets formatter for item_formatter

    Code
      fmt
    Output
      [1] "function(params, ticket, callback) {\n    var fmt = new Intl.NumberFormat('en', {\"style\":\"decimal\",\"minimumFractionDigits\":2,\"maximumFractionDigits\":2,\"currency\":\"USD\"});\n    return params.marker + ' ' +\n           params.name + ': ' +\n           fmt.format(params.value);\n    }"
      attr(,"class")
      [1] "JS_EVAL"        "item_formatter"

# e_tooltip_pointer_formatter sets formatter for item_formatter

    Code
      fmt
    Output
      [1] "function(params, ticket, callback) {\n        var fmt = new Intl.NumberFormat('en', {\"style\":\"decimal\",\"minimumFractionDigits\":2,\"maximumFractionDigits\":2,\"currency\":\"USD\"});\n        var res = params[0].value[0];\n        for (i = 0; i < params.length; i++) {\n            res += '<br />' +\n                   params[i].marker + ' ' +\n                   params[i].seriesName + ': ' +\n                   fmt.format(parseFloat(params[i].value[1]));\n        }\n        return res;\n    }"
      attr(,"class")
      [1] "JS_EVAL"           "pointer_formatter"

# e_legend errors informatively with too few icons

    Code
      e_legend(e, icons = list("circle"))
    Condition
      Error in `e_legend()`:
      ! invalid number of icons; 1 icons passed but 2 legend items.

