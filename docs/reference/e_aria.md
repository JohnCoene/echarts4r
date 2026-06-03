# Aria

W3C defined the Accessible Rich Internet Applications Suite (WAI-ARIA)
to make Web content and Web applications more accessible to the
disabled. From ECharts 4.0, echarts4r supports ARIA by generating
description for charts automatically.

## Usage

``` r
e_aria(e, enabled = TRUE, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- enabled:

  Whether to enable aria helper text.

- ...:

  Any other option to pass, check See Also section.

## Details

There should be an aria-label attribute on the chart DOM, which can help
the disabled understand the content of charts with the help of certain
devices.

## See also

[official documentation](https://echarts.apache.org/en/option.html#aria)
