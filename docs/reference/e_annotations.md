# Add annotations to a chart

Apache ECharts does not include a native annotation system. This is a
custom function that creates annotations using SVG. This currently only
works in a 'cartesian2d' coordinate system.

Each annotation must be in a list with an x, y, and text. Styling can be
added - see @details. Because they are SVG, it takes SVG arguments, not
ECharts arguments for styling.

In Shiny, to output an annotation position after dragging the box, use
`input$id_dragged_annotation` or see
[echarts4r-shiny](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).
This captures the annotation parameters - not any of the styles.

## Usage

``` r
e_annotations(
  e,
  annotations,
  .facet = NULL,
  legend = TRUE,
  name = "Annotations",
  legend_color = "#738DE4",
  default_color = "#738DE4",
  draggable = TRUE
)
```

## Arguments

- e:

  An echarts4r object

- annotations:

  list of annotations to plot

- .facet:

  integer; facet's index. Only needed for faceted plots. Defaults to
  first plot. panel.

- legend:

  Whether to add annotations to legend.

- name:

  name of the legend

- legend_color:

  color of the legend box

- default_color:

  color of all SVG elements, unless specified in each style. It colors
  text, line, arrow, and rectable outline.

- draggable:

  booleon; if TRUE, annotations can be dragged by the user

## Details

annotations can take the following styles to change the defaults. To
remove any element use "none".

\- **rectStyle** Styles the annotation box. It also takes **shape** that
uses [echarts graphic.elements-rect.shape
attributes](https://echarts.apache.org/en/option.html#graphic.elements-rect.shape)
that controls rectangle width, height and corner radius.

\- **textStyle**, Styles the annotation text. This supports limited HTML
tags using tspan. Supported tags are: `b, strong, i, em, u, br, span`.
Styling inside `span` can use `color, fontSize, fontWeight, fontStyle`.
`x` and `y` attributes are automatically determined based on box size,
unless specified. `padding_trbl` was also added to add padding - this
must be a list of exactly 4 integers - in the order of top, right,
bottom, and left padding. This padding just pushes the text in that
direction.

\- **lineStyle** Styles the line that connects the annotation box to the
arrow using [SVG stroke
attributes](https://www.w3schools.com/graphics/svg_stroking.asp)

\- **arrowStyle**: Styles the arrow. size was added that automatically
adjusts the vertices of the triangle.

## Note

HTML tags may not apply in RStudio viewer, open in browser.

## See also

\- [Additional arguments for
textStyle](https://www.w3schools.com/graphics/svg_text.asp)

\- [Additional arguments for
rectStyle](https://www.w3schools.com/graphics/svg_stroking.asp)

\- [Additional arguments for
lineStyle](https://www.w3schools.com/graphics/svg_stroking.asp)

\- [Additional arguments for
arrowStyle](https://www.w3schools.com/graphics/svg_fill.asp)

\- [Even more arguments for SVG
attributes](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute)

## Examples

``` r
# May not work render HTML correctly in RStudio viewer
# Open in browser
mtcars |>
  e_charts(mpg) |>
  e_scatter(wt) |>
  e_annotations(
    annotations = list(
      list(
        id = 0,
        x = 15,
        y = 3,
        text = 'An annotation<br>with <i>style</i>',
        offsetX = 0,
        offsetY = -50,
        textStyle = list(
          "font-size" = 14,
          "font-weight" = 'bold',
          color = "#394",
          padding_trbl = list(0, 0, 5, 0)
        ),
        rectStyle = list(
          `stroke-dasharray` = c(35, 10),
          `stroke-width` = 2,
              # Shadow can be added!
               shadow = list(
                 dx= 0,
                 dy= 2,
                 blur= 4,
                 color= '#000',
                 opacity= 0.3
               ),
          shape = list(width = 105, height = 50, r = 0)
        ),
        lineStyle = list(`stroke-width` = 2),
        arrowStyle = list(size = 8)
      ),
      # No styles, will just show text
      list(
        id = 0,
        x = 25,
        y = 1,
        text = 'No styles',
        lineStyle = "none",
        rectStyle = "none",
        arrowStyle = "none"
      ),
      # Default style
      list(
        id = 2,
        x = 25,
        y = 4.5,
        text = "Default style",
        offsetX = 0,
        offsetY = -40
      ),
      # left-aligned annotation
      list(
        id = 2,
        x = 0,
        y = 0.5,
        text = "I'm left<br>aligned!",
        offsetX = 60,
        offsetY = -40,
        textStyle = list(
          "text-anchor" = "start",
          padding_trbl = list(0, 0, 6, 10)
        )
      )
    ))

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt"]},"series":[{"data":[{"value":[10.4,5.25]},{"value":[10.4,5.424]},{"value":[13.3,3.84]},{"value":[14.3,3.57]},{"value":[14.7,5.345]},{"value":[15,3.57]},{"value":[15.2,3.78]},{"value":[15.2,3.435]},{"value":[15.5,3.52]},{"value":[15.8,3.17]},{"value":[16.4,4.07]},{"value":[17.3,3.73]},{"value":[17.8,3.44]},{"value":[18.1,3.46]},{"value":[18.7,3.44]},{"value":[19.2,3.44]},{"value":[19.2,3.845]},{"value":[19.7,2.77]},{"value":[21,2.62]},{"value":[21,2.875]},{"value":[21.4,3.215]},{"value":[21.4,2.78]},{"value":[21.5,2.465]},{"value":[22.8,2.32]},{"value":[22.8,3.15]},{"value":[24.4,3.19]},{"value":[26,2.14]},{"value":[27.3,1.935]},{"value":[30.4,1.615]},{"value":[30.4,1.513]},{"value":[32.4,2.2]},{"value":[33.9,1.835]}],"name":"wt","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3},{"type":"scatter","name":"Annotations","data":[],"symbol":"rect","symbolSize":8,"itemStyle":{"color":"#738DE4"},"gridIndex":0,"xAxisIndex":0,"yAxisIndex":0,"tooltip":{"show":false},"silent":true,"animation":false,"isAnnotationLegend":true}],"tooltip":{"trigger":"fake"}},"dispose":true,"annotations":[{"id":0,"x":15,"y":3,"text":"An annotation<br>with <i>style<\/i>","offsetX":0,"offsetY":-50,"rectShape":{"x":-52.5,"y":-25,"width":105,"height":50,"r":0},"rectStyle":{"stroke":"#738DE4","fill":"#ffffff","stroke-width":2,"stroke-dasharray":[35,10],"shadow":{"dx":0,"dy":2,"blur":4,"color":"#000","opacity":0.3},"shape":{"width":105,"height":50,"r":0}},"textStyle":{"font-size":14,"font-weight":"bold","text-anchor":"middle","dominant-baseline":"central","color":"#394","padding_trbl":[0,0,5,0],"x":0,"y":-5,"text":"An annotation<br>with <i>style<\/i>","fill":"#394"},"lineStyle":{"stroke-width":2,"stroke":"#738DE4"},"arrowStyle":{"fill":"#738DE4","size":8},"arrowVertices":[[-4,-8],[4,-8],[0,0]],"arrowTip":-8,"draggable":true,"gridIndex":0,"legend_name":"Annotations"},{"id":0,"x":25,"y":1,"text":"No styles","offsetX":0,"offsetY":0,"rectShape":{"x":-40,"y":-20,"width":80,"height":40,"r":3},"rectStyle":{"fill":"rgba(255, 255, 255, 0)"},"textStyle":{"font-size":11,"font-weight":"bold","text-anchor":"middle","dominant-baseline":"central","x":0,"y":0,"text":"No styles","fill":"#738DE4"},"lineStyle":[],"arrowStyle":{"size":0},"arrowVertices":[[-0,-0],[0,-0],[0,0]],"arrowTip":-0,"draggable":true,"gridIndex":0,"legend_name":"Annotations"},{"id":2,"x":25,"y":4.5,"text":"Default style","offsetX":0,"offsetY":-40,"rectShape":{"x":-40,"y":-20,"width":80,"height":40,"r":3},"rectStyle":{"stroke":"#738DE4","fill":"#ffffff","stroke-width":2},"textStyle":{"font-size":11,"font-weight":"bold","text-anchor":"middle","dominant-baseline":"central","x":0,"y":0,"text":"Default style","fill":"#738DE4"},"lineStyle":{"stroke-width":2,"stroke":"#738DE4"},"arrowStyle":{"fill":"#738DE4","size":8},"arrowVertices":[[-4,-8],[4,-8],[0,0]],"arrowTip":-8,"draggable":true,"gridIndex":0,"legend_name":"Annotations"},{"id":2,"x":0,"y":0.5,"text":"I'm left<br>aligned!","offsetX":60,"offsetY":-40,"rectShape":{"x":-40,"y":-20,"width":80,"height":40,"r":3},"rectStyle":{"stroke":"#738DE4","fill":"#ffffff","stroke-width":2},"textStyle":{"font-size":11,"font-weight":"bold","text-anchor":"start","dominant-baseline":"central","padding_trbl":[0,0,6,10],"x":-30,"y":-6,"text":"I'm left<br>aligned!","fill":"#738DE4"},"lineStyle":{"stroke-width":2,"stroke":"#738DE4"},"arrowStyle":{"fill":"#738DE4","size":8},"arrowVertices":[[-4,-8],[4,-8],[0,0]],"arrowTip":-8,"draggable":true,"gridIndex":0,"legend_name":"Annotations"}]},"evals":[],"jsHooks":{"render":[{"code":"function(el, x, data) {\n      function initWhenReady() {\n        if (typeof EChartsAnnotations === 'undefined') {\n          setTimeout(initWhenReady, 50);\n          return;\n        }\n\n        EChartsAnnotations.initialize(el, x, data);\n      }\n\n      initWhenReady();\n    }","data":null}]}}
```
