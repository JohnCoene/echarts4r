# Graphic

Low level API to define graphic elements.

## Usage

``` r
e_graphic_g(e, ...)

e_group_g(e, ...)

e_image_g(e, ...)

e_text_g(e, ...)

e_rect_g(e, ...)

e_circle_g(e, ...)

e_ring_g(e, ...)

e_sector_g(e, ...)

e_arc_g(e, ...)

e_polygon_g(e, ...)

e_polyline_g(e, ...)

e_line_g(e, ...)

e_bezier_curve_g(e, ...)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ...:

  Any other option to pass, check See Also section.

## Note

Some elements, i.e.: `e_image_g` may not display in the RStudio browwser
but will work fine in your browser, R markdown documents and Shiny
applications.

## Functions

- `e_graphic_g` to initialise graphics, entirely optional.

- `e_group_g` to create group, the children of which will share
  attributes.

- `e_image_g` to a png or jpg image.

- `e_text_g` to add text.

- `e_rect_g` to add a rectangle.

- `e_circle_g` to add a circle.

- `e_ring_g` to add a ring.

- `e_sector_g`

- `e_arc_g` to create an arc.

- `e_polygon_g` to create a polygon.

- `e_polyline_g` to create a polyline.

- `e_line_g` to draw a line.

- `e_bezier_curve_g` to draw a quadratic bezier curve or cubic bezier
  curve.

## Examples

``` r
# may not work in RStudio viewer
# Open in browser
cars |>
  e_charts(speed) |>
  e_scatter(dist) |>
  e_image_g(
    right = 20,
    top = 20,
    z = -999,
    style = list(
      image = "https://www.r-project.org/logo/Rlogo.png",
      width = 150,
      height = 150,
      opacity = .6
    )
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["dist"]},"series":[{"data":[{"value":[4,2]},{"value":[4,10]},{"value":[7,4]},{"value":[7,22]},{"value":[8,16]},{"value":[9,10]},{"value":[10,18]},{"value":[10,26]},{"value":[10,34]},{"value":[11,17]},{"value":[11,28]},{"value":[12,14]},{"value":[12,20]},{"value":[12,24]},{"value":[12,28]},{"value":[13,26]},{"value":[13,34]},{"value":[13,34]},{"value":[13,46]},{"value":[14,26]},{"value":[14,36]},{"value":[14,60]},{"value":[14,80]},{"value":[15,20]},{"value":[15,26]},{"value":[15,54]},{"value":[16,32]},{"value":[16,40]},{"value":[17,32]},{"value":[17,40]},{"value":[17,50]},{"value":[18,42]},{"value":[18,56]},{"value":[18,76]},{"value":[18,84]},{"value":[19,36]},{"value":[19,46]},{"value":[19,68]},{"value":[20,32]},{"value":[20,48]},{"value":[20,52]},{"value":[20,56]},{"value":[20,64]},{"value":[22,66]},{"value":[23,54]},{"value":[24,70]},{"value":[24,92]},{"value":[24,93]},{"value":[24,120]},{"value":[25,85]}],"name":"dist","type":"scatter","symbol":null,"coordinateSystem":"cartesian2d","yAxisIndex":0,"xAxisIndex":0,"symbolSize":3}],"graphic":{"right":20,"top":20,"z":-999,"style":{"image":"https://www.r-project.org/logo/Rlogo.png","width":150,"height":150,"opacity":0.6},"type":"image","right.1":20,"top.1":20,"z.1":-999,"style.1":{"image":"https://www.r-project.org/logo/Rlogo.png","width":150,"height":150,"opacity":0.6}}},"dispose":true},"evals":[],"jsHooks":[]}
anno_text <-
'xAxis represents miles per gallon, yAxis represents weight in tonnes.'
# Position does change depending on window size.
mtcars |>
  e_charts(mpg) |>
  e_line(wt) |>
  e_group_g(
    left = '18%',
    top = '55%',
    children = list(
      # Background rectangle
      list(
        type = 'rect',
        z = 100,
        left = 'center',
        top = 'middle',
        shape = list(
          width = 240,
          height = 70
        ),
        style = list(
          fill = '#fff',
          stroke = '#555',
          lineWidth = 1,
          shadowBlur = 8,
          shadowOffsetX = 3,
          shadowOffsetY = 3,
          shadowColor = 'rgba(0,0,0,0.2)'
        )
      ),
      # Text element
      list(
        type = 'text',
        z = 100,
        left = 'center',
        top = 'middle',
        style = list(
          fill = '#333',
          width = 220,
          overflow = 'break',
          text = anno_text,
          font = '14px Microsoft YaHei'
        )
      )
    )
  )

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"settings":{"crosstalk_key":null,"crosstalk_group":null},"opts":{"yAxis":[{"show":true}],"xAxis":[{"type":"value"}],"legend":{"data":["wt"]},"series":[{"data":[{"value":[10.4,5.25]},{"value":[10.4,5.424]},{"value":[13.3,3.84]},{"value":[14.3,3.57]},{"value":[14.7,5.345]},{"value":[15,3.57]},{"value":[15.2,3.78]},{"value":[15.2,3.435]},{"value":[15.5,3.52]},{"value":[15.8,3.17]},{"value":[16.4,4.07]},{"value":[17.3,3.73]},{"value":[17.8,3.44]},{"value":[18.1,3.46]},{"value":[18.7,3.44]},{"value":[19.2,3.44]},{"value":[19.2,3.845]},{"value":[19.7,2.77]},{"value":[21,2.62]},{"value":[21,2.875]},{"value":[21.4,3.215]},{"value":[21.4,2.78]},{"value":[21.5,2.465]},{"value":[22.8,2.32]},{"value":[22.8,3.15]},{"value":[24.4,3.19]},{"value":[26,2.14]},{"value":[27.3,1.935]},{"value":[30.4,1.615]},{"value":[30.4,1.513]},{"value":[32.4,2.2]},{"value":[33.9,1.835]}],"yAxisIndex":0,"xAxisIndex":0,"name":"wt","type":"line","coordinateSystem":"cartesian2d"}],"graphic":{"left":"18%","top":"55%","children":[{"type":"rect","z":100,"left":"center","top":"middle","shape":{"width":240,"height":70},"style":{"fill":"#fff","stroke":"#555","lineWidth":1,"shadowBlur":8,"shadowOffsetX":3,"shadowOffsetY":3,"shadowColor":"rgba(0,0,0,0.2)"}},{"type":"text","z":100,"left":"center","top":"middle","style":{"fill":"#333","width":220,"overflow":"break","text":"xAxis represents miles per gallon, yAxis represents weight in tonnes.","font":"14px Microsoft YaHei"}}],"type":"group","left.1":"18%","top.1":"55%","children.1":[{"type":"rect","z":100,"left":"center","top":"middle","shape":{"width":240,"height":70},"style":{"fill":"#fff","stroke":"#555","lineWidth":1,"shadowBlur":8,"shadowOffsetX":3,"shadowOffsetY":3,"shadowColor":"rgba(0,0,0,0.2)"}},{"type":"text","z":100,"left":"center","top":"middle","style":{"fill":"#333","width":220,"overflow":"break","text":"xAxis represents miles per gallon, yAxis represents weight in tonnes.","font":"14px Microsoft YaHei"}}]}},"dispose":true},"evals":[],"jsHooks":[]}#' @seealso \href{https://echarts.apache.org/en/option.html#graphic}{official documentation}
```
