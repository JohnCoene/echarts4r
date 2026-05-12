# Themes

You can easily customise the appearance of your chart using one of the
13 themes provided, or even build you own theme: use the online [theme
builder](https://echarts.apache.org/en/theme-builder.html) to create
your own.

## default

``` r
p <- USArrests |> 
  tibble::rownames_to_column("State") |> 
  dplyr::mutate(Rape = -Rape) |> 
  e_charts(State) |> 
  e_line(Murder) |> 
  e_area(Rape, name = "Sick basterd", x_index = 1) |>  # second y axis
  e_title("Your plot", "Subtext", sublink = "https://john-coene.com") |> 
  e_x_axis(index = 1, show = FALSE) |> # hide second X Axis
  e_tooltip(trigger = "axis")

p # default plot
```

## dark

``` r
p |> e_theme("dark")
```

## vintage

``` r
p |> e_theme("vintage")
```

## westeros

``` r
p |> e_theme("westeros")
```

## essos

``` r
p |> e_theme("essos")
```

## wonderland

``` r
p |> e_theme("wonderland")
```

## walden

``` r
p |> e_theme("walden")
```

## chalk

``` r
p |> e_theme("chalk")
```

## infographic

``` r
p |> e_theme("infographic")
```

## macarons

``` r
p |> e_theme("macarons")
```

## roma

``` r
p |> e_theme("roma")
```

## shine

``` r
p |> e_theme("shine")
```

## purple-passion

``` r
p |> e_theme("purple-passion")
```

## halloween

``` r
p |> e_theme("halloween")
```

## azul

``` r
p |> e_theme("azul")
```

## blue

``` r
p |> e_theme("blue")
```

## caravan

``` r
p |> e_theme("caravan")
```

## carp

``` r
p |> e_theme("carp")
```

## cool

``` r
p |> e_theme("cool")
```

## dark-blue

``` r
p |> e_theme("dark-blue")
```

## dark-bold

``` r
p |> e_theme("dark-bold")
```

## dark-digerati

``` r
p |> e_theme("dark-digerati")
```

## dark-fresh-cut

``` r
p |> e_theme("dark-fresh-cut")
```

## dark-mushroom

``` r
p |> e_theme("dark-mushroom")
```

## forest

``` r
p |> e_theme("forest")
```

## fresh-cut

``` r
p |> e_theme("fresh-cut")
```

## fruit

``` r
p |> e_theme("fruit")
```

## gray

``` r
p |> e_theme("gray")
```

## green

``` r
p |> e_theme("green")
```

## helianthus

``` r
p |> e_theme("helianthus")
```

## inspired

``` r
p |> e_theme("inspired")
```

## london

``` r
p |> e_theme("london")
```

## macarons

``` r
p |> e_theme("macarons")
```

## macarons2

``` r
p |> e_theme("macarons2")
```

## mint

``` r
p |> e_theme("mint")
```

## red

``` r
p |> e_theme("red")
```

## red-velvet

``` r
p |> e_theme("red-velvet")
```

## royal

``` r
p |> e_theme("royal")
```

## sakura

``` r
p |> e_theme("sakura")
```

## tech-blue

``` r
p |> e_theme("tech-blue")
```

## custom

``` r
p |> e_theme_custom('{"color":["#ff715e","#ffaf51"]}')
```

The above is an admittedly small theme, if you want to create a large
custom theme, download the JSON from the [theme
builder](https://echarts.apache.org/en/theme-builder.html) and use it,
`e_theme_custom` and `e_register_theme` both detect the json file and
read it in.

``` json
// theme.json
{
  "color":["#ff715e","#ffaf51"],
  "backgroundColor": "black"
}
```

``` r
# in R
p |> e_theme_custom("theme.json")
```

The method above, however, registers the theme every time the chart is
drawn, if displaying multiple charts this can leads to large html files
or slower Shiny application. There is thus also the function
`e_theme_register` to register the custom theme once, and subsequently
use it.

``` r
library(shiny)
library(echarts4r)

ui <- fluidPage(
  e_theme_register('{"color":["#ff715e","#ffaf51"]}', name = "myTheme"),
  echarts4rOutput("chart1"),
  echarts4rOutput("chart2")
)

server <- function(input, output){
  e <- e_charts(cars, speed) |> 
      e_scatter(dist) 

  output$chart1 <- renderEcharts4r({
    e_theme(e, "myTheme")
  })

  output$chart2 <- renderEcharts4r({
    e_theme(e, "myTheme")
  })

}

shinyApp(ui, server)
```

Note that the custom theme registered as above works with `e_common`,
e.g.: `e_common(theme = "myTheme")`.
