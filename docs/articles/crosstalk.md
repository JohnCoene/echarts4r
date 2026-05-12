# Crosstalk

This implementation of
[{crosstalk}](https://rstudio.github.io/crosstalk/) adds another level
to {echarts4r} interactivity! The {crosstalk} package allows for
interactive filtering and selecting of data across multiple widgets
without requiring a Shiny server. This is great for Markdown and HTML
documents to add interactivity without the complexity of Shiny.

Crosstalk is supported for the following chart types:

- [`e_area()`](https://echarts4r.john-coene.com/reference/e_area.md)
- [`e_bar()`](https://echarts4r.john-coene.com/reference/e_bar.md)
- [`e_candle()`](https://echarts4r.john-coene.com/reference/e_candle.md)
- [`e_density()`](https://echarts4r.john-coene.com/reference/histogram.md)
- [`e_effect_scatter()`](https://echarts4r.john-coene.com/reference/scatter.md)
- [`e_funnel()`](https://echarts4r.john-coene.com/reference/e_funnel.md)
- [`e_heatmap()`](https://echarts4r.john-coene.com/reference/e_heatmap.md)
- [`e_histogram()`](https://echarts4r.john-coene.com/reference/histogram.md)
- [`e_line()`](https://echarts4r.john-coene.com/reference/e_line.md)
- [`e_pictorial()`](https://echarts4r.john-coene.com/reference/e_pictorial.md)
- [`e_pie()`](https://echarts4r.john-coene.com/reference/e_pie.md)
- [`e_scatter()`](https://echarts4r.john-coene.com/reference/scatter.md)
- [`e_step()`](https://echarts4r.john-coene.com/reference/e_step.md)

Of these chart types, cartesian2d and no coordinate system are supported
while timeline is also supported. Data requiring crosstalk functionality
must be used in `e_charts().` Functions that add data to an echart (such
as [`e_add()`](https://echarts4r.john-coene.com/reference/nesting.md))
is not supported.

This integration has not been tested in Shiny.

I want to acknowledge {echarty} for guidance on implementing crosstalk
in Echarts!

## Example

``` r
library(echarts4r)
library(crosstalk)
library(dplyr)
library(DT)
library(htmltools)
```

To use {crosstalk} in {echarts4r}, we need to create a `SharedData`
object. Then add `selectedMode` argument - this can be `TRUE` or one of
`single`, `multiple` or `series` to each e\_\* trace to have
interaction.

``` r
# Needs a ShareData object
sd <- SharedData$new(mtcars |> tibble::rownames_to_column("model"))

# Yes, it's a linked object
crosstalk::is.SharedData(sd)
#> [1] TRUE


bscols(
  sd |>
    e_charts(model) |>
    e_bar_("mpg", 
           selectedMode = "single",
           # Let's make the selection stand out!
           emphasis = list(
             focus='self'
           ),
           blur = list(
             itemStyle = list(
               opacity = 0.3
             ))) |> e_brush(),
    datatable(sd,
              class = "compact",
              width = "100%", 
              height = 350,
              extensions ="Scroller", 
              style = "bootstrap",
              options = list(scroller=TRUE, deferRender=TRUE, scrollY=200)
    )
)
```

## With Grouped Data

Grouping data must be done before a `SharedData` object is created.
Here’s an example using {crosstalk} filtering.

``` r
sdf <- mtcars |>
  group_by(cyl) |>
  SharedData$new()

p <- sdf |>
  e_charts(hp) |>
  e_line(mpg,
         selectedMode = 'multiple',
         select = list(itemStyle= list(color='red')),
         # Highlight point in a series (by cyl)
         emphasis = list(focus='self', blurScope='series'),
         blur = list(itemStyle= list(opacity = 0.3))
  )
         
bscols(
  list(
    filter_slider("hp", "Horsepower", sdf, ~hp, width = "100%"),
    filter_checkbox("cyl", "Cylinders", sdf, ~cyl, inline=TRUE),
    filter_checkbox("gear", "Gears", sdf, ~gear, inline=TRUE),
    filter_select("carb", "Carburetors (select)", sdf, ~carb),
    datatable(sdf,
              class = "compact",
              width = "100%", 
              height = 350,
              extensions ="Scroller", 
              style = "bootstrap",
              options = list(scroller=TRUE, scrollY=200, deferRender=TRUE, searching = FALSE)
    )
  ),
  p,
  widths = c(6, 6)
)
```

Horsepower

Cylinders

4

6

8

Gears

3

4

5

Carburetors (select)

## Crosstalk with Two Charts

When two charts use the same `SharedData` object, selected point(s) are
highlighted and non-selected points are downplayed.

``` r
sd <- USArrests |> tibble::rownames_to_column("State") |> SharedData$new()

bscols(
  sd |>
    e_charts(State) |>
    e_scatter(Murder,
              selectedMode = 'multiple',
              emphasis = list(
                focus='self'
                ),
           blur = list(
             itemStyle = list(
               opacity = 0.3 
               ))
           ),
  sd |>
    e_charts(State) |>
    e_bar(Murder,
          selectedMode = 'single',
            emphasis = list(
             focus='self'
           ),
          blur = list(
             itemStyle = list(
               opacity = 0.3
             ))
          ),
  datatable(sd, 
            extensions="Scroller",
            style="bootstrap", 
            class="compact",
            width="100%", 
            options=list(deferRender=TRUE, 
                         scrollY=300, 
                         scroller=TRUE
                         )
            )
)
```

## Timeline

Same with timeline, selected bars are highlighted and unselected are
downplayed.

``` r
grouped_sd <- iris |>
  distinct(Species, .keep_all = TRUE) |> 
  dplyr::group_by(Species) |>
  SharedData$new(key = ~Species)

bscols(
  grouped_sd |> e_chart(Species, timeline = TRUE) |>
  e_bar_("Sepal.Length", legend = FALSE,
         selectedMode = "single", 
         blur = list(
    itemStyle = list(
      opacity = 0.3
    ))
  ),
  datatable(grouped_sd, 
            extensions="Scroller",
            style="bootstrap", 
            class="compact",
            width="100%", 
            options=list(deferRender=TRUE, 
                         scrollY=300, 
                         scroller=TRUE
                         )
            )
)
```
