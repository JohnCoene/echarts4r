# Connect charts

Connect charts together.

## Usage

``` r
e_connect(e, ids)

e_group(e, group)

e_connect_group(e, group)

e_disconnect_group(e, group = NULL)

e_arrange(..., rows = NULL, cols = NULL, width = "xs", title = NULL)
```

## Arguments

- e:

  An `echarts4r` object as returned by
  [`e_charts`](https://echarts4r.john-coene.com/reference/init.md) or a
  proxy as returned by
  [`echarts4rProxy`](https://echarts4r.john-coene.com/reference/echarts4r-shiny.md).

- ids:

  Scalar, vector or list of ids of chart to connect with.

- group:

  Group name.

- ...:

  Any `echarts` objects.

- rows, cols:

  Number of rows and columns.

- width:

  Width of columns, one of `xs`, `md`, `lg`.

- title:

  Title of charts.

## Value

`e_arrange`: in an interactive session, returns a
[`htmltools::browsable`](https://rstudio.github.io/htmltools/reference/browsable.html),
in `rmarkdown` returns a container
([`htmltools::div`](https://rstudio.github.io/htmltools/reference/builder.html)).

## Note

`e_arrange` may not work properly in the RStudio viewer.

## Functions

- `e_connect`: connects charts by `ids`, *cannot* be disconnected.

- `e_group`: assigns a group to chart.

- `e_connect_group`: connects chart with another group.

- `e_disconnect_group`: diconnects chart from group.

- `e_arrange`: arrange charts.

## Examples

``` r
# linked datazoom
e1 <- cars |>
  e_charts(
    speed,
    height = 200
  ) |>
  e_scatter(dist) |>
  e_datazoom(show = FALSE) |>
  e_group("grp") # assign group

e2 <- cars |>
  e_charts(
    dist,
    height = 200
  ) |>
  e_scatter(speed) |>
  e_datazoom() |>
  e_group("grp") |> # assign group
  e_connect_group("grp") # connect

if (interactive()) {
  e_arrange(e1, e2, title = "Linked datazoom")
}
```
