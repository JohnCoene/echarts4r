# Color range

Build manual color range

## Usage

``` r
e_color_range(
  data,
  input,
  output,
  colors = c("#bf444c", "#d88273", "#f6efa6"),
  ...
)

e_color_range_(
  data,
  input,
  output,
  colors = c("#bf444c", "#d88273", "#f6efa6"),
  ...
)
```

## Arguments

- data:

  Data.frame in which to find column names.

- input, output:

  Input and output columns.

- colors:

  Colors to pass to
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html).

- ...:

  Any other argument to pass to
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html).

## Examples

``` r
df <- data.frame(val = 1:10)

e_color_range(df, val, colors)
#>    val  colors
#> 1    1 #BF444C
#> 2    2 #C55354
#> 3    3 #CB615D
#> 4    4 #D06E66
#> 5    5 #D57B6F
#> 6    6 #DC8E79
#> 7    7 #E4A784
#> 8    8 #EBBF8F
#> 9    9 #F1D79A
#> 10  10 #F6EFA6
```
