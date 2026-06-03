# Country names

Convert country names to echarts format.

## Usage

``` r
e_country_names(data, input, output, type = "iso2c", ...)

e_country_names_(data, input, output = NULL, type = "iso2c", ...)
```

## Arguments

- data:

  Data.frame in which to find column names.

- input, output:

  Input and output columns.

- type:

  Passed to
  [countrycode](https://vincentarelbundock.github.io/countrycode/man/countrycode.html)
  `origin` parameter.

- ...:

  Any other parameter to pass to
  [countrycode](https://vincentarelbundock.github.io/countrycode/man/countrycode.html).

## Details

Taiwan and Hong Kong cannot be plotted.

## Examples

``` r
cns <- data.frame(country = c("US", "BE"))

# replace
e_country_names(cns, country)
#>         country
#> 1 United States
#> 2       Belgium

# specify output
e_country_names(cns, country, country_name)
#>   country  country_name
#> 1      US United States
#> 2      BE       Belgium
```
