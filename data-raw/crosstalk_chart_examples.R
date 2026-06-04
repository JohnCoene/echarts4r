# e_area_ -----------------------------------------------------------------
library(crosstalk)
library(DT)

sd <- CO2 |>
  group_by(Plant) |> SharedData$new()

bscols(
  sd |>
    e_charts(conc) |>
    e_area(uptake, selectedMode = 'single') |>
    e_tooltip(trigger = "axis"),
  datatable(sd)
)
# e_step ------------------------------------------------------------------
sd <- USArrests |> tibble::rownames_to_column("State") |> SharedData$new()

bscols(
  sd |>
    e_charts(State) |>
    e_step(Murder, name = "Start", step = "start", fill = TRUE, selectedMode = 'single',
           blur = list(
             itemStyle = list(
               opacity = 0.3          # dim non-selected bars strongly
             ))) |>
    e_step(Rape, name = "Middle", step = "middle") |>
    e_step(Assault, name = "End", step = "end") |>
    e_tooltip(trigger = "axis"),
  datatable(sd)
)

# e_scatter ---------------------------------------------------------------
sd <- USArrests |> tibble::rownames_to_column("State") |> SharedData$new()

bscols(
  sd |>
    e_charts(State) |>
    e_scatter(Murder, name = "Start", step = "start", fill = TRUE, selectedMode = 'single',
           blur = list(
             itemStyle = list(
               opacity = 0.3          # dim non-selected bars strongly
             ))),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)

# coord_system = 'geo' - doesnt work
# df <- quakes  |> SharedData$new()
# df |>
#   e_charts(long) |>
#   e_geo(
#     roam = TRUE,
#     boundingCoords = list(
#       c(185, -10),
#       c(165, -40)
#     )
#   ) |>
#   e_scatter(lat, mag, coord_system = "geo") |>
#   e_visual_map(min = 4, max = 6.5)

# e_effect_scatter_ -------------------------------------------------------
sd <- USArrests |> tibble::rownames_to_column("State") |> SharedData$new()

bscols(
  sd |>
    e_charts(State) |>
    e_effect_scatter(Murder, name = "Start", step = "start", fill = TRUE, selectedMode = 'single',
              blur = list(
                itemStyle = list(
                  opacity = 1         # dim non-selected bars strongly
                ))),
  datatable(sd)
)

# e_candle_ ---------------------------------------------------------------
date <- c(
  "2017-01-01",
  "2017-01-02",
  "2017-01-03",
  "2017-01-04",
  "2017-03-05",
  "2017-01-06",
  "2017-01-07"
)

stock <- data.frame(
  date = date,
  opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
  closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
  low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
  high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
) |> SharedData$new()

bscols(
  stock |>
    e_charts(date) |>
    e_candle(opening, closing, low, high, selectedMode = "single") |>
    e_y_axis(min = 190, max = 220),
  datatable(stock)
)

# e_funnel ------------------------------------------------------------------

funnel <- data.frame(
  stage = c("View", "Click", "Purchase"),
  value = c(80, 30, 20)
)|> SharedData$new()


bscols(
  funnel |>
    e_charts() |>
    e_funnel(value, stage, selectedMode = "single"),
  datatable(funnel)
)


# e_heatmap_ --------------------------------------------------------------
v <- LETTERS[1:10]
matrix <- data.frame(
  x = sample(v, 300, replace = TRUE),
  y = sample(v, 300, replace = TRUE),
  z = rnorm(300, 10, 1),
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(x, y) |>
  dplyr::summarise(z = sum(z)) |>
  dplyr::ungroup()  |> SharedData$new()
bscols(
  matrix |>
    e_charts(x) |>
    e_heatmap(y, z,  selectedMode = "multiple", itemStyle = list(emphasis = list(shadowBlur = 10))),
  datatable(matrix)
)


# e_pie -------------------------------------------------------------------
df <- mtcars |>
  head() |>
  tibble::rownames_to_column("model") |> SharedData$new()

bscols(
  df |>
  e_charts(model) |>
    e_pie(carb),
  datatable(df)
)


# e_pictorial -------------------------------------------------------------

sd <- y <- rnorm(10, 10, 2)
df <- data.frame(
  x = 1:10,
  y = y,
  z = y - rnorm(10, 5, 1)
) |>  SharedData$new()
bscols(
  df |>
  e_charts(x) |>
    e_bar(z, barWidth = 10) |>
    e_pictorial(
      y,
      symbol = "rect",
      symbolRepeat = TRUE,
      z = -1,
      symbolSize = c(10, 4),
      selectedMode = "multiple"
    ),
  datatable(df, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)

# e_histogram_ ------------------------------------------------------------
sd <- USArrests |> tibble::rownames_to_column("State") |> SharedData$new()

bscols(
  sd |>
    e_charts(State) |>
    e_histogram_("Murder", name = "Start", step = "start", fill = TRUE, selectedMode = 'single',
          blur = list(
            itemStyle = list(
              opacity = 0.3          # dim non-selected bars strongly
            ))),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)


# e_density_ --------------------------------------------------------------
sd <- USArrests |> tibble::rownames_to_column("State") |> SharedData$new()

bscols(
  sd |>
    e_charts(State) |>
    e_density_("Murder", selectedMode = 'single',
                 blur = list(
                   itemStyle = list(
                     opacity = 0.3          # dim non-selected bars strongly
                   ))),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)



# Not working -------------------------------------------------------------
# dates <- seq.Date(as.Date("2017-01-01"), as.Date("2019-12-31"), by = "day")
# values <- rnorm(length(dates), 20, 6)
# year <- data.frame(date = dates, values = values) |> SharedData$new()
#
# year |>
#   e_charts(date) |>
#   e_calendar(range = "2017") |>
#   e_visual_map(max = 30) |>
#   e_heatmap(values, coord_system = "calendar")
#
#
#
#
# df <- quakes |> SharedData$new()
#
#
# bscols(
#   df |>
#     e_charts(long) |>
#     # e_geo(
#     #   roam = TRUE,
#     #   selectedMode = "single",
#     #   boundingCoords = list(
#     #     c(185, -10),
#     #     c(165, -40)
#     #   )
#     # ) |>
#     e_scatter_gl(lat, depth),
#   datatable(df)
# )
#
#
# # ??
# df <- flights |> SharedData$new()
# bscols(
#   df |> e_charts() |>
#     e_geo(
#   ) |>
#   e_lines(
#     start_lon,
#     start_lat,
#     end_lon,
#     end_lat,
#     name = "flights",
#     effect = list(show = TRUE),
#     selectedMode = "multiple"
#   ) |>
#   e_legend(FALSE),
#   datatable(df)
# )
#
# bscols(
# df |>
#   e_charts() |>
#   e_boxplot(y, outliers = TRUE, selectedMode = "series") |>
#   e_boxplot(x, outliers = TRUE),
# datatable(df)
# )
#
#
# choropleth <- data.frame(
#   countries = c(
#     "France",
#     "Brazil",
#     "China",
#     "Russia",
#     "Canada",
#     "India",
#     "United States",
#     "Argentina",
#     "Australia"
#   ),
#   height = runif(9, 1, 5),
#   color = c(
#     "#F7FBFF",
#     "#DEEBF7",
#     "#C6DBEF",
#     "#9ECAE1",
#     "#6BAED6",
#     "#4292C6",
#     "#2171B5",
#     "#08519C",
#     "#08306B"
#   )
# )  |> SharedData$new()
#
# bscols(
# choropleth |>
#   e_charts(countries) |>
#   e_geo_3d(height, selectedMode = "single"),
# datatable(df)
# )
#
#
# df <- airports |> SharedData$new()
#
# bscols(
# df |>
#   e_charts(long) |>
#   e_scatter_3d(lat, cnt, selectedMode = 'single'),
# datatable(df)
# )
#
#
#
# df <- data.frame(x = 1:10, y = seq(1, 20, by = 2)) |> SharedData$new()
# bscols(
# df |>
#   e_charts(x) |>
#   e_polar() |>
#   e_angle_axis() |>
#   e_radius_axis() |>
#   e_line(y, smooth = TRUE, selectedMode = 'single'),
# datatable(df)
# )
#
# df <- data.frame("Class" = rep(c("Class1", "Class2", "Class3"),each = 3),
#                  "Grade" = c("Grade1","Grade2", "Grade3"),
#                  "A" = sample(1:10, 9),
#                  "B" = sample(1:10,9),
#                  "C" = sample(1:10,9)) |> SharedData$new()
#
# bscols(
# df |> e_chart() |>
#   e_matrix(xAxis = "Class", yAxis = "Grade") |>
#   e_pie_matrix(x = "A", y = c( "B", "C"), selectedMode = 'single'),
# datatable(df)
# )
