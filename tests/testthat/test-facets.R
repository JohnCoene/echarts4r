test_that("pie charts can be visualized with e_facet", {
    # Check in interactive session if the chart matches expectations
    
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "test of visualizations must be run individually")
    
    n_groups <- 12
    get_letters <- function(n) sample(x  = LETTERS, size = sample(5:8, 1))
    letter_per_group <- sapply(1:n_groups, get_letters)
    
    
    df <- tibble::tibble("person" = 1:n_groups,
                 "activities" = letter_per_group) |>
        tidyr::unnest(activities) |>
        dplyr::mutate(duration = sample(1:6, dplyr::n(), replace = T))
    
    # Test only runs when the necessary lines are executed 
    expect_no_error(print(
        df |>
            dplyr::group_by(person) |>
            echarts4r::e_charts(activities) |>
            echarts4r::e_pie(duration) |>
            echarts4r::e_facet(rows = 3, cols = 4)
    ))
})

