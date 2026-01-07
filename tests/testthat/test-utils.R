
test_that(".correct_countries works with vectors", {
  input <- c("United States of America", "France", "Viet Nam", "Brazil")
  expected <- c("United States", "France", "Vietnam", "Brazil")
  expect_equal(.correct_countries(input), expected)
})
