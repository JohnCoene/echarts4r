test_that(".get_validate_range", {
  expect_equal(
    .get_validate_range(range(30, 50)),
    list(max = 50, min = 30)
  )
  expect_equal(
    .get_validate_range(range(30, 30)),
    list(max = 30, min = 0)
  )
  expect_equal(
    .get_validate_range(range(-30, -30)),
    list(max = 0, min = -30)
  )
  expect_equal(
    .get_validate_range(range(-30, -50)),
    list(max = -30, min = -50)
  )
})
