test_file <- create_test_file("199
200
208
210
200
207
240
269
260
263")

depths <- read_depths(test_file)

test_that(test_name(day = 1, part = 1), {
  expect_equal(7,  depths |> detect_increases() |> sum())
})

test_that(test_name(day = 1, part = 2), {
  expect_equal(5,  depths |> window_depths(window_size = 3) |> detect_increases() |> sum())
})
