# generate test input file
test_input <- "199
200
208
210
200
207
240
269
260
263"
test_file <- tempfile()
writeLines(test_input, test_file)

depths <- read_depths(test_file)

test_that("Day 1 part 1 adheres to the specification", {
  expect_equal(7,  depths |> detect_increases() |> sum())
})

test_that("Day 1 part 2 adheres to the specification", {
  expect_equal(5,  depths |> window_depths(window_size = 3) |> detect_increases() |> sum())
})
