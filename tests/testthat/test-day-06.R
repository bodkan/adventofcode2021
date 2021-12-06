test_file <- create_test_file("3,4,3,1,2")

times <- read_lanternfish(test_file)

test_that(test_name(day = 6, part = 1), {
  expect_true(simulate_lanternfish(times, 18) == 26)
  expect_true(simulate_lanternfish(times, 80) == 5934)
})

test_that(test_name(day = 6, part = 2), {
  expect_true(simulate_lanternfish(times, 256) == 26984457539)
})
