test_file <- create_test_file("16,1,2,0,4,2,7,1,2,14")

crabs <- read_crabs(test_file)
options <- seq(min(crabs), max(crabs))

test_that(test_name(day = 7, part = 1), {
  fuel <- compute_fuel(crabs, options)

  expect_true(min(fuel) == 37)
  i <- 1; expect_true(fuel[i + 1] == 41)
  i <- 3; expect_true(fuel[i + 1] == 39)
  i <- 10; expect_true(fuel[i + 1] == 71)
})

test_that(test_name(day = 7, part = 2), {
  fuel <- compute_fuel(crabs, options, incremental_fuel)

  expect_true(min(fuel) == 168)
  i <- 2; expect_true(fuel[i + 1] == 206)
})
