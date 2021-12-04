test_file <- create_test_file("00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

m <- read_matrix(test_file)

test_that(test_name(day = 3, part = 1), {
  gamma <- compute_gamma_rate(m)
  epsilon <- compute_epsilon_rate(m)

  expect_true(gamma * epsilon == 198)
})

test_that(test_name(day = 3, part = 2), {
  oxygen_rating <- compute_oxygen_rating(m)
  co2_rating <- compute_co2_rating(m)

  expect_true(oxygen_rating * co2_rating == 230)
})
