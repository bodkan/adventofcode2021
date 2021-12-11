test_file <- create_test_file("5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

m <- read_matrix(test_file)

test_that(test_name(day = 11, part = 1), {
  expect_true(count_flashes(m) == 1656)
})

test_that(test_name(day = 11, part = 2), {
  expect_true(detect_synchronized(m) == 195)
})
