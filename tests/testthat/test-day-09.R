test_file <- create_test_file("2199943210
3987894921
9856789892
8767896789
9899965678")

heightmap <- read_heights(test_file)

test_that(test_name(day = 9, part = 1), {
  expect_true(compute_risk(heightmap) == 15)
})

test_that(test_name(day = 9, part = 2), {
  expect_true(compute_basin_product(heightmap) == 1134)
})
