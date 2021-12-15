test_file <- create_test_file("1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

map <- read_day15_map(test_file)

test_that(test_name(day = 15, part = 1), {
  expect_true(compute_cost(map) == 40)
})

test_that(test_name(day = 15, part = 2), {
  tiled_map <- tile_map(map)
  expect_true(compute_cost(tiled_map) == 315)
})

tiled_map <- tile_map(map)
compute_cost(tiled_map)

