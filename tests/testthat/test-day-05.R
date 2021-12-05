test_file <- create_test_file("0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

test_that(test_name(day = 5, part = 1), {
  lines <- read_lines(test_file, is_horizontal, is_vertical)
  points <- lines |> lapply(line_points) |> (\(x) do.call(rbind, x))()
  expect_true(count_overlaps(points) == 5)
})

test_that(test_name(day = 5, part = 2), {
  lines <- read_lines(test_file, is_horizontal, is_vertical, is_diagonal)
  points <- lines |> lapply(line_points) |> (\(x) do.call(rbind, x))()
  expect_true(count_overlaps(points) == 12)
})
