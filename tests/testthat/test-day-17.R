file <- create_test_file("target area: x=20..30, y=-10..-5")

area <- read_target_area(file)

test_that(test_name(day = 17, part = 1), {
  # couple of examples of maximum heights reached from the puzzle "ASCII images"
  expect_true(maximum_height(compute_trajectory(velocity = c(7, 2), area)) == 3)
  expect_true(maximum_height(compute_trajectory(velocity = c(6, 3), area)) == 6)
  expect_true(maximum_height(compute_trajectory(velocity = c(9, 0), area)) == 0)
  expect_true(maximum_height(compute_trajectory(velocity = c(17, -4), area)) == -Inf)
  expect_true(maximum_height(compute_trajectory(area, velocity = c(6, 9))) == 45)
  optim_result <- optimize_height(area, init = c(1, 1), step_x = 1, step_y = 1)
  expect_true(optim_result$value == 45)
})

test_that(test_name(day = 17, part = 2), {
  expect_true(count_valid_velocities(area, max_height = 45) == 112)
})
