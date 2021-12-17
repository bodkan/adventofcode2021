suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-17.txt", package = "adventofcode2021")
area <- read_target_area(input_file)

part1 <- optimize_height(area, init = c(1, 1), step_x = 50, step_y = 50, maxit = 20000)

print_result(day = 17, part = 1, part1$value)
check_answer(day = 17, part = 1, part1$value)

part2 <- count_valid_velocities(area, max_height = part1$value)

print_result(day = 17, part = 2, part2)
check_answer(day = 17, part = 2, part2)

print_sep()
