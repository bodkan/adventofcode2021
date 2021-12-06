suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-06.txt", package = "adventofcode2021")

times <- read_lanternfish(input_file)

part1 <- simulate_lanternfish(times, 80)

print_result(day = 6, part = 1, part1)
check_answer(day = 6, part = 1, part1)

part2 <- simulate_lanternfish(times, 256)

print_result(day = 6, part = 2, part2)
check_answer(day = 6, part = 2, part2)

print_sep()
