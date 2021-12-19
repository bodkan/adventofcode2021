suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-18.txt", package = "adventofcode2021")
input_numbers <- readLines(input_file)

part1 <- compute_magnitude(sum_numbers(input_numbers))

print_result(day = 18, part = 1, part1)
check_answer(day = 18, part = 1, part1)

part2 <- compute_max_magnitude(input_numbers)

print_result(day = 18, part = 2, part2)
check_answer(day = 18, part = 2, part2)

print_sep()
