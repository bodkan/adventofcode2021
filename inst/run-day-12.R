suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-12.txt", package = "adventofcode2021")

part1 <- length(find_paths(input_file, can_enter1))

print_result(day = 12, part = 1, part1)
check_answer(day = 12, part = 1, part1)

part2 <- length(find_paths(input_file, can_enter2))

print_result(day = 12, part = 2, part2)
check_answer(day = 12, part = 2, part2)

print_sep()
