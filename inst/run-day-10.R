suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-10.txt", package = "adventofcode2021")

part1 <- report_errors(input_file)

print_result(day = 10, part = 1, part1)
check_answer(day = 10, part = 1, part1)

part2 <- report_autocomplete(input_file)

print_result(day = 10, part = 2, part2)
check_answer(day = 10, part = 2, part2)

print_sep()
