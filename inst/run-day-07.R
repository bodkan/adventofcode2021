suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-07.txt", package = "adventofcode2021")

crabs <- read_crabs(input_file)
options <- seq(min(crabs), max(crabs))

part1 <- compute_fuel(crabs, options)

print_result(day = 7, part = 1, part1)
check_answer(day = 7, part = 1, part1)

part2 <- compute_fuel(crabs, options)

print_result(day = 7, part = 2, part2)
check_answer(day = 7, part = 2, part2)

print_sep()
