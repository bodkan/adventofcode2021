suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-11.txt", package = "adventofcode2021")
m <- read_matrix(input_file)

part1 <- count_flashes(m)

print_result(day = 11, part = 1, part1)
# check_answer(day = 11, part = 1, part1)

part2 <- detect_synchronized(m)

print_result(day = 11, part = 2, part2)
# check_answer(day = 11, part = 2, part2)

print_sep()
