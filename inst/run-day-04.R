suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-04.txt", package = "adventofcode2021")

input_bingo <- read_bingo(input_file)

part1 <- solve_bingo(input_bingo$draws, input_bingo$bingos)

print_result(day = 4, part = 1, part1)

part2 <- solve_bingo2(input_bingo$draws, input_bingo$bingos)

print_result(day = 4, part = 2, part2)

print_sep()
