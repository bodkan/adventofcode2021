suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-09.txt", package = "adventofcode2021")

heightmap <- read_heights(input_file)

part1 <- compute_risk(heightmap)

print_result(day = 9, part = 1, part1)
check_answer(day = 9, part = 1, part1)

part2 <- compute_basin_product(heightmap)

print_result(day = 9, part = 2, part2)
check_answer(day = 9, part = 2, part2)

print_sep()
