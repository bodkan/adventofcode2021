suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-15.txt", package = "adventofcode2021")

map <- read_day15_map(input_file)

part1 <- compute_cost(map)

print_result(day = 15, part = 1, part1)
check_answer(day = 15, part = 1, part1)

tiled_map <- tile_map(map)

part2 <- compute_cost(tiled_map, log = F)
print_result(day = 15, part = 2, part2)
check_answer(day = 15, part = 2, part2)

print_sep()
