suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-19.txt", package = "adventofcode2021")

cubes <- read_scanners(input_file)
aligned <- align_cubes(cubes, debug = TRUE)
beacons <- dedupe_beacons(aligned)

part1 <- nrow(beacons)

print_result(day = 19, part = 1, part1)
# check_answer(day = 19, part = 1, part1)
#
# part2 <-
#
# print_result(day = 19, part = 2, part2)
# check_answer(day = 19, part = 2, part2)
#
# print_sep()
