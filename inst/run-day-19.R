suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-19.txt", package = "adventofcode2021")

cubes <- read_scanners(input_file)
result <- align_cubes(cubes, debug = FALSE)

beacons <- dedupe_beacons(result$aligned)
part1 <- nrow(beacons)

print_result(day = 19, part = 1, part1)
check_answer(day = 19, part = 1, part1)

locations <- do.call(rbind, result$locations)

rel_distances <- compute_distances(locations)
manhattan_distances <- rowSums(rel_distances, dims = 2)

part2 <- max(manhattan_distances)

print_result(day = 19, part = 2, part2)
check_answer(day = 19, part = 2, part2)

print_sep()
