suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-20.txt", package = "adventofcode2021")

input <- read_trench_map(input_file)

enhanced_image <- enhance_image_mapply(input, steps = 2)

part1 <- sum(enhanced_image == "#")

print_result(day = 20, part = 1, part1)
check_answer(day = 20, part = 1, part1)

enhanced_image <- enhance_image_mapply(input, steps = 50)

part2 <- sum(enhanced_image == "#")

print_result(day = 20, part = 2, part2)
check_answer(day = 20, part = 2, part2)

print_sep()
