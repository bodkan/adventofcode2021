suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-01.txt", package = "adventofcode2021")

depths <- read_depths(input_file)

part1 <- depths |> detect_increases() |> sum()

print_result(day = 1, part = 1, part1)
check_answer(day = 1, part = 1, part1)

part2 <- depths |> window_depths(window_size = 3) |> detect_increases() |> sum()

print_result(day = 1, part = 2, part2)
check_answer(day = 1, part = 2, part2)

print_sep()
