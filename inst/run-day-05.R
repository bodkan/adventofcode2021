suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-05.txt", package = "adventofcode2021")

lines <- read_lines(input_file, is_horizontal, is_vertical)
points <- lines |> lapply(line_points) |> (\(x) do.call(rbind, x))()

part1 <- count_overlaps(points)

print_result(day = 5, part = 1, part1)
check_answer(day = 5, part = 1, part1)

lines <- read_lines(input_file, is_horizontal, is_vertical, is_diagonal)
points <- lines |> lapply(line_points) |> (\(x) do.call(rbind, x))()

part2 <- count_overlaps(points)

print_result(day = 5, part = 2, part2)
check_answer(day = 5, part = 2, part2)

print_sep()
