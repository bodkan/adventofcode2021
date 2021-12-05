suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-05.txt", package = "adventofcode2021")

lines <- read_lines(input_file, is_horizontal, is_vertical)
points <- lines |> lapply(line_coords) |> (\(x) do.call(rbind, x))()

# part1a <- count_overlaps1(points)

map <- make_map(lines)
part1b <- count_overlaps2(map, points)

# print_result(day = 5, part = 1, part1a)
# check_answer(day = 5, part = 1, part1a)

print_result(day = 5, part = 1, part1b)
check_answer(day = 5, part = 1, part1b)

lines <- read_lines(input_file, is_horizontal, is_vertical, is_diagonal)
points <- lines |> lapply(line_coords) |> (\(x) do.call(rbind, x))()
map <- make_map(lines)

part2 <- count_overlaps2(map, points)

print_result(day = 5, part = 2, part2)
check_answer(day = 5, part = 2, part2)

print_sep()
