suppressMessages(devtools::load_all("."))

options(scipen = 999)

part1 <-
  system.file("extdata/day-22.txt", package = "adventofcode2021") |>
  read_steps() |>
  filter_cuboids(min = -50, max = 50) |>
  process_all() |>
  count_on()

print_result(day = 22, part = 1, part1)
check_answer(day = 22, part = 1, part1)

part2 <-
  system.file("extdata/day-22.txt", package = "adventofcode2021") |>
  read_steps() |>
  process_all(quiet = FALSE) |>
  count_on()

print_result(day = 22, part = 2, part2)
check_answer(day = 22, part = 2, part2)

print_sep()
