suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-14.txt", package = "adventofcode2021")
input <- read_day14(input_file)

part1 <- expand(input$counts, input$rules, 10) |> compute_day14()

print_result(day = 14, part = 1, part1)
check_answer(day = 14, part = 1, part1)

part2 <- expand(input$counts, input$rules, 40) |> compute_day14()

print_result(day = 14, part = 2, part2)
check_answer(day = 14, part = 2, part2)

print_sep()
