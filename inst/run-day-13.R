suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-13.txt", package = "adventofcode2021")

input <- read_day13(input_file)

part1 <- fold_paper(input$paper, input$folds[[1]]) |> sum()

print_result(day = 13, part = 1, part1)
check_answer(day = 13, part = 1, part1)

part2 <- fold_all(input$paper, input$folds) |> decode_day13()

print_result(day = 13, part = 2, "")
cat("", paste0(part2, "\n"))
check_answer(day = 13, part = 2, part2)

print_sep()
