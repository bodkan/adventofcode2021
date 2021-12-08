suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-08.txt", package = "adventofcode2021")

input <- read_digits(input_file)

digits <- sapply(input, \(x) x$digits)
counts <- lapply(digits, function(d) sapply(d, nchar)) |> unlist()

part1 <- sum(counts == 2 | counts == 3 | counts == 4 | counts == 7)

print_result(day = 8, part = 1, part1)
check_answer(day = 8, part = 1, part1)

part2 <- sapply(input, assign_digits) |> sum()

print_result(day = 8, part = 2, part2)
check_answer(day = 8, part = 2, part2)

print_sep()
