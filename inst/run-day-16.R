suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-16.txt", package = "adventofcode2021")

bin <- decode_bits(readLines(input_file)[1])

part1 <- sum_versions(parse_packet(bin))

print_result(day = 16, part = 1, part1)
check_answer(day = 16, part = 1, part1)

part2 <- eval_expression(parse_packet(bin))

print_result(day = 16, part = 2, part2)
check_answer(day = 16, part = 2, part2)

print_sep()
