suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-24.txt", package = "adventofcode2021")

program <- parse_program(input_file)
input_number <- process_input(13579246899999)
input_number <- process_input(99999999999999)
run_program(program, input_number, debug = TRUE)

part1 <-

print_result(day = 24, part = 1, part1)
check_answer(day = 24, part = 1, part1)

part2 <-

print_result(day = 24, part = 2, part2)
check_answer(day = 24, part = 2, part2)

print_sep()
