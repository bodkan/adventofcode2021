suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-24.txt", package = "adventofcode2021")

program <- parse_program(input_file)

part1 <- 97919997299495
run1 <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = 0), program, input = process_input(part1))

print_result(day = 24, part = 1, part1)
check_answer(day = 24, part = 1, part1)

part2 <- 51619131181131
run1 <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = 0), program, input = process_input(part2))

print_result(day = 24, part = 2, part2)
check_answer(day = 24, part = 2, part2)

print_sep()
