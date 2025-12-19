suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-24.txt", package = "adventofcode2021")

program <- parse_program(input_file)

# input_number <- process_input(13579246899999)
#
# init_alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)
# run_program(init_alu, program, input_number, debug = F)

# # get coordinates of "subprograms" within the large puzzle program and
# # split them into a list, one element for each subprogram
# parts <- c(which(sapply(program, \(x) x$instruction == "inp")), length(program) + 1)
# subprograms <- lapply(seq_len(length(parts) - 1), \(i) program[parts[i] : (parts[i + 1] - 1)])

part1 <- 97919997299495
run1 <- run_program(initialize_alu(w = 0, x = 0, y = 0, z = 0), program, input = process_input(part1))

print_result(day = 24, part = 1, part1)
check_answer(day = 24, part = 1, part1)

# part2 <-
#
# print_result(day = 24, part = 2, part2)
# check_answer(day = 24, part = 2, part2)
#
# print_sep()
