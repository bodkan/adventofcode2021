suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-02.txt", package = "adventofcode2021")

sub1 <- submarine(mode = "v1")
sub1$load_program(input_file)
sub1$execute_program()

part1 <- abs(prod(sub1$get_location()))

print_result(day = 2, part = 1, part1)
check_answer(day = 2, part = 1, part1)

sub2 <- submarine(mode = "v2")
sub2$load_program(input_file)
sub2$execute_program()

part2 <- prod(sub2$get_location())

print_result(day = 2, part = 2, part2)
check_answer(day = 2, part = 2, part2)

print_sep()
