suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-02.txt", package = "adventofcode2021")

sub <- submarine()
sub$load_program(input_file)
sub$execute_program()
result_part1 <- prod(sub$get_location())

cat("Day 2 puzzle -- part 1 result: ", result_part1)

cat("\n")

sub2 <- submarine2()
sub2$load_program(input_file)
sub2$execute_program()
result_part2 <- prod(sub2$get_location())

cat("Day 2 puzzle -- part 2 result: ", result_part2)

cat("\n-------\n")
