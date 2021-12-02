suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-02.txt", package = "adventofcode2021")

sub1 <- submarine(mode = "v1")
sub1$load_program(input_file)
sub1$execute_program()
result_part1 <- abs(prod(sub1$get_location()))

cat("Day 2 puzzle -- part 1 result: ", result_part1)

cat("\n")

sub2 <- submarine(mode = "v2")
sub2$load_program(input_file)
sub2$execute_program()
result_part2 <- prod(sub2$get_location())

cat("Day 2 puzzle -- part 2 result: ", result_part2)

cat("\n-------\n")
