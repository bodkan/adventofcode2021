suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-03.txt", package = "adventofcode2021")

m <- read_matrix(input_file)

gamma <- compute_gamma_rate(m)
epsilon <- compute_epsilon_rate(m)
result_part1 <- gamma * epsilon

cat("Day 3 puzzle -- part 1 result: ", result_part1)

cat("\n")

oxygen <- compute_oxygen_rating(m)
co2 <- compute_co2_rating(m)
result_part2 <- oxygen * co2

cat("Day 2 puzzle -- part 2 result: ", result_part2)

cat("\n-------\n")
