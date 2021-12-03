suppressMessages(devtools::load_all("."))

input_file <- system.file("extdata/day-03.txt", package = "adventofcode2021")

m <- read_matrix(input_file)

gamma <- compute_gamma_rate(m)
epsilon <- compute_epsilon_rate(m)

part1 <- gamma * epsilon

print_result(day = 3, part = 1, part1)

oxygen <- compute_oxygen_rating(m)
co2 <- compute_co2_rating(m)

part2 <- oxygen * co2

print_result(day = 3, part = 2, part2)
