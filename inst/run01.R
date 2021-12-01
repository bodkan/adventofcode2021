devtools::load_all(".")

input_file <- system.file("extdata/01.txt", package = "adventofcode2021")

depths <- read_depths(input_file)

result_part1 <- depths %>% detect_increases() %>% sum()
cat("Day #1 puzzle -- part 1 result: ", result_part1)

cat("\n")

result_part2 <- depths %>% window_depths(window_size = 3) %>% detect_increases() %>% sum()
cat("Day #1 puzzle -- part 2 result: ", result_part2)

cat("\n-------\n")
