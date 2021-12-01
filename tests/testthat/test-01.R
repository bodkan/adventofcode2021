# generate test input file
test_input <- "199
200
208
210
200
207
240
269
260
263"
test_file <- tempfile()
writeLines(test_input, test_file)

depths <- read_depths(test_file)

# test part 1
expect_equal(7,  depths %>% detect_increases() %>% sum())

# test part 2
expect_equal(5,  depths %>% window_depths(window_size = 3) %>% detect_increases() %>% sum())
