test_name <- function(day, part) {
  sprintf("Day %d, part %d adheres to the specification", day, part)
}

# Save the example string input from Advent of Code to a file
create_test_file <- function(test_input) {
  test_file <- tempfile()
  writeLines(test_input, test_file)
  test_file
}
