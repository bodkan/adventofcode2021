test_name <- function(day, part, subtitle = "") {
  if (subtitle != "") subtitle <- sprintf("(%s)", subtitle)
  sprintf("Day %d, part %d adheres to the specification %s", day, part, subtitle)
}

# Save the example string input from Advent of Code to a file
create_test_file <- function(test_input) {
  test_file <- tempfile()
  writeLines(test_input, test_file)
  test_file
}
