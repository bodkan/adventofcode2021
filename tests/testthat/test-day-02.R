test_input <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
test_file <- tempfile()
writeLines(test_input, test_file)

test_that(test_name(day = 2, part = 1), {
  sub <- submarine(mode = "v1")
  sub$load_program(test_file)
  sub$execute_program()
  location <- sub$get_location()
  expect_true(abs(prod(location)) == 150)
})

test_that(test_name(day = 2, part = 1), {
  sub <- submarine(mode = "v2")
  sub$load_program(test_file)
  sub$execute_program()
  location <- sub$get_location()
  expect_true(abs(prod(location)) == 900)
})
