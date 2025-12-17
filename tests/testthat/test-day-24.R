test_that(test_name(day = 24, part = 1, subtitle = "example 1"), {
  file <- create_test_file("inp x
mul x -1")

  program <- parse_program(file)

  alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)
  expect_equal(run_program(alu, program, 1), list(w = 0, x = -1, y = 0, z = 0))

  alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)
  expect_equal(run_program(alu, program, 42), list(w = 0, x = -42, y = 0, z = 0))

  alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)
  expect_equal(run_program(alu, program, -42), list(w = 0, x = 42, y = 0, z = 0))
})

test_that(test_name(day = 24, part = 1, subtitle = "example 2"), {
  file <- create_test_file("inp z
inp x
mul z 3
eql z x")

  program <- parse_program(file)

  alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)
  expect_equal(run_program(alu, program, c(1, 3)), list(w = 0, x = 3, y = 0, z = 1))
})

test_that(test_name(day = 24, part = 1, subtitle = "example 2"), {
  file <- create_test_file("inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2")
  program <- parse_program(file)

  alu <- initialize_alu(w = 0, x = 0, y = 0, z = 0)

  expect_equal(run_program(alu, program, 0), list(w = 0, x = 0, y = 0, z = 0))
  expect_equal(run_program(alu, program, 1), list(w = 0, x = 0, y = 0, z = 1))
  expect_equal(run_program(alu, program, 2), list(w = 0, x = 0, y = 1, z = 0))
  expect_equal(run_program(alu, program, 3), list(w = 0, x = 0, y = 1, z = 1))
  expect_equal(run_program(alu, program, 4), list(w = 0, x = 1, y = 0, z = 0))
  expect_equal(run_program(alu, program, 8), list(w = 1, x = 0, y = 0, z = 0))
  expect_equal(run_program(alu, program, 10), list(w = 1, x = 0, y = 1, z = 0))
})


# test_that(test_name(day = 24, part = 1), {
#   expect_true( == )
# })
#
# test_that(test_name(day = 24, part = 2), {
#   expect_true( == )
# })
