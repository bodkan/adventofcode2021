test_that(test_name(day = 21, part = 1), {
  expect_true(part_1_game(4, 8) == 739785)
})

test_that(test_name(day = 21, part = 2), {
  expect_true(part_2_game(4, 8) == 444356092776315)
})
