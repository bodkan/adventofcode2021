test_that(test_name(day = 25, part = 1, subtitle = "example 1"), {
  start <- read_cucumbers(create_test_file("...>>>>>..."))
  end1 <- read_cucumbers(create_test_file("...>>>>.>.."))
  end2 <- read_cucumbers(create_test_file("...>>>.>.>."))

  # check situation after one step
  expect_equal(move_cucumbers(start), end1)
  # check situation after two steps
  expect_equal(move_cucumbers(move_cucumbers(start)), end2)
})

test_that(test_name(day = 25, part = 1, subtitle = "example 2"), {
  start <- create_test_file("..........
.>v....v..
.......>..
..........") |> read_cucumbers()

  end <- create_test_file("..........
.>........
..v....v>.
..........") |> read_cucumbers()

  expect_equal(move_cucumbers(start), end)
})

test_that(test_name(day = 25, part = 1, subtitle = "example 3 (wraps)"), {
  start <- create_test_file("...>...
.......
......>
v.....>
......>
.......
..vvv..") |> read_cucumbers()

  end <- create_test_file(">......
..v....
..>.v..
.>.v...
...>...
.......
v......") |> read_cucumbers()

  cucumbers <- start
  for (i in 1:4) {
    cucumbers <- move_cucumbers(cucumbers)
  }
  expect_equal(cucumbers, end)
})

test_that(test_name(day = 25, part = 1), {
  start <- create_test_file("v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>") |> read_cucumbers()

  expect_true(find_terminal(start) == 58)
})

# test_that(test_name(day = 25, part = 2), {
#   expect_true( == )
# })
