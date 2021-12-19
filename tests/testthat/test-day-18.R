file1 <- create_test_file("[1,1]
[2,2]
[3,3]
[4,4]")

file2 <- create_test_file("[1,1]
[2,2]
[3,3]
[4,4]
[5,5]")

file3 <- create_test_file("[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]")

file4 <- create_test_file("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]")

file5 <- create_test_file("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

test_that(test_name(day = 18, part = 1, "parsing and printing numbers"), {
  numbers <- c(
    "[1,2]",
    "[[1,2],3]",
    "[9,[8,7]]",
    "[[1,9],[8,5]]",
    "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]",
    "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]",
    "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]",
    "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
    "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    "[7,[5,[[3,8],[1,4]]]]",
    "[[2,[2,2]],[8,[8,1]]]",
    "[2,9]",
    "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    "[[[5,[7,4]],7],1]",
    "[[[[4,2],2],6],[8,7]]"
  )
  # test that parsing and printing of numbers are proper inverse functions
  expect_true(all(sapply(numbers, \(x) x == print_number(parse_number(x)))))
})

test_that(test_name(day = 18, part = 1, "exploding numbers"), {
  expect_equal(explode_number("[[[[[9,8],1],2],3],4]"), "[[[[0,9],2],3],4]")
  expect_equal(explode_number("[7,[6,[5,[4,[3,2]]]]]"), "[7,[6,[5,[7,0]]]]")
  expect_equal(explode_number("[[6,[5,[4,[3,2]]]],1]"), "[[6,[5,[7,0]]],3]")
  expect_equal(explode_number("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"),
               "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
  expect_equal(explode_number("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
               "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
})

test_that(test_name(day = 18, part = 1, "exploding and splitting numbers"), {
  # manual reduction
  expect_equal({
    add_number("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]") |>
    explode_number() |>
    explode_number() |>
    split_number() |>
    split_number() |>
    explode_number()
  },
  "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

  # automatic reduction
  expect_equal(
    add_number("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]") |> reduce_number(),
    "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
  )
})

test_that(test_name(day = 18, part = 1, "adding numbers"), {
  n1 <- readLines(file1)
  expect_equal(sum_numbers(n1), "[[[[1,1],[2,2]],[3,3]],[4,4]]")
  n2 <- readLines(file2)
  expect_equal(sum_numbers(n2), "[[[[3,0],[5,3]],[4,4]],[5,5]]")
  n3 <- readLines(file3)
  expect_equal(sum_numbers(n3), "[[[[5,0],[7,4]],[5,5]],[6,6]]")
  n4 <- readLines(file4)
  expect_equal(sum_numbers(n4), "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
})

test_that(test_name(day = 18, part = 1), {
  expect_equal(compute_magnitude(sum_numbers(readLines(file5))), 4140)
})

test_that(test_name(day = 18, part = 2), {
  expect_true(compute_max_magnitude(readLines(file5)) == 3993)
})
