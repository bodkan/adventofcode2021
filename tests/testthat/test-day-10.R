test_file <- create_test_file("[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

test_that(test_name(day = 10, part = 1), {
  expect_true(report_errors(test_file) == 26397)
})


test_that(test_name(day = 10, part = 2), {
  expect_true(report_autocomplete(test_file) == 288957)
})
