file <- create_test_file("NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

input <- read_day14(file)

test_that(test_name(day = 14, part = 1), {
  counts <- expand(input$counts, input$rules, 10)
  expect_true(compute_day14(counts) == 1588)
})

test_that(test_name(day = 14, part = 2), {
  counts <- expand(input$counts, input$rules, 40)
  expect_true(compute_day14(counts) == 2188189693529)
})
