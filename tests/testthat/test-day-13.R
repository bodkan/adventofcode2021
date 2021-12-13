test_file <- create_test_file("6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

input <- read_day13(test_file)
m <- input$paper
folds <- input$folds

test_that(test_name(day = 13, part = 1), {
  expect_true(sum(fold_all(m, folds)) == 16)
})

# test_that(test_name(day = 13, part = 2), {
#   expect_true( == )
# })
