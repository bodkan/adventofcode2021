# file <- create_test_file("")

p1 <- 0; pos_p1 <- 10;
p2 <- 0; pos_p2 <- 9

pos_max <- 10

die <- 1
die_max <- 100
die_counter <- 0

while (TRUE) {
  # browser()
  # p1
  die_add <- 0
  for (i in 1:3) {
    cat(die, " ")
    die_add <- die_add + die
    die <- die + 1
    if (die > die_max) die <- 1
    die_counter <- die_counter + 1
  }
  p1_shift <- die_add %% pos_max
  cat("die add ", die_add, "\n")
  pos_p1 <- (pos_p1 + p1_shift) %% pos_max
  if (pos_p1 == 0) pos_p1 <- 10
  p1 <- p1 + pos_p1
  if (p1 >= 1000) break

  # p2
  die_add <- 0
  for (i in 1:3) {
    cat(die, " ")
    die_add <- die_add + die
    die <- die + 1
    if (die > die_max) die <- 1
    die_counter <- die_counter + 1
  }
  p2_shift <- die_add %% pos_max
  cat("die add ", die_add, "\n")
  pos_p2 <- (pos_p2 + p2_shift) %% pos_max
  if (pos_p2 == 0) pos_p2 <- 10
  p2 <- p2 + pos_p2
  if (p2 >= 1000) break

  cat("p1 ", p1, "-", pos_p1, "   ", "p2 ", p2, "-", pos_p2, "\n")
}
cat(p1, " vs ", p2, "\n")
cat(min(p1, p2) * die_counter)



# test_that(test_name(day = 21, part = 1), {
#   expect_true( == )
# })
#
# test_that(test_name(day = 21, part = 2), {
#   expect_true( == )
# })
