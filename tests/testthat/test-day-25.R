file1 <- create_test_file("v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")

file2 <- create_test_file("...>>>>>...")

cucumbers <- read_cucumbers(file2)

move <- function(cucumbers, direction) {
  # extract matrices of eastward- and southward-moving cucumbers
  e <- cucumbers$east
  s <- cucumbers$south

  # add a new dummy right column wrap
  e <- cbind(e, e[, 1])
  # e <- e[1:2, , drop = FALSE]
  e

  # similarly, add a new bottom column wrap
  s <- cbind(s, s[, 1])
  # s <- s[1:2, , drop = FALSE]
  s

  # eastward movement first

  # get indicators of cucumbers which moved left in a vectorized matrix operation
  emoving <- (t(diff(t(e | s))) == -1) & e[, -ncol(e)]
  emoving

  # those who don't move remain static, by definition
  estatic <- e[, -ncol(e)] & !emoving
  estatic

  # careful, we can't afford to lose cucumbers!
  stopifnot(sum(e) == sum(emoving) + sum(estatic))

  # shift eastward-moving cucumbers to their new indices
  eshifted <- emoving[, c(ncol(emoving), 1:(ncol(emoving) - 1)), drop = FALSE]
  eshifted

  # combine the matrices of static and moved cucumbers back to a single matrix
  e <- estatic | eshifted

  list(east = e, south = s[, -ncol(s), drop = FALSE])
}

cucumbers
move(.Last.value)
move(.Last.value)



# YOU LEFT AT A STAGE IN WHICH THE SHIFTING OF EASTWARD CUCUMBERS SEEMS
# TO WORK CORRECTLY (THE COUPLE OF LINES ABOVE)--YOU GET true WHERE A
# CUCUMBER COULD'VE MOVED EASTWARD--BUT EVERYTHING ELSE IS false WHICH
# IS OBVIOUSLY WRONG (SOME CUCUMBERS CANNOT MOVE BUT THEY SHOULDN'T
# BE MARKED AS false -- THEY SHOULD BE PERHAPS COMPUTED SEPARATELY AND
# THEN MERGED WITH THOSE THAT MOVED USING THE | OPERATOR?)








# test_that(test_name(day = 25, part = 1), {
#   expect_true( == )
# })
#
# test_that(test_name(day = 25, part = 2), {
#   expect_true( == )
# })
