# Read eastward- and southward-moving cucumbers as two separate
# logical matrices (both sets of cucumbers will be later moved
# independently)
read_cucumbers <- function(file) {
  split_lines <- lapply(readLines(file), \(l) strsplit(l, "")[[1]])
  east <- do.call(rbind, lapply(split_lines, \(x) x == ">"))
  south <- do.call(rbind, lapply(split_lines, \(x) x == "v"))
  list(east = east, south = south)
}

# Given a pair of binary matrices (indicators of where eastward- and
# southward-moving cucumbers are), compute their next positions using
# a series of fully vectorized matrix operations
move_cucumbers <- function(cucumbers) {
  # extract matrices of eastward- and southward-moving cucumbers
  e <- cucumbers$east
  s <- cucumbers$south

  ##############################
  # eastward movement first

  # add a new dummy right east column wrap
  e <- cbind(e, e[, 1])
  e

  # similarly, add a new right south column wrap
  s <- cbind(s, s[, 1])
  s

  # get indicators of cucumbers which moved left using a vectorized matrix operation
  emoving <- (t(diff(t(e | s))) == -1) & e[, -ncol(e)]
  emoving

  # those who don't move remain static, by definition
  estatic <- e[, -ncol(e)] & !emoving
  estatic

  # careful, we can't afford to lose cucumbers!
  stopifnot(sum(cucumbers$east) == sum(emoving) + sum(estatic))

  # shift eastward-moving cucumbers to their new indices
  # (unique-ing the column matrix indices to take care of one-column cases)
  eshifted <- emoving[, unique(c(ncol(emoving), 1:(ncol(emoving) - 1))), drop = FALSE]
  eshifted

  # combine the matrices of static and moved cucumbers back to a single matrix
  e <- estatic | eshifted

  ##############################
  # reset state

  # remove the dummy right column introduced while processing the
  # eastward-moving cucumbers in the previous step
  s <- s[, -ncol(s), drop = FALSE]

  ##############################
  # southward movement

  # add a new dummy bottom east row wrap
  e <- rbind(e, e[1, ])
  e

  # similarly, add a new bottom south row wrap
  s <- rbind(s, s[1, ])
  s

  # get indicators of cucumbers which moved south using a vectorized matrix operation
  smoving <- (diff(e | s) == -1) & s[-nrow(s), ]
  smoving

  # those who don't move remain static, by definition
  sstatic <- s[-nrow(s), ] & !smoving
  sstatic

  # careful, we can't afford to lose cucumbers!
  stopifnot(sum(cucumbers$south) == sum(smoving) + sum(sstatic))

  # shift southward-moving cucumbers to their new indices
  # (unique-ing the row matrix indices to take care of one-row cases)
  sshifted <- smoving[unique(c(nrow(smoving), 1:(nrow(smoving) - 1))), , drop = FALSE]
  sshifted

  # combine the matrices of static and moved cucumbers back to a single matrix
  s <- sstatic | sshifted

  list(east = e[-nrow(e), , drop = FALSE], south = s)
}

# Get the number of iterations needed for cucumbers to remain locked
# in their places
find_terminal <- function(cucumbers) {
  i <- 1
  current <- cucumbers
  repeat {
    following <- move_cucumbers(current)

    if (all(current$east == following$east) && all(current$south == following$south))
      break

    current <- following
    i <- i + 1
  }
  i
}
