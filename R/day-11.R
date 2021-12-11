count_flashes <- function(m) {
  n_flashes <- 0

  for (step in seq_len(100)) {
    new_flashes <- flashes <- matrix(FALSE,
                                     nrow = nrow(m),
                                     ncol = ncol(m))
    m <- m + 1

    repeat {
      new_flashes <- m > 9 & !flashes
      flashes <- flashes | new_flashes

      for (dir in c("up", "down", "left", "right",
                    "upleft", "upright",
                    "downleft", "downright")) {
        adjacent_flashes <- shift(new_flashes, dir, FALSE)
        m[adjacent_flashes] <- m[adjacent_flashes] + 1
      }

      if (!any(new_flashes)) break
    }

    m[flashes] <- 0

    n_flashes <- n_flashes + sum(flashes)
  }

  n_flashes
}

detect_synchronized <- function(m) {
  step <- 1

  while (TRUE) {
    new_flashes <- flashes <- matrix(FALSE,
                                     nrow = nrow(m),
                                     ncol = ncol(m))
    m <- m + 1

    repeat {
      new_flashes <- m > 9 & !flashes
      flashes <- flashes | new_flashes

      for (dir in c("up", "down", "left", "right",
                    "upleft", "upright",
                    "downleft", "downright")) {
        adjacent_flashes <- shift(new_flashes, dir, FALSE)
        m[adjacent_flashes] <- m[adjacent_flashes] + 1
      }

      if (!any(new_flashes)) break
    }

    m[flashes] <- 0

    if (all(flashes))
      return(step)
    else
      step <- step + 1
  }
}
