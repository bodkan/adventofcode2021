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

      increment <-
        shift(new_flashes, "up", FALSE) +
        shift(new_flashes, "down", FALSE) +
        shift(new_flashes, "left", FALSE) +
        shift(new_flashes, "right", FALSE) +
        shift(new_flashes, "upleft", FALSE) +
        shift(new_flashes, "upright", FALSE) +
        shift(new_flashes, "downleft", FALSE) +
        shift(new_flashes, "downright", FALSE)
      m <- m + increment

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

      increment <-
        shift(new_flashes, "up", FALSE) +
        shift(new_flashes, "down", FALSE) +
        shift(new_flashes, "left", FALSE) +
        shift(new_flashes, "right", FALSE) +
        shift(new_flashes, "upleft", FALSE) +
        shift(new_flashes, "upright", FALSE) +
        shift(new_flashes, "downleft", FALSE) +
        shift(new_flashes, "downright", FALSE)
      m <- m + increment

      if (!any(new_flashes)) break
    }

    m[flashes] <- 0

    if (all(flashes))
      return(step)
    else
      step <- step + 1
  }
}
