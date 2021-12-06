read_lanternfish <- function(file) {
  # read integer times and convert them to a factor (to force unobserved times
  # to 0 during counting)
  times <- scan(file, what = integer(), sep = ",", quiet = TRUE) |>
    factor(levels = seq(0, 8))
  counts <- table(times) |> as.numeric()
  counts
}

simulate_lanternfish <- function(times, generations) {
  n <- length(times)

  # indices into fish counter vector in R's 1-based coordinates
  reproducing <- 1
  reset <- 7
  offspring <- 9

  for (gen in seq_len(generations)) {
    # get the number of reproducing fish in current generation
    n_reproducing <- times[reproducing]

    # shift the vector of times by one
    times[-n] <- times[2:n]

    # reset fish which just reproduced and add new fish to the counter
    times[reset] <- times[reset] + n_reproducing
    times[offspring] <- n_reproducing
  }

  # return the count of all fish at the end of the simulation
  sum(times)
}
