#' Read the contents of a file with sonnar depth values
read_depths <- function(file) scan(file, what = integer(), quiet = TRUE)

#' Detect which sonnar readings indicate an increase in depth
detect_increases <- function(depths) diff(depths) > 0

#' Calculate sum of depths in a window of a given size
window_depths <- function(depths, window_size) {
  step <- window_size - 1
  indices <- seq_along(depths) |> head(., n = -step)

  window_start <- indices
  window_end <- window_start + step

  vapply(indices, \(i) sum(depths[window_start[i] : window_end[i]]), integer(1))
}
