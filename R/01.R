read_depths <- function(file) scan(file, what = integer(), quiet = TRUE)

detect_increases <- function(depths) diff(depths) > 0

window_depths <- function(depths, window_size) {
  step <- window_size - 1
  indices <- seq_along(depths) |> head(., n = -step)

  window_start <- indices
  window_end <- window_start + step

  vapply(indices, \(i) sum(depths[window_start[i] : window_end[i]]), integer(1))
}
