
read_scanners <- function(file) {
  lines <- readLines(file) |> (\(lines) Filter(\(x) x != "", lines))()
  delimiters <- c(grep("scanner", lines), length(lines))
  lapply(seq_along(delimiters)[-1], function(i) {
    coords <- lines[delimiters[i - 1]:delimiters[i]]
    coords[-c(1, length(coords))] |>
      strsplit(",") |>
      unlist() |>
      as.numeric() |>
      matrix(ncol = 3, byrow = TRUE)
  })
}

compute_distances <- function(cube) {
  distances <- array(NA, dim = c(nrow(cube), nrow(cube), 3))
  for (i in seq_len(nrow(cube))) {
    for (j in seq_len(nrow(cube))) {
      distances[i, j, ] <- abs(cube[i, ] - cube[j, ])
    }
  }
  distances
}

find_overlaps <- function(c1, c2) {
  overlapping <- list()
  for (i in seq(1, dim(c2)[1])) {
    for (j in seq(i, dim(c2)[2])) {
      if (i == j) next
      for (k in seq(1, dim(c1)[1])) {
        for (l in seq(k, dim(c1)[2])) {
          if (k == l) next
          if (all(sort(c2[i, j, ]) == sort(c1[k, l, ]))) {
            overlapping[[length(overlapping) + 1]] <- c(i = i, j = j, k = k, l = l)
          }
        }
      }
    }
  }
  if (length(overlapping) < 12) return(NULL)
  # if (!length(overlapping)) return(NULL)

  overlapping <- overlapping |> unlist() |> matrix(ncol = 4, byrow = TRUE)

  c1_beacons <- unique(as.integer(overlapping[, 3:4])) |> sort()
  c2_beacons <- unique(as.integer(overlapping[, 1:2])) |> sort()

  matches <- rep(NA, dim(c1)[1])
  for (kl in c1_beacons) {
    kl_matches <- overlapping[overlapping[, 3] == kl | overlapping[, 4] == kl, 1:2]
    set_a <- kl_matches[3, ]
    set_b <- kl_matches[4, ]
    matches[kl] <- intersect(set_a, set_b)
  }

  matches
}
