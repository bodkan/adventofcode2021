
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
  if (!length(overlapping)) return(NULL)

  overlapping <- overlapping |> unlist() |> matrix(ncol = 4, byrow = TRUE)

  if (length(unique(as.vector(overlapping[, 3:4]))) < 12) return(NULL)

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

permute_axes <- function(x, i) {
  perm_i <- (i - 1) %% 6 + 1
  rot_i <- floor((i - 1) / 8) + 1

  permutations <- list(
    c(1, 2, 3),
    c(2, 3, 1),
    c(3, 1, 2),
    c(1, 3, 2),
    c(3, 2, 1),
    c(2, 1, 3)
  )
  rotations <- list(
    c(1, 1, 1),
    c(-1, -1, 1),
    c(1, -1, -1),
    c(-1, 1, -1),

    c(-1, -1, -1),
    c(1, 1, -1),
    c(-1, 1, 1),
    c(1, -1, 1)
  )

  # cat(perm_i, " ", rot_i, "\n")
  sweep(x, MARGIN = 2, rotations[[rot_i]], "*")[, permutations[[perm_i]], drop = FALSE]
}

determine_orientation <- function(cube1, cube2) {
  u <- cube1[1, , drop = FALSE] - cube1[2, , drop = FALSE]
  v <- cube2[1, , drop = FALSE] - cube2[2, , drop = FALSE]

  for (i in 1:48) {
    v_ <- permute_axes(v, i)
    if (length(unique(as.vector(u / v_))) == 1 && all(sign(u) == sign(v_))) break
  }

  if (!(length(unique(as.vector(u / v_))) == 1 && all(sign(u) == sign(v_))))
    return(-1)
  else
    return(i)
}
