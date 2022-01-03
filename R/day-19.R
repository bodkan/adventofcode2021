read_scanners <- function(file) {
  lines <- readLines(file) |> (\(lines) Filter(\(x) x != "", lines))()
  delimiters <- c(grep("scanner", lines), length(lines) + 1)
  lapply(seq_along(delimiters)[-1], function(i) {
    coords <- lines[delimiters[i - 1]:delimiters[i]]
    coords[-c(1, length(coords))] |>
      strsplit(",") |>
      unlist() |>
      as.numeric() |>
      matrix(ncol = 3, byrow = TRUE)
  })
}

combinations <- expand.grid(1:6, 1:8)

permutations <- list(
  c(1, 2, 3),
  c(2, 3, 1),
  c(3, 1, 2),
  c(3, 2, 1),
  c(1, 3, 2),
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

# This is clearly not wrong but it's inefficient and unnecessary. Find
# a smarter way to do the transformation based on real matrix algebra:
# https://preview.redd.it/55v2qywirk681.png?width=942&format=png&auto=webp&s=d3609a802ace1199c6f62616e5b02cc78663a69e
permute_axes <- function(x, i) {
  p <- permutations[[combinations[i, 1]]]
  r <- rotations[[combinations[i, 2]]]

  sweep(x, MARGIN = 2, r, "*")[, p, drop = FALSE]
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
    for (j in seq(1, i)) {
      if (i == j) next
      for (k in seq(1, dim(c1)[1])) {
        for (l in seq(1, k)) {
          if (k == l) next
          if (all(sort(c2[i, j, ]) == sort(c1[k, l, ]))) {
            overlapping[[length(overlapping) + 1]] <- c(i = i, j = j, k = k, l = l)
            if (length(overlapping) > 12 * 11 / 2) break
          }
        }
        if (length(overlapping) > 12 * 11 / 2) break
      }
      if (length(overlapping) > 12 * 11 / 2) break
    }
    if (length(overlapping) > 12 * 11 / 2) break
  }
  if (!length(overlapping)) return(NULL)

  overlapping <- overlapping |> unlist() |> matrix(ncol = 4, byrow = TRUE)

  c1_beacons <- unique(as.integer(overlapping[, 3:4])) |> sort()
  c2_beacons <- unique(as.integer(overlapping[, 1:2])) |> sort()

  if (length(c2_beacons) < 12) return(NULL)

  matches <- rep(NA, dim(c1)[1])
  for (kl in c1_beacons) {
    kl_matches <- overlapping[overlapping[, 3] == kl | overlapping[, 4] == kl, 1:2]
    set_a <- kl_matches[1, ]
    set_b <- kl_matches[2, ]
    matches[kl] <- intersect(set_a, set_b)
  }

  matches
}

determine_orientation <- function(cube1, cube2) {
  u <- cube1[1, , drop = FALSE] - cube1[2, , drop = FALSE]
  v <- cube2[1, , drop = FALSE] - cube2[2, , drop = FALSE]

  found <- -1
  for (i in 1:48) {
    v_ <- permute_axes(v, i)
    if (length(unique(as.vector(u / v_))) == 1 && all(sign(u) == sign(v_))) {
      found <- i
    }
  }

  found
}

# Reconstruct the coordinates of all unique beacons based on the overlap of
# of beacon positions among partially overlapping scanner cubes
align_cubes <- function(cubes, debug = FALSE) {
  dist <- lapply(cubes, compute_distances)

  aligned <- vector("list", length(cubes))
  aligned[[1]] <- cubes[[1]]

  # find the next unaligned cube which matches one of the cubes already aligned
  repeat {
    unaligned <- sapply(aligned, is.null)
    if (!any(unaligned))
      break
    else
      if (debug) cat(sum(unaligned), "unaligned cubes remain\n-----\n")

    for (i in seq_along(cubes)) {
      for (j in seq_along(cubes)) {
        if (debug) cat("comparing", i, "and", j, "... ")
        if (i == j) { if (debug) cat("same pair\n"); next }
        if (!is.null(aligned[[i]]) && !is.null(aligned[[j]])) { if (debug) cat("both aligned already\n"); next }
        if (is.null(aligned[[i]]) && is.null(aligned[[j]])) { if (debug) cat("neither aligned yet\n"); next }
        cat("\n")

        # find out which cube of the pair is already aligned w.r.t. cube #1 and
        # set its index to `c1` used as a reference for rotations and shifts below
        if (!is.null(aligned[[i]])) {
          c1 <- i
          c2 <- j
        } else {
          c1 <- j
          c2 <- i
        }

        # detect beacons overlapping between (aligned) cube i and (unaligned)
        # cube j
        overlaps <- find_overlaps(dist[[c1]], dist[[c2]])

        # if not enough beacons overlap (less than 12), test a different pair
        # of scanner cube regions
        if (is.null(overlaps))
          next
        else {
          if (debug) cat("overlap of", sum(!is.na(overlaps)), "cubes detected\n")

          # extract the coordinates of the overlapping beacons
          c1_overlap <- aligned[[c1]][which(!is.na(overlaps)), ]
          c2_overlap <- cubes[[c2]][overlaps[!is.na(overlaps)], ]

          # determine which transformation of the 3D beacon matrix aligns the cube
          # j (which is unaligned) to the cube i
          orientation <- determine_orientation(c1_overlap, c2_overlap)

          # compute the shift necessary to transform the beacons in the scanner
          # cube j to the same coordinate system with respect to the scanner in the
          # cube i
          c2_overlap_ <- permute_axes(c2_overlap, orientation)
          shifted_by <- (c1_overlap - c2_overlap_)[1, ]

          # perform the alignment of the second cube
          aligned[[c2]] <- permute_axes(cubes[[c2]], orientation) |>
            sweep(MARGIN = 2, shifted_by, "+")
        }
      }
    }
  }

  aligned
}

dedupe_beacons <- function(cubes) {
  beacons <- do.call(rbind, cubes)
  beacons[!duplicated(beacons), ]
}
