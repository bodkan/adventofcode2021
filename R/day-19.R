
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
  overlapping <- overlapping[(overlapping[, 1] != overlapping[, 3]) & (overlapping[, 2] != overlapping[, 4]), ]

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

permute_axes <- function(x, i) {
  combinations <- expand.grid(1:6, 1:8)

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

  p <- permutations[[combinations[i, 1]]]
  r <- rotations[[combinations[i, 2]]]

  sweep(x, MARGIN = 2, r, "*")[, p, drop = FALSE]
}

determine_orientation <- function(cube1, cube2) {
  u <- cube1[1, , drop = FALSE] - cube1[2, , drop = FALSE]
  v <- cube2[1, , drop = FALSE] - cube2[2, , drop = FALSE]

  for (i in 1:48) {
    v_ <- permute_axes(v, i)
    if (length(unique(as.vector(u / v_))) == 1 && all(sign(u) == sign(v_))) break
  }

  i
}

# Reconstruct the coordinates of all unique beacons based on the overlap of
# of beacon positions among partially overlapping scanner cubes
reconstruct_beacons <- function(cubes, debug = FALSE) {
  dist <- lapply(cubes, compute_distances)

  aligned <- vector("list", length(cubes))
  aligned[[1]] <- cubes[[1]]

  repeat {
    # do any scanner beacon cubes remain to be unaligned?
    unaligned <- sapply(aligned, is.null)

    # no unaligned cubes remain -- we're done
    if (!any(unaligned)) break

    if (debug) cat(sprintf("%d out of %d unaligned cubes remain\n",
                           sum(unaligned), length(cubes)))

    # find the next unaligned cube which matches one of the cubes already aligned
    for (i in which(!unaligned)) {
      for (j in which(unaligned)) {
        # detect beacons overlapping between (aligned) cube i and (unaligned)
        # cube j
        overlaps <- find_overlaps(dist[[i]], dist[[j]])

        # if not enough beacons overlap (less than 12), test a different pair
        # of scanner cube regions
        if (is.null(overlaps)) next

        # extract the coordinates of the overlapping beacons
        c1_overlap <- aligned[[i]][which(!is.na(overlaps)), ]
        c2_overlap <- cubes[[j]][overlaps[!is.na(overlaps)], ]

        # determine which transformation of the 3D beacon matrix aligns the cube
        # j (which is unaligned) to the cube i
        orientation <- determine_orientation(c1_overlap, c2_overlap)

        # compute the shift necessary to transform the beacons in the scanner
        # cube j to the same coordinate system with respect to the scanner in the
        # cube i
        c2_overlap_ <- permute_axes(c2_overlap, orientation)
        shifted_by <- (c1_overlap - c2_overlap_)[1, ]

        # perform the alignment of the second cube
        aligned[[j]] <- permute_axes(cubes[[j]], orientation) |>
          sweep(MARGIN = 2, shifted_by, "+")

        # a new cube was aligned and we can look for another pair
        break
      }
        # if (i == 5 && j == 3) browser()
      # if one alignment succeeded, restart the process to look for another
      # aligned-unaligned pair
      if (!is.null(aligned[[j]])) break
    }
  }

  # bind all aligned beacon coordinates and remove duplicates in each cube
  beacons <- do.call(rbind, aligned)
  beacons[!duplicated(beacons), ]
}
