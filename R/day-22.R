# Read reboot steps as a list of pairs of:
#   - an `action` to be taken: TRUE ("on") or FALSE ("off")
#   - a list `cuboid` if the x, y, and z dimensions of the cuboid for that action
#
# I.e., a list of elements like this:
#   $action
#   [1] TRUE
#
#   $cuboid
#   $cuboid$x
#   [1] -5 47
#
#   $cuboid$y
#   [1] -31  22
#
#   $cuboid$z
#   [1] -19  33
read_steps <- function(file) {
  readLines(file) |>
    lapply(\(l) {
      parts <- strsplit(l, " ")[[1]]

      action <- parts[1] == "on"
      cuboid <-
        strsplit(parts[2], ",")[[1]] |>
        lapply(\(i) as.numeric(strsplit(strsplit(i, "=")[[1]][2], "\\.\\.")[[1]]))
      names(cuboid) <- c("x", "y", "z")

      list(action = action, cuboid = cuboid)
    })
}

# A cuboid step filtering function for Part 1
filter_cuboids <- function(steps, min, max) {
  within_bounds <- sapply(steps, function(step) {
    all(sapply(c("x", "y", "z"), function(axis) {
      min <= step$cuboid[[axis]][1] && step$cuboid[[axis]][2] <= max
    }))
  })
  steps[within_bounds]
}

# Given two two-element vectors indicating intervals (p[1], p[2]) and (q[1], q[2]),
# find their intersection and return it as another two-element vector (or
# NULL in case of no overlap)
intersect_intervals <- function(p, q) {
  # take care of the testing scenario of 1D and 2D cuboids
  if (is.null(p) && is.null(q))
    return(NULL)

  # nonoverlapping intervals
  if (p[2] < q[1] || q[2] < p[1]) {
    return(NULL)
  } else {
    return(c(max(c(p[1], q[1])), min(c(p[2], q[2]))))
  }
}

# Find the intersection cuboid of two cuboids A and B (or return NULL in case
# they do not overlap)
intersect_cuboids <- function(A, B) {
  ax <- A$x; bx <- B$x
  ay <- A$y; by <- B$y
  az <- A$z; bz <- B$z

  # take care of the testing scenario of 1D and 2D cuboids
  ndim <- as.integer(!all(is.null(ax))) + as.integer(!all(is.null(ay))) + as.integer(!all(is.null(az)))

  ix <- intersect_intervals(ax, bx)
  iy <- intersect_intervals(ay, by)
  iz <- intersect_intervals(az, bz)

  cuboid <- list(x = ix, y = iy, z = iz)[1:ndim]

  if (any(sapply(cuboid, is.null)))
    cuboid <- NULL

  cuboid
}

# Given some previously processed `past` reboot step (represented by an
# `action` logical value and the coordinates of its `cuboid`), execute the given
# `current` reboot, returning the new cuboid which results from this
execute_step <- function(past, current) {
  intersection <- intersect_cuboids(past$cuboid, current$cuboid)

  # if past and current steps' cuboids don't intersect, there is nothing
  # to add to the growing collection of processed steps
  if (is.null(intersection)) {
    return(list())
  } else if (past$action == current$action) {
    # 1. two "on" steps result in adding an "off" intersection
    #    (inclusion-exclusion taking care of double counting)
    # 2. two "off" steps result in adding an "on" intersection (to prevent
    #    turning a previously turned "off" region twice)
    # => the resulting intersection always has the opposite action state of both
    action <- !current$action
  } else {
    # 1. applying an "on" cuboid B to an "off" cuboid A will add an "on"
    #    intersection (because the intersecting A cubes need to be turned "on")
    # 2. applying an "off" cuboid B to an "on" cuboid A will add an "off"
    #    intersection (because the intersecting A cubes need to be turned "off")
    # => the resulting intersection always inherits B's action state
    action <- current$action
  }

  return(list(action = action, cuboid = intersection))
}

# Apply a single reboot step to a list of previously processed cuboids
process_step <- function(step, processed) {
  # apply the current reboot step to all step cuboids processed so far
  results <- parallel::mclapply(processed, function(p) list(p, execute_step(p, step)),
                                mc.cores = parallel::detectCores())
  # transform the list of list-pairs into a flat list
  results <- unlist(results, recursive = FALSE)
  # remove all non-intersecting results
  Filter(\(l) length(l) > 0, results)
}

# Execute the given list of reboot steps in sequence, creating new intermediate
# cuboids after each step, and returning them all at the end
process_all <- function(steps, quiet = TRUE) {
  # put the first reboot step cuboid in the the list of final processed steps
  processed <- list(steps[[1]])

  # go through the remaining steps iteratively, always adding them to the list
  # once they are processed (but only if they represent an "on" action)
  for (i in 2:length(steps)) {
    if (!quiet) cat("Processing step", i, "/", length(steps), "\r")

    # process the next step
    step <- steps[[i]]
    processed <- process_step(step, processed)

    # if the next reboot step was an "on" cuboid, add it to the list of
    # processed steps ("off" cuboid steps are not added as a whole, only
    # their intersections are potentially added when applying them to each
    # previously processed cuboid
    if (step$action == TRUE)
      processed <- c(processed, list(step))
  }
  processed
}

count_on <- function(processed) {
  on <- Filter(\(x) x$action == TRUE, processed)
  off <- Filter(\(x) x$action == FALSE, processed)

  sizes_on <- if (length(on)) sapply(on, compute_volume) else 0
  sizes_off <- if (length(off)) sapply(off, compute_volume) else 0

  sum(sizes_on) - sum(sizes_off)
}

# Compute the total volume of a given step's cuboid
compute_volume <- function(step) {
  size_x <- (diff(step$cuboid$x) + 1)
  size_y <- (diff(step$cuboid$y) + 1)
  size_z <- (diff(step$cuboid$z) + 1)

  size <- size_x
  if (length(size_y)) size <- size * size_y
  if (length(size_z)) size <- size * size_z

  size
}
