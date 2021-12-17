# Wrapper around the optim function to determine the maximum possible height
# among all trajectories which ultimately hit the target area -- part 1 solution
#
# Note: this is ridiculously overengineered on purpose. The point was to learn
# how could one use the built-in `optim()` function for integer optimization and
# combinatorial problems
optimize_height <- function(area, init, step_x, step_y, maxit = 10000) {
  result <- optim(par = init, fn = optim_fn, gr = change_fun,
                  method = "SANN",
                  control = list(fnscale = -1, maxit = maxit),
                  area = area, step_x = step_x, step_y = step_y)
  result
}

# Count how many initial velocities lead to the probe hitting the target area
# -- part 2 solution
count_valid_velocities <- function(area, max_height) {
  valid_velocities <- list()

  # compute the range of horizontal velocities which are worth exploring
  vx_range <- seq(compute_min_vx(area$x1), area$x2)
  for (vx in vx_range) {
    # for a given vx, find all vy for which [vx, vy] is a valid initial velocity
    vy <- area$y1
    repeat {
      velocity <- c(vx, vy)
      trajectory <- compute_trajectory(velocity, area)
      if (in_target(trajectory[[length(trajectory)]], area)) {
        valid_velocities[[length(valid_velocities) + 1]] <- velocity
      } else if (maximum_height(trajectory[-length(trajectory)]) > max_height) {
        # if the current trajectory exceeded the maximum possible height,
        # it's not worth exploring further vy values
        break
      }
      vy <- vy + 1
    }
  }

  unique_velocities <- unlist(valid_velocities) |>
    matrix(ncol = 2, byrow = TRUE) |>
    (\(x) x[!duplicated(x), ])()

  nrow(unique_velocities)
}

# Objective function for the optimization problem of finding the maximum height
optim_fn <- function(velocity, area, ...) {
  positions <- compute_trajectory(velocity, area)
  maximum_height(positions)
}

# Plot a single simulation run for exploratory purposes
plot_trajectory <- function(velocity, area) {
  positions <- compute_trajectory(velocity, area)
  max_height <- maximum_height(positions)
  positions <- unlist(positions) |> matrix(ncol = 2, byrow = TRUE)

  # discard the last infinite row if the trajectory missed
  if (any(is.infinite(positions)))
    positions <- positions[-nrow(positions), ]
  plot(0, type = "n", ann = FALSE, xlim = c(0, area$x2),
       ylim = c(min(positions[, 2], area$y1),
                max(positions[, 2], area$y2)))
  rect(area$x1, area$y1, area$x2, area$y2, border = "white", col = "gray")
  points(positions, pch = 20, type = "b")
  abline(h = 0, col = "red", lty = 2)
  abline(v = 0, col = "red", lty = 2)

  height_str <- ifelse(is.infinite(max_height),
                       "probe missed", sprintf("max height: %d", max_height))
  title(main = paste0(sprintf("Initial velocity: [%s, %s]", velocity[1], velocity[2]),
                     "; ", height_str))
}

# Simulate the probe's trajectory given initial velocity vector
compute_trajectory <- function(velocity, area, report = FALSE) {
  pos <- c(0, 0)
  positions <- list(pos)
  found <- NULL
  repeat {
    # compute the next step
    step <- next_position(pos, velocity)
    pos <- step$pos
    velocity <- step$velocity

    positions[[length(positions) + 1]] <- pos

    # did the probe hit the target?
    if (in_target(pos, area)) {
      found <- pos
      break
    }

    if (missed(pos, area)) break
  }

  # return TRUE/FALSE if only success is needed
  if (report) return(!is.null(found))

  if (is.null(found))
    positions[[length(positions) + 1]] <- c(-Inf, -Inf)

  positions
}

# Does the given position hit the target area?
in_target <- function(pos, area) {
  pos[1] >= area$x1 &&
    pos[1] <= area$x2 &&
    pos[2] >= area$y1 &&
    pos[2] <= area$y2
}

# Did the given position miss the target area?
missed <- function(pos, area) {
  pos[1] > area$x2 || pos[2] < area$y1
}

# Generate next position assuming that the probe is at `pos` with the given
# velocity
next_position <- function(pos, velocity) {
  vx <- velocity[1]; vy <- velocity[2]
  list(
    pos = pos + velocity,
    velocity = c(
      if (vx == 0) vx else if (vx > 0) vx - 1 else vx + 1,
      vy - 1
    )
  )
}

# Find the maximum height in the given trajectory
maximum_height <- function(trajectory) {
  if (all(is.infinite(trajectory[[length(trajectory)]])))
    -Inf
  else
    max(sapply(trajectory, \(pos) pos[2]))
}

# Custom function for proposing new velocities in the optimization routine
change_fun <- function(velocity, area, step_x, step_y) {
  vx <- velocity[1] + sample(-step_x:step_x, size = 1)
  vy <- velocity[2] + sample(-step_y:step_y, size = 1)
  c(if (vx < 0) 0 else if (vx > area$x2) area$x2 else vx, vy)
}

# Find the minimum number of consecutive integers whose sum exceeds a given
# threshold
compute_min_vx <- function(total) {
  i <- 1
  repeat {
    if (i * (i + 1) / 2 > total)
      break
    else
      i <- i + 1
  }
  i
}

read_target_area <- function(file) {
  tokens <- readLines(file) |> strsplit(":|=|,|\\.\\.") |> (\(x) x[[1]])()
  list(
    x1 = as.integer(tokens[3]),
    x2 = as.integer(tokens[4]),
    y1 = as.integer(tokens[6]),
    y2 = as.integer(tokens[7])
  )
}
