submarine <- function(location = c(0, 0)) {
  location <- location
  program <- list()
  index <- 1

  load_program <- function(file) {
    instructions <- readLines(file) |>
      lapply(\(instruction) {
        tokens <- strsplit(instruction, " ")[[1]]
        list(direction = tokens[1], step = as.integer(tokens[2]))
      })
    program <<- instructions
    invisible(NULL)
  }

  next_instruction <- function() {
    if (index > length(program)) {
      cat("No instructions left\n")
      return()
    }

    direction <- program[[index]]$direction
    step <- program[[index]]$step

    if (direction == "forward")
      move <- c(step, 0)
    else if (direction == "up")
      move <- c(0, -step)
    else if (direction == "down")
      move <- c(0, step)
    else
      stop("Invalid direction", direction, call. = FALSE)

    location <<- location + move
    index <<- index + 1
  }

  print_program <- function() {
    for (i in seq(index, length(program))) {
      direction <-  program[[i]]$direction
      step <- program[[i]]$step
      cat(sprintf("instruction #%d: %s - %d\n", i, direction, step))
    }
  }

  execute_program <- function() {
    for (i in seq_along(program)) {
      next_instruction()
    }
  }

  get_location <- function() location

  invisible(list(
    load_program = load_program,
    next_instruction = next_instruction,
    print_program = print_program,
    execute_program = execute_program,
    get_location = get_location
  ))
}
