#' Closure-based implementation of a submarine "object"
#'
#' The function acts as a "constructor" which returns a closure capturing
#' all "properties" of the object and returns a list of functions acting as
#' its "methods".
submarine <- function(location = c(0, 0), aim = 0, mode = c("v1", "v2")) {
  # the mode argument acts as a switch between the two submarine specifications
  mode <- match.arg(mode)

  #
  # "public methods"
  #

  # Load the submarine program from a given file
  load_program <- function(file) {
    # parse the file of submarine movement instructions -- one instruction per line,
    # each instruction in the form of:
    #   <"forward" | "up" | "down"> <integer number>
    instructions <- readLines(file) |>
      lapply(\(instruction) {
        tokens <- strsplit(instruction, " ")[[1]]
        list(direction = tokens[1], step = as.integer(tokens[2]))
      })
    program <<- instructions
    index <<- 1

    invisible(NULL)
  }

  # Execute the next instruction in the program
  next_instruction <- function() {
    if (index > length(program)) {
      cat("No instructions left\n")
      return()
    }

    direction <- program[[index]]$direction
    step <- program[[index]]$step

    move <- next_move(direction, step)

    location <<- location + move
    index <<- index + 1
  }

  # Print the entire program in a readable form
  print_program <- function() {
    for (i in seq(index, length(program))) {
      direction <-  program[[i]]$direction
      step <- program[[i]]$step
      cat(sprintf("instruction #%d: %s - %d\n", i, direction, step))
    }
  }

  # Execute the entire program, one instruction at a time
  execute_program <- function() {
    for (i in seq_along(program)) {
      next_instruction()
    }
  }

  # Return the current location of the submarine in two dimensions
  # (horizontal coordinate, depth)
  get_location <- function() location

  #
  # "private methods"
  #

  # First version of the move method utilizing only horizontal
  next_move_v1 <- function(direction, step) {
    if (direction == "forward")
      move <- c(step, 0)
    else if (direction == "up")
      move <- c(0, -step)
    else if (direction == "down")
      move <- c(0, step)
    else
      stop("Invalid direction", direction, call. = FALSE)
    move
  }

  next_move_v2 <- function(direction, step) {
    move <- c(0, 0)
    if (direction == "forward")
      move <- c(step, abs(aim * step))
    else if (direction == "up")
      aim <<- aim + step
    else if (direction == "down")
      aim <<- aim - step
    else
      stop("Invalid direction", direction, call. = FALSE)
    move
  }

  next_move <- if (mode == "v1") next_move_v1 else next_move_v2

  # return the constructed "object" as a list of its methods
  list(
    load_program = load_program,
    next_instruction = next_instruction,
    print_program = print_program,
    execute_program = execute_program,
    get_location = get_location
  )
}
