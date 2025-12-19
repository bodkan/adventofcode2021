# Parse the ALU program in the given file into a list of instructions, each
# instruction represented by a pair [<code>, [<arg 1>, ...]]
parse_program <- function(file) {
  readLines(file) |>
    lapply(\(l) {
      tokens <- strsplit(l, " ")[[1]]

      args <- list(tokens[2])
      if (length(tokens) > 2) {
        args[2] <- c(tokens[3])
        if (!is.na(strtoi(args[2]))) {
          args[2] <- as.numeric(args[2])
        }
      }

      list(
        instruction = tokens[[1]],
        args = args
      )
    })
}

# Extract a numerical value from a given register (or just return a numerical
# value directly)
get_value <- function(value, alu) {
  if (is.numeric(value))
    value
  else
    alu[[value]]
}

# Given an instruction specified as [<code>, [<arg 1>, ...]], a current state
# of the w, x, y, z registers of the ALU and a remaining list of digits to
# be processed, modify the ALU registers accordingly
execute_instruction <- function(instruction, alu, input, debug) {
  code <- instruction$instruction
  args <- instruction$args

  if (debug) {
    cat("Current ALU registers:", concatenate(alu), "\n")
    cat("Current input buffer:", concatenate(input), "\n")
    cat("Current instruction to be executed:", code, paste(args), "\n")
  }

  if (code == "inp") {
    alu[[args[[1]]]] <- input[1]
    input <- input[-1]
  } else if (code == "add") {
    alu[[args[[1]]]] <- alu[[args[[1]]]] + get_value(args[[2]], alu)
  } else if (code == "mul") {
    alu[[args[[1]]]] <- alu[[args[[1]]]] * get_value(args[[2]], alu)
  } else if (code == "div") {
    alu[[args[[1]]]] <- alu[[args[[1]]]] %/% get_value(args[[2]], alu)
  } else if (code == "mod") {
    alu[[args[[1]]]] <- alu[[args[[1]]]] %% get_value(args[[2]], alu)
  } else if (code == "eql") {
    alu[[args[[1]]]] <- as.numeric(alu[[args[[1]]]] == get_value(args[[2]], alu))
  } else {
    stop("Invalid instruction", code, "\n", call. = FALSE)
  }

  if (debug) {
    cat("---\n")
    cat("Updated ALU registers:", concatenate(alu), "\n")
    cat("Updated input buffer:", concatenate(input), "\n")
    cat("===\n")
  }

  list(alu = alu, input = input)
}

# Execute a program given an ALU starting state and a list of digits to process
run_program <- function(alu, program, input, debug = FALSE) {
  for (i in seq_along(program)) {
    instruction <- program[[i]]
    update <- execute_instruction(instruction, alu, input, debug)
    alu <- update$alu
    input <- update$input
  }
  alu
}

# Partition the given number into individual digits
process_input <- function(x) as.numeric(strsplit(as.character(x), "")[[1]])

# Initialize ALu registers to given starting values
initialize_alu <- function(w, x, y, z) {
  list(w = w, x = x, y = y, z = z)
}

concatenate <- function(x) paste(x, collapse = " ")
