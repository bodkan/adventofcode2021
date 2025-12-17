parse_program <- function(file) {
  readLines(file) |>
    lapply(\(l) {
      tokens <- strsplit(l, " ")[[1]]

      args <- list(tokens[2])
      if (length(tokens) > 2) {
        args[2] <- c(tokens[3])
        if (!is.na(strtoi(args[2]))) {
          args[2] <- as.integer(args[2])
        }
      }

      list(
        instruction = tokens[[1]],
        args = args
      )
    })
}

get_value <- function(value, alu) {
  if (is.numeric(value))
    value
  else
    alu[[value]]
}

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
    alu[[args[[1]]]] <- bit64::as.integer64(alu[[args[[1]]]] + get_value(args[[2]], alu))
  } else if (code == "mul") {
    alu[[args[[1]]]] <- bit64::as.integer64(alu[[args[[1]]]] * get_value(args[[2]], alu))
  } else if (code == "div") {
    alu[[args[[1]]]] <- bit64::as.integer64(alu[[args[[1]]]] %/% get_value(args[[2]], alu))
  } else if (code == "mod") {
    alu[[args[[1]]]] <- bit64::as.integer64(alu[[args[[1]]]] %% get_value(args[[2]], alu))
  } else if (code == "eql") {
    alu[[args[[1]]]] <- bit64::as.integer64(alu[[args[[1]]]] == get_value(args[[2]], alu))
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

run_program <- function(alu, program, input, debug = FALSE) {
  for (i in seq_along(program)) {
    instruction <- program[[i]]
    update <- execute_instruction(instruction, alu, input, debug)
    alu <- update$alu
    input <- update$input
  }
  alu
}

process_input <- function(x) bit64::as.integer64(strsplit(as.character(x), "")[[1]])

initialize_alu <- function(w, x, y, z) {
  list(w = bit64::as.integer64(w),
       x = bit64::as.integer64(x),
       y = bit64::as.integer64(y),
       z = bit64::as.integer64(z))
}

concatenate <- function(x) paste(lapply(x, bit64::as.character.integer64), collapse = " ")
