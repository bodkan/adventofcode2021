# Extract i-th character of a given string
get_char <- function(str, i) substr(str, i, i)

# Get a chunk from a given string of a specified length
get_chunk <- function(str, start, len = nchar(str) - start + 1) {
  substr(str, start, start + len - 1)
}

# Convert binary number (encoded as a string of "0" a "1") to decimal
decimal <- function(bin) {
  strsplit(bin, "")[[1]] |> as.integer() |> rev() |> bin_to_dec()
}

# Convert hexadecimal input to a bitstring
decode_bits <- function(hexstr) {
  decodings <- list(
    "0" = "0000",
    "1" = "0001",
    "2" = "0010",
    "3" = "0011",
    "4" = "0100",
    "5" = "0101",
    "6" = "0110",
    "7" = "0111",
    "8" = "1000",
    "9" = "1001",
    "A" = "1010",
    "B" = "1011",
    "C" = "1100",
    "D" = "1101",
    "E" = "1110",
    "F" = "1111"
  )
  seq_len(nchar(hexstr)) |>
    sapply(\(i) decodings[[get_char(hexstr, i)]]) |>
    paste(collapse = "")
}

# Extract version number of the packet in the given bitstring
decode_version <- function(bits) {
  start <- 1
  nbits <- 3
  list(
    shift = nbits,
    value = get_chunk(bits, start, nbits) |> decimal()
  )
}

# Extract type ID number of the packet in the given bitstring
decode_type_id <- function(bits) {
  start <- 4
  nbits <- 3
  list(
    shift = nbits,
    value = get_chunk(bits, start, nbits) |> decimal()
  )
}

# Extract the number carried by the given packet
decode_literal <- function(bits) {
  # starting position of the literal number bit groups in each packet
  pos <- 7
  # number of bits in each group
  nbits <- 4

  groups <- list()
  while (TRUE) {
    groups[[length(groups) + 1]] <- get_chunk(bits, pos + 1, nbits)
    if (get_char(bits, pos) == 0) break
    pos <- pos + nbits + 1
  }
  concat <- groups |> unlist() |> paste(collapse = "")

  list(
    value = decimal(concat),
    shift = nchar(concat) + length(groups)
  )
}

# Extract length type (0 or 1 value) of the subpackets carried by the
# packet in the given bitstring
decode_length_type <- function(bits) {
  # position of the bit in the packet bitstring
  pos <- 7
  # number of bits occupied by the length type
  nbits <- 1
  list(
    shift = nbits,
    value = as.integer(get_char(bits, pos))
  )
}

# Extract lengths of the bitstring containing the subpackets
decode_subpacket_length <- function(bits) {
  # position of the length block in the packet bitstring
  pos <- 8
  # number of bits occupied by the subpackets' length
  nbits <- 15
  list(
    shift = nbits,
    value = get_chunk(bits, pos, nbits) |> decimal()
  )
}

# Extract the number of the subpackets encoded in the bitstring
decode_subpacket_count <- function(bits) {
  # position of the subpacket count block in the packet bitstring
  pos <- 8
  # number of bits occupied by the subpacket count
  nbits <- 11
  list(
    shift = nbits,
    value = get_chunk(bits, pos, nbits) |> decimal()
  )
}

# Helper function to answer part 1 of the day 16 puzzle -- summing up the
# version numbers of all packets encoded in the recursive tree
sum_versions <- function(x) {
  flattened <- unlist(x)
  sum(flattened[grep("version", names(flattened))])
}

# Recursively parse a given binary bitstring, returning a tree of nested
# packet hierarchies
parse_packet <- function(bin) {
  # extract version and type ID from the header
  version <- decode_version(bin)
  type_id <- decode_type_id(bin)
  # if (version$value == 3 && type_id$value == 0) browser()
  # record how many bitshifts occured so far
  all_shifts <- version$shift + type_id$shift

  packet <- list(
    version = version$value,
    type_id = type_id$value
  )

  # if the header indicates a literal type, extract the number
  if (type_id$value == 4) {
    literal <- decode_literal(bin)
    content <- literal$value
    all_shifts <- all_shifts + literal$shift
  } else { # the header indicates an operator type
    content <- list()

    # read the type of subpacket bitstring length encoding and increment
    # the shift counter
    length_type <- decode_length_type(bin)
    all_shifts <- all_shifts + length_type$shift

    # length type 0: subpackets are encoded in a fixed length bit string
    if (length_type$value == 0) {
      subpacket_bit_length <- decode_subpacket_length(bin)
      all_shifts <- all_shifts + subpacket_bit_length$shift

      # at which position in the bit string to start looking for the next
      # subpacket bit string (or a consecutive series thereof)
      start <- all_shifts + 1
      shift <- subpacket_bit_length$value

      repeat {
        # move along the bitstring and extract the subpackets' bits
        bin <- get_chunk(bin, start = start, len = shift)

        # recursively parse the next subpacket and add its contents to
        # the current packet's content body
        parsed_result <- parse_packet(bin)
        content[[length(content) + 1]] <- parsed_result

        # shift along the subpacket bitstring accordingly
        start <- parsed_result$shift + 1
        shift <- shift - parsed_result$shift

        all_shifts <- all_shifts + parsed_result$shift

        if (shift == 0) break
      }
    } else { # length type 1: a given number of subpackets is specified
      subpacket_count <- decode_subpacket_count(bin)
      all_shifts <- all_shifts + subpacket_count$shift

      start <- all_shifts + 1
      for (i in seq_len(subpacket_count$value)) {
        # move along the bitstring and extract the subpackets' bits
        bin <- get_chunk(bin, start = start)

        # recursively parse the next subpacket and add its contents to
        # the current packet's content body
        parsed_result <- parse_packet(bin)
        content[[length(content) + 1]] <- parsed_result

        # shift along the subpacket bitstring accordingly
        start <- parsed_result$shift + 1

        # if (packet$version == 3 & i == 1) browser()
        all_shifts <- all_shifts + parsed_result$shift
      }
    }
  }

  packet[["content"]] <- content

  list(packet = packet, shift = all_shifts)
}

# Traverse the expression tree and evaluate it bottom up based on operations
# encoded by the type ID in the header of each packet
eval_expression <- function(expr) {
  type_id <- expr$packet$type_id
  # type 4 indicates a literal numerical value which can be returned as it is
  if (type_id == 4)
    result <- expr$packet$content
  else {
    # for more complex expressions, first evaluate individual components...
    values <- sapply(expr$packet$content, eval_expression)
    operator_fun <- switch(
      as.character(type_id),
      "0" = sum,
      "1" = prod,
      "2" = min,
      "3" = max,
      "5" = \(x) as.integer(x[1] > x[2]), # binary operations are
      "6" = \(x) as.integer(x[1] < x[2]), # guaranteed to have only
      "7" = \(x) as.integer(x[1] == x[2]) # two operands, by the spec
    )
    # ... and then apply the appropriate operation on all of them
    result <- operator_fun(values)
  }
  result
}
