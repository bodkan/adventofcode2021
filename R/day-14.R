# Read the template and the set of substitution rules from the input file,
# return counts of characters in the template, the count of pairs present
# in the initial template, and the set of rules in a list
read_day14 <- function(file) {
  lines <- readLines(file)

  template <- strsplit(lines[1], "")[[1]]
  rule_tokens <- lines[3:length(lines)] |> strsplit(" -> ")
  rules <- sapply(rule_tokens, `[[`, 2)
  names(rules) <- sapply(rule_tokens, `[[`, 1)

  # determine all possible characters from the rules and count their
  # initial occurrences in the input template (converting to factor to
  # include 0 counts for characters not yet present)
  letters <- unique(unlist(strsplit(names(rules), "")))
  char_counts <- table(factor(template, levels = letters)) |> as.vector()
  names(char_counts) <- letters

  # count the pairs of adjacent characters in the input template
  pair_counts <- rep(0, length(rules))
  names(pair_counts) <- names(rules)
  for (i in seq(1, length(template) - 1)) {
    pair <- paste(template[i:(i + 1)], collapse = "")
    if (is.na(pair)) next
    pair_counts[pair] <- pair_counts[pair] + 1
  }

  list(
    counts = list(char_counts = char_counts,
                 pair_counts = pair_counts),
    rules = rules
  )
}

# Run a single expansion from the pairs present in the current iteration
expand_once <- function(counts, rules) {
  char_counts <- counts$char_counts
  pair_counts <- counts$pair_counts

  # which rule pairs are present in the current expansion iteration?
  present_pairs <- names(pair_counts[pair_counts > 0])

  # prepare vector of new pair counts for the next iteration
  new_pair_counts <- rep(0, length(pair_counts))
  names(new_pair_counts) <- names(pair_counts)

  for (pair in present_pairs) {
    # new character to be added in the middle of the current pair
    new <- rules[pair]
    # get two new pairs which will result from the expansion
    new1 <- paste0(substr(pair, 1, 1), new, collapse = "")
    new2 <- paste0(new, substr(pair, 2, 2), collapse = "")

    # how many characters will be created is determined on the number of
    # occurrences of the generating pair in the current iteration
    # (no character is ever removed, so the counts will be incremented)
    char_counts[new] <- char_counts[new] + pair_counts[pair]

    # update the counts of pairs by the number of pairs just created
    new_pair_counts[new1] <- new_pair_counts[new1] + pair_counts[pair]
    new_pair_counts[new2] <- new_pair_counts[new2] + pair_counts[pair]
  }

  list(char_counts = char_counts, pair_counts = new_pair_counts)
}

# Execute a given number of expansion steps
expand <- function(counts, rules, steps) {
  for (step in seq_len(steps)) counts <- expand_once(counts, rules)
  counts$char_counts
}

compute_day14 <- function(char_counts) max(char_counts) - min(char_counts)
