# Create a two-dimensional matrix of the number of parallel universes in which
# a player is at a position <row> with <column - 1> number of points
create_player <- function(init_pos, pos_max, points_win) {
  states <- matrix(rep(0, pos_max * (pos_max + points_win)), nrow = pos_max)
  rownames(states) <- 1:10
  colnames(states) <- seq(0, pos_max + points_win - 1)
  states[init_pos, 1] <- 1
  states
}

# For each universe with <position>-<score> state (two-dimensional matrix of
# states), expand to a new set of universes in which the three dice rolled all
# possible sums, taking into account that different dice results create
# different number of universes. Return the new universes counts as a new
# matrix.
play <- function(states, dice, pos_max) {
  new_states <- states
  new_states[] <- 0

  # detect which states are present in the matrix
  present_states <- which(states > 0, arr.ind = TRUE)
  for (i in seq_len(nrow(present_states))) {
    # extract at which positions of the game have how many players scored
    # how many points
    coord <- present_states[i, , drop = FALSE]
    pos <- as.integer(coord[, 1])
    score <- as.integer(coord[, 2]) - 1
    count <- states[coord]

    # compute the new positions starting from the current position in
    # all parallel universes
    new_pos <- (pos + as.integer(names(dice))) %% pos_max
    new_pos[new_pos == 0] <- pos_max

    # compute the new score for players who move to a given new position
    # (knowing the started from the same score before the universes branch out
    new_score <- score + new_pos
    # compute how many of such players are, given how likely are the sums of the
    # three dice rolls
    new_count <- count * as.integer(dice)

    # update the state matrix -
    new_states[cbind(new_pos, new_score + 1)] <-
      new_states[cbind(new_pos, new_score + 1)] +
      new_count
  }

  new_states
}

part_2_game <- function(p1_pos, p2_pos, debug = FALSE) {
  pos_max <- 10
  points_win <- 21

  # sum up all possible combinations of three die rolls
  comb <- as.matrix(expand.grid(1:3, 1:3, 1:3))
  dice <- table(rowSums(comb))

  # create initial state matrices for counting all parallel universes -- at the
  # beginning, each matrix has only one non-zero value, which is the row
  # position corresponding to the initial position and the first column
  # (representing zero score)
  player1 <- create_player(init_pos = p1_pos, pos_max, points_win)
  player2 <- create_player(init_pos = p2_pos, pos_max, points_win)

  # which columns of the state matrix represent winning situations (i.e. states
  # in which a player at whatever position of the board has at least 21 points)
  win_states <- (points_win + 1):ncol(player1)
  wins1 <- wins2 <- 0

  while (TRUE) {
    # player 1

    # calculate the number of universes surviving after player 2 played their
    # round (at the beginning this is 1)
    universes <- sum(player2)
    if (debug) {
      cat(sprintf("turn %i\n", turn))
      cat(sprintf("player 1 universes %s\n", universes))
      cat("universes / sum(player1)", universes / sum(player1), "\n")
    }
    # let the player 1 play the game, taking into account the change in the
    # number of universes that happened between their last round and the number
    # of universes in which player 2 has not won in their round of the game
    player1 <- play(player1 * universes / sum(player1), dice, pos_max)

    # count the number of universes in which player 1 won
    win_count <- sum(player1[, win_states])
    wins1 <- wins1 + win_count
    player1[, win_states] <- 0

    if (debug) {
      cat("player 1 wins in", win_count, "universes\n")
      cat("player 1 has", sum(player1[, -win_states]), "universes alive\n")
    }

    if (all(player1[, -win_states] == 0)) break

    # player 2 -- same procedure as the one for player 1 above
    universes <- sum(player1)
    if (debug) {
      cat(sprintf("turn %i\n", turn))
      cat(sprintf("player 2 universes %s\n", universes))
      cat("universes / sum(player2)", universes / sum(player2), "\n")
    }

    player2 <- play(player2 * universes / sum(player2), dice, pos_max)

    win_count <- sum(player2[, win_states])
    wins2 <- wins2 + win_count
    player2[, win_states] <- 0

    if (debug) {
      cat("player 2 wins in", win_count, "universes\n")
      cat("player 2 has", sum(player2[, -win_states]), "universes alive\n")
    }

    if (all(player2[, -win_states] == 0)) break
  }

  max(wins1, wins2)
}

# Hard-coded ugly solution to game in part 1 -- not worth improving, given that
# part 2 is a completely different beast
part_1_game <- function(pos_p1, pos_p2, debug = FALSE) {
  p1 <- p2 <- 0

  pos_max <- 10

  die <- 1
  die_max <- 100
  die_counter <- 0

  while (TRUE) {
    # p1
    die_add <- 0
    for (i in 1:3) {
      if (debug) cat(die, " ")
      die_add <- die_add + die
      die <- die + 1
      if (die > die_max) die <- 1
      die_counter <- die_counter + 1
    }
    p1_shift <- die_add %% pos_max
    if (debug) cat("die add ", die_add, "\n")
    pos_p1 <- (pos_p1 + p1_shift) %% pos_max
    if (pos_p1 == 0) pos_p1 <- 10
    p1 <- p1 + pos_p1
    if (p1 >= 1000) break

    # p2
    die_add <- 0
    for (i in 1:3) {
      if (debug) cat(die, " ")
      die_add <- die_add + die
      die <- die + 1
      if (die > die_max) die <- 1
      die_counter <- die_counter + 1
    }
    p2_shift <- die_add %% pos_max
    if (debug) cat("die add ", die_add, "\n")
    pos_p2 <- (pos_p2 + p2_shift) %% pos_max
    if (pos_p2 == 0) pos_p2 <- 10
    p2 <- p2 + pos_p2
    if (p2 >= 1000) break

    if (debug) cat("p1 ", p1, "-", pos_p1, "   ", "p2 ", p2, "-", pos_p2, "\n")
  }
  if (debug) cat(p1, " vs ", p2, "\n")

  min(p1, p2) * die_counter
}
