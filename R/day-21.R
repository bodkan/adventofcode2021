pos_max <- 10
points_win <- 21

comb <- as.matrix(expand.grid(1:3, 1:3, 1:3))
dice <- table(rowSums(comb))

create_player <- function(init_pos, pos_max, points_win) {
  states <- matrix(rep(0, pos_max * (pos_max + points_win)), nrow = pos_max)
  rownames(states) <- 1:10
  colnames(states) <- seq(0, pos_max + points_win - 1)
  states[init_pos, 1] <- 1
  states
}

play <- function(states, dice) {
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

player1 <- create_player(init_pos = 10, pos_max, points_win)
player2 <- create_player(init_pos = 9, pos_max, points_win)

win_states <- (points_win + 1) : ncol(player1)
wins1 <- wins2 <- 0

round <- 1
turn <- 0

while (TRUE) {
  # if (turn == 4) browser()
  # player 1
  universes <- sum(player2)
  # if (turn == 4) browser()
  # cat(sprintf("turn %i\n", turn))
  # cat(sprintf("player 1 universes %s\n", universes))
  # cat("universes / sum(player1)", universes / sum(player1), "\n")
  player1 <- play(player1 * universes / sum(player1), dice)

  win_count <- sum(player1[, win_states])
  wins1 <- wins1 + win_count
  player1[, win_states] <- 0

  # cat("player 1 wins in", win_count, "universes\n")
  # cat("player 1 has", sum(player1[, -win_states]), "universes alive\n")

  if (all(player1[, -win_states] == 0)) break

  turn <- turn + 1

  # player 2
  universes <- sum(player1)
  # cat(sprintf("turn %i\n", turn))
  # cat(sprintf("player 2 universes %s\n", universes))
  # cat("universes / sum(player2)", universes / sum(player2), "\n")

  player2 <- play(player2 * universes / sum(player2), dice)

  win_count <- sum(player2[, win_states])
  wins2 <- wins2 + win_count
  player2[, win_states] <- 0

  # cat("player 2 wins in", win_count, "universes\n")
  # cat("player 2 has", sum(player2[, -win_states]), "universes alive\n")

  if (all(player2[, -win_states] == 0)) break

  turn <- turn + 1
}

options("scipen" = 10)
cat(max(wins1, wins2))

