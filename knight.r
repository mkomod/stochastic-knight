
INIT_POS <- matrix(c(1, 0), nrow=2)
MOVES <- matrix(c(-1, +2, +1, +2, -2, +1, +2, +1, 
		  -1, -2, +1, -2, -2, -1, +2, -1), 
		nrow=8, ncol=2, byrow=TRUE)
COUNTER <- 0
BOARD_DIM <- c(8,8)
NUM_TRIALS <- 10000


make_move <- function(pos) {
    # Takes current position and makes a random valid move
    #
    # Args:
    #  pos: (matrix) the current position
    #
    # Returns:
    #  (matrix) the new poisition
    #
    possible_moves <- apply(MOVES, 1, function(x) (x + pos))

    # validate the moves to ensure they are a legal move
    valid_moves <- possible_moves[ ,
	    (possible_moves[1,] > -1 & possible_moves[1,] < BOARD_DIM[1]) &
	    (possible_moves[2,] > -1 & possible_moves[2,] < BOARD_DIM[2])]

    # select a single validated move
    random_move <- sample(1:ncol(valid_moves), 1)

    # set the current position to the randomly selected move

    return(valid_moves[ , random_move, drop=FALSE])
}

num.moves <- c()
avg.moves <- c()

set.seed(42)

for(i in 1:NUM_TRIALS) {
    current_pos <- make_move(INIT_POS)
    COUNTER <- COUNTER + 1
    while(! (current_pos[1,1] == INIT_POS[1,1] && 
	    current_pos[2,1] == INIT_POS[2,1])) 
    { 
	current_pos <- make_move(current_pos)
	COUNTER <- COUNTER + 1
    }
    num.moves <- append(num.moves, COUNTER)
    avg.moves <- append(avg.moves, sum(num.moves)/i)
    COUNTER <- 0
}

mean(num.moves)
jpeg("knight.jpg", height=480, width=500)
plot(avg.moves, ylim=c(80, 150), xlim=c(0, NUM_TRIALS), t="l", xlab="Simulation", ylab="Avg. Number of Moves")
dev.off()

