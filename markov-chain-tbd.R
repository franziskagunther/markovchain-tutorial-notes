## This script is supposed to follow along with a tutorial on Markov chains and serves the purpose of 
# documenting my progress.

library(ggplot2)

# EX: Sample from an unbiased six-sided die
s <- sample(c(1, 2, 3, 4, 5, 6), 10000, replace = TRUE)
df_s <- as.data.frame(s)
s_int <- sample.int(6, 10000, replace = TRUE)
df_s_int <- as.data.frame(s_int)

p1 <- ggplot(df_s, aes(x=s), binwidth=0.5, bins=6) + geom_histogram() + labs(x="Number on Die", y = "Count")
p2 <- ggplot(df_s, aes(x=s), binwidth=0.5, bins=6) + geom_histogram() + labs(x="Number on Die", y = "Count")
grid.arrange(p1, p2, nrow=1)

# In a Markov process, the probability of going from one state to the other only depends on 
# the current and not past states. If a Markov process operates on a finite set of states, it
# is called a Markov chain. Properties of a Markov process: a space of possible states in the 
# process; a transition matrix describing the probabilities for each transition that is itself 
# characterised by a start and an end; a current state probability distribution describing the 
# probability of occupying a certain state.

# EX: Weather simulation

transitionMatrix = matrix(c(0.7, 0.1, 0.2, 0.6, 0.2, 0.2, 0.3, 0.4, 0.3), nrow=3, ncol=3, byrow=TRUE) 

state = 1 # initial state - it is [1] sunny or [2] cloudy or [3] rainy
weather_sequence = rep(0, 30) # vector to store simulated values
for ( day in 1:30 ) { # simulate for 30 days
  pr = transitionMatrix[state, ] # select the row of transition probabilities
  state = sample(c(1, 2, 3), size=1, prob=pr) # sample [1] or [2] based on the probs pr 
  weather_sequence[day] = state # store the sampled state
} 
print(weather_sequence)

# EX: Monopoly

num_players <- 4

num_games <- 1000 # number of games to play
num_turns <- rep(1:num_players, 1000)

current_board_position <- integer(num_players) # 4 zeros
go_to_jail_position <- 30 # the go to jail space
jail_position <- 10 # jail space
properties_that_can_be_bought <- data.frame(pos=c(1, 3, 5, 6, 8, 9, 11, 12, 13, 14, 15, 16, 18, 19, 21, 23, 24, 25, 26, 27, 28, 29, 31, 32, 34, 35, 37, 39), value=c(60, 60, 200, 100, 100, 120, 140, 150, 140, 160, 200, 180, 180, 200, 220, 220, 240, 200, 260, 260, 150, 280, 300, 300, 320, 200, 350, 400), cost=c(60, 60, 200, 100, 100, 120, 140, 150, 140, 160, 200, 180, 180, 200, 220, 220, 240, 200, 260, 260, 150, 280, 300, 300, 320, 200, 350, 400))

time_to_buy_all_properties <- rep(0, num_games) # vector to store number of turns to buy all properties
account <- rep(1500, 4)

# simulate multiple games 
for ( game in 1:num_games ) {
  
  # move.size <- rep(0, num_turns)
  positions_visited <- rep(0, length(num_turns)) # 4000 zeros
  positions_purchased <- rep(0, 40) # repeat zero 40 times
  properties_bought <- rep(0, length(num_turns)) # 4000 zeros
  
  # use a for loop to simulate a number of turns
  for ( turn in 1:length(num_turns) ) {
    
    # determine whose turn it is
    whose_turn <- num_turns[turn]
    
    # roll two dice
    die.values <- sample(c(1:6), 2, replace=TRUE)
    
    # move player position
    
    # number of positions to move
    plus.move <- sum(die.values)
    
    # compute new board position
    new_board_position <- current_board_position[whose_turn] + plus.move
    
    cat(plus.move, "number on dies\n")
    
    # if Go is passed, increase account by 200
    if ((current_board_position[whose_turn] %% 40 >= 28) && (new_board_position %% 40 <= 11)) {
      cat(current_board_position[whose_turn], " current position\n")
      cat(new_board_position %% 40, " position obtained\n")
      account[whose_turn] <- account[whose_turn] + 200
    }
    
    # if land on GO TO JAIL square, then go backwards to the JAIL square
    if ( new_board_position == go_to_jail_position ) {
      new_board_position = jail_position
    }    
    
    # update board position (this corrects for the fact the board is circular)
    current_board_position[whose_turn] = ( new_board_position %% 40 ) 
    
    # if we can on a square that can be purchased and which has not been purchased (note R uses 1-indexing for arrays)
    if ( positions_purchased[current_board_position[whose_turn]+1] == 0 ) {
      if ( current_board_position[whose_turn] %in% properties_that_can_be_bought$pos ) {
        prop_index <- which(properties_that_can_be_bought$pos %in% current_board_position[whose_turn])
        cat("Balance before: ", account[whose_turn], "\n")
        if(account[whose_turn] >= properties_that_can_be_bought$value[prop_index]) { # buy if we have enough money
          account[whose_turn] <- account[whose_turn] - properties_that_can_be_bought$value[prop_index]
          positions_purchased[current_board_position[whose_turn]+1] <- whose_turn
        } 
        cat("Balance after: ", account[whose_turn], "\n")
        cat("Property value: ", properties_that_can_be_bought$value[prop_index], "\n")
      }
    }        
    
    # store position visited
    positions_visited[turn] <- current_board_position[whose_turn]
    
    # store number of properties bought
    properties_bought[turn] <- sum(positions_purchased != 0)
    
    # check if all properties are gone
    if ( properties_bought[turn] == length(properties_that_can_be_bought$pos) ) {
      time_to_buy_all_properties[game] = turn
      break
    }
    
    
  }
  
}

hist(time_to_buy_all_properties, breaks=20) 