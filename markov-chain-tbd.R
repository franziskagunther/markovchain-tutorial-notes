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