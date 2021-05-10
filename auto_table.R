# quickly create a matrix in the style of a correlation table with a set of effects on a single "DV" and r = 0 covariances between "predictors"
# 'effects' argument takes a vector of numbers (e.g., all the correlation coefficients for the relationships between the "predictors" and the "DV")
# Joel Le Foresiter
# joel.leforestier@mail.utoronto.ca
# May 10, 2021

auto_table <- function(effects) {

  #put in the main effects
  cortable <- matrix(c(rep(0, times = length(effects)+1**2)), length(effects)+1, length(effects)+1)
  cortable[1,] <- c(1, effects)
  cortable[,1] <- c(1, effects)
  
  #add r = 1 covariances on the diagonal
  for(e in 1:length(effects)+1) {
    cortable[e,e] <- 1
  }
  
  return(cortable)
}
