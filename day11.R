# Day 11
input = adventdrob::advent_input(day = 11, year = 2024, parse=F); input
s = strsplit(input$x, " ") |> unlist() |> as.numeric(); s

# notice that the order doesn't matter
# we are able to work on stones on their own 

update_stones = function (stones) {
  
  # get the locations of the stones based on rules 
  idx = seq(1:length(stones))
  idx_zeros = which(stones == 0)
  idx_even_length = which(nchar(stones) %% 2 == 0)
  idx_mult_2024 = idx[!(idx %in% c(idx_zeros, idx_even_length))]
  
  # make the new vector of stones 
  split_in_half = lapply(stones[idx_even_length], \(x) {
    stone1 = substr(x, 1, nchar(x)/2) |> as.numeric()
    stone2 = substr(x, nchar(x)/2 + 1, nchar(x)) |> as.numeric()
    c(stone1, stone2)
  }) |> unlist()
  
  return(
    c(rep(1, length(idx_zeros)),
      stones[idx_mult_2024] * 2024, 
      split_in_half)
  ) 
}

out = lapply(1:25, \(x) {
  s <<- update_stones(s) # co-pilot trick to track stones as a global 
})

length(out[[25]])

# Part II: 
# Likely need to make more efficient to run through 75 iterations 

# stones follow a linear history independent of each other 
# it is a deterministic process
# we can make a recursive process which tells us how many stones will be at the bottom step, where we save the intermediate results 

# there should be a way to do this caching using memoise: https://www.r-bloggers.com/2020/04/caching-in-r/
# here is an example of someone doing this using memoise in R: https://github.com/JeanDupin/AOC/blob/main/2024/Day11.R

# write function to operate on each individual stone 
library(memoise)
library(tidyverse)
update_s = function (s) {
  
  if (s == 0) return(1)
  if (nchar(s) %% 2 == 0) {
    return(list(substr(s, 1, nchar(s)/2) |> as.numeric(), 
           substr(s, nchar(s)/2 + 1, nchar(s)) |> as.numeric()))
  } 
  return(s * 2024)
}

update_s = memoise(update_s)


out = lapply(1:75, \(x) {
  s <<- update_stones(s) # co-pilot trick to track stones as a global 
})

length(out[[75]])
