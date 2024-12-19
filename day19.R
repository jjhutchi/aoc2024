library(tidyverse)
options(scipen = 999)

# parse the main input
input = adventdrob::advent_input(day = 19, year = 2024, parse=F); input 
towels = strsplit(input$x[1], ", ")[[1]]
patterns = input[-(1:2), ]$x

# need to find all the ways to make the patterns using the towels 
# want to solve DP problem recursively, checking how many words can be made with the given string, and saving this to a cache
# followed this base R solution which uses collections::cache()
# link: https://github.com/AdroMine/AdventOfCode/blob/main/2024/Day19/solution.R


check_word = function (letters, word) {
  
  # if we've calculated already, get its value
  if (cache$has(word)) {
    return(cache$get(word))
  }
  
  # if the word is empty, we've found a valid word
  if (word == "") {
    return(1)
  }
  
  # recurse through the start of each word 
  ans = 0 
  for (l in letters) {
    
    if (startsWith(word, l)) {
      ans = ans + check_word(letters, substr(word, nchar(l) + 1, nchar(word)))
    }
    
  }
  
  # add result to the cache
  cache$set(word, ans)
  
  # return result for the word
  ans
}

cache = collections::dict()
possible_designs = sapply(patterns, \(x) check_word(towels, x))

# Part I: 
sum(possible_designs > 0)

# Part II: 
sum(possible_designs)
