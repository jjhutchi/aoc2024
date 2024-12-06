# Day 6

# Part I: 

# get data into a grid 
input = adventdrob::advent_input(day = 6, year = 2024, parse=F); input
s = strsplit(input$x, "") |> unlist()
G = matrix(s, nrow = nrow(input), byrow = T)
start = which(G == "^", arr.ind = T) |> as.vector()

# input = readLines("input6")
# s = strsplit(input, "") |> unlist()
# G = matrix(s, ncol = length(input), byrow = T)

# find here the starting (^) point is

# define the directions we can move in through the grid, in order: up, right, down, left
dirs = list(
  c(-1, 0),
  c(0, 1), 
  c(1, 0), 
  c(0, -1) 
) 

# start going up 
direction = 1

# track states
map = G
position = start
visited_counter = 1 # rather than 0 to include starting point

while (max(position) <= nrow(G) & min(position) >= 1) {
  
  position = position + dirs[[direction]]
  
  # check step out of grid 
  if (!(max(position) <= nrow(G) & min(position) >= 1)) {
    break
  }
  
  floor = map[position[1], position[2]]
  
  if (floor == ".") {
    visited_counter = visited_counter + 1
    map[position[1], position[2]] = "X"
    next
  } 
  
  if (floor == "X") {
    next
  } 
  
  if (floor == "#") {
    position = position - dirs[[direction]]
    direction = direction %% 4 + 1
    next
  }
}

print(visited_counter)
# map # test the sample data 


# Part II: 

# Is there a better way than to loop through placing a blocker at each possible square?
# we can write a function to check based on the blocked coord, and then furrr through them 

#' x, y: coordinates to add the block
#' We say it is a loop if we revisit an obstacle with the same direction 

closed_loop = function (x, y) { 
  
  map = G
  position = start
  direction = 1
  visited_obstacles = list()
  
  # stop if we try to drop obstacle on start point 
  if (map[x, y] == "^|#") {
    return(FALSE)
  } else {
    map[x, y] = "#" # add in the obstacle
  } 
 
  
  while (max(position) <= nrow(G) & min(position) >= 1) {
    
    position = position + dirs[[direction]]
    
    # make our identifier for position and direction 
    id = paste(position[1], position[2], direction)
    
    # check step out of grid, can return false 
    if (!(max(position) <= nrow(G) & min(position) >= 1)) {
      return(FALSE)
    }
    
    floor = map[position[1], position[2]]
    
    if (floor == ".") {
      visited_counter = visited_counter + 1
      map[position[1], position[2]] = "X"
      next
    } 
    
    if (floor == "X") {
      next
    } 
    
    if (floor == "#") {
      
      # stop if seen this before, otherwise back up, turn, keep going
      if (id %in% visited_obstacles) {
        return(TRUE)
      } else {
        visited_obstacles[[length(visited_obstacles) + 1]] = id
        position = position - dirs[[direction]]
        direction = direction %% 4 + 1
        next
      }
    }
  }
}

# out = closed_loop(10, 8)

library(parallel)

compute_loop =function(indices) {
  i =indices[1] |> as.numeric()
  j =indices[2] |> as.numeric()
  out =closed_loop(i, j)
  return(as.numeric(out))
}

combinations =expand.grid(1:nrow(G), 1:nrow(G))  # All (i, j) pairs

results =mclapply(1:nrow(combinations), function(idx) {
  compute_loop(combinations[idx, ])
}, mc.cores = detectCores() - 1)  # Use all available cores minus one for efficiency

valid_loops =sum(unlist(results))