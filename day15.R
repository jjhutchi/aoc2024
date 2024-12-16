library(tidyverse)

# work off the test input
# input = readLines("input15")
# G = input[1:10]
# G = strsplit(x = G, "") |> unlist() |> matrix(nrow = 10, byrow = T)
# steps = input[11:length(input)] |> strsplit("") |> unlist()


# # parse the main input 
input = adventdrob::advent_input(day = 15, year = 2024, parse=F); input
G = input[1:50, ]
G = strsplit(x = G$x, "") |> unlist() |> matrix(ncol = 50, byrow = T)
steps = input[52:nrow(input), ]$x |>  strsplit("") |> unlist()

# Part I: 

# update the state, and return the updated grid 
take_step = function (grid, movement, part_2=FALSE) {
  
  # directions of movement  
  movements = list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  movement_names = c("^", "v", "<", ">")
  dr = movements[[which(movement==movement_names)]][1]
  dc = movements[[which(movement==movement_names)]][2]
  
  start = which(grid == "@", arr.ind = T)
  cr = start[1]
  cc = start[2]
  
  # check if the next step is a wall, then stop
  if (grid[cr + dr, cc + dc] == "#") {
    return(grid)
  }
  
  # find the set of boxes to move based on the step
  # we do this by keep moving in this area until we hit a "." or a wall "#"
  # if we hit a wall, nothing moves
  # if we hit an open spot, we move everything over in that direction 
  
  nr = cr + dr # next step row
  nc = cc + dc # next step col
  keep_moving = TRUE
  room_to_move = FALSE
  
  while (keep_moving) {
    
    if (grid[nr, nc] == "#") {
      keep_moving = FALSE
      room_to_move = FALSE
    }
    if (grid[nr, nc] == ".") {
      keep_moving = FALSE
      room_to_move = TRUE
    }
    if (grid[nr, nc] == "O") {
      nr = nr + dr
      nc = nc + dc
    }
  
  }
  
  if (room_to_move) {
    # set the spaces until the first open space as a box
    grid[(cr+dr) : nr, (cc+dc) : nc] = "O" 
    grid[cr, cc] = "."
    grid[(cr + dr), (cc + dc)] = "@"
  }
  
  return(grid)
}

# Simulate through the steps 
for (i in 1:length(steps)) {
  G <- take_step(G, steps[i])
}


coords = which(G == "O", arr.ind = T)
sum((coords[, 1] - 1) * 100 + (coords[, 2] - 1)) # since R is 1 indexed 


# Part II: 

# update the grid for w i d e boxes 
G = input[1:50, ]
G = G$x
G = gsub("#", "##", G)
G = gsub("O", "[]", G)
G = gsub("\\.", "\\..", G)
G = gsub("@", "@.", G)
G = strsplit(x = G, "") |> unlist() |> matrix(ncol = 50, byrow = T)

#TODO: need to add in logic where we can use the wide boxes to push eachother if they're in contact 