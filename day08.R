# Day 8

# Part I: 
library(dplyr)

# get data into a grid 
input = adventdrob::advent_input(day = 8, year = 2024, parse=F); input
s = strsplit(input$x, "") |> unlist()
G = matrix(s, nrow = nrow(input), byrow = T)

# we want to keep track of the locations of each distinct pattern
# then loop through all of the pairs within each pattern, and add their bounding
# antinodes to the grid, then count the number of spaces with antinodes

# get the unique patterns
patterns = unique(unlist(s))
patterns = patterns[!patterns=="."]

# loop through the patterns, and add their antinodes to the grid
add_points = function(x1, x2) {
  
  out_x_1 = 2 * x1[1] - x2[1]
  out_y_1 = 2 * x1[2] - x2[2]
  
  out_x_2 = 2 * x2[1] - x1[1]
  out_y_2 = 2 * x2[2] - x1[2]
  
  return(
    list(
      c(out_x_1, out_y_1),
      c(out_x_2, out_y_2)
    )
  ) 
}


antinodes = list()

for (x in patterns) {
  # get the coordinates 
  coords = which(G == x, arr.ind = T)
  
  # loop through the coordinates so we can check all the pairs
  for (i in 1:nrow(coords)) {
    for (j in 1:(nrow(coords) - 1)) {
      if (i == j) {
        next
      }
      
      antinodes[[length(antinodes) + 1]] = add_points(coords[i,], coords[j,])
    }
  }
}

# count up the number of unique positions in the grid
antinodes |> 
  dplyr::bind_rows() |> 
  dplyr::filter(row %in% 1:50, 
         col %in% 1:50) |> 
  dplyr::distinct()

# Part II - take the locations from Part I, and solve for their new points based on their locations 
#TODO: need to come back to
  
  

