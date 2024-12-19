# Day 4

# part I
input = adventdrob::advent_input(day = 4, year = 2024, parse=F)
input

# Part I: 
# there are 8 ways XMAS can appear
# L->R, R->L, U->D, D->U, Diagonal U L->R, Diagonal U R->L, Diagonal D L->LR,Diagonal D R->L
# We can use th fact that this is a grid to pull out all the possible lines, and check each for XMAS 
# using the reverse also as a pattern cuts the checking in half -> horiz, verti, diag U, diag D

pat = "XMAS|SAMX"


# make grid so we can search through 
s = input$x
# s = readLines("test4")
G = matrix(unlist(strsplit(s, "")), nrow = length(s), byrow = TRUE)
rows = nrow(G)
cols = ncol(G)


count = 0
ans_grid = matrix(" ", nrow = rows, ncol = cols)

for (r in 1:rows) {
  for (c in 1:cols) {
    
    # L -> R
    if (c + 3 <= cols) {
      if (grepl(pat, paste0(G[r, c:(c+3)], collapse = ""))) {
        count = count + 1
        ans_grid[r, c:(c+3)] = G[r, c:(c+3)]
      }
    }
    
    # U -> D
    if (r + 3 <= rows) {
      if (grepl(pat, paste0(G[r:(r+3), c], collapse = ""))) {
        count = count + 1
        ans_grid[r:(r+3), c] = G[r:(r+3), c]
      }
    }
    
    # Up diagonal L -> R
    if (r - 3 >= 1 & c + 3 <= cols) { #TODO check the bounds of the grid here
      if (grepl(pat, paste0(G[r, c], G[r-1, c+1], G[r-2, c+2], G[r-3, c+3], collapse = ""))) {
        count = count + 1
        
        ans_grid[r, c] = G[r, c]
        ans_grid[r-1, c+1] = G[r-1, c+1]
        ans_grid[r-2, c+2] = G[r-2, c+2]
        ans_grid[r-3, c+3] = G[r-3, c+3]
        
      }
    }
    
    # Down diagonal L -> R
    if (r + 3 <= rows & c + 3 <= cols) {
      if (grepl(pat, paste0(G[r, c], G[r+1, c+1], G[r+2, c+2], G[r+3, c+3], collapse = ""))) {
        count = count + 1
        
        ans_grid[r, c] = G[r, c]
        ans_grid[r+1, c+1] = G[r+1, c+1]
        ans_grid[r+2, c+2] = G[r+2, c+2]
        ans_grid[r+3, c+3] = G[r+3, c+3]
      }
    }
  }
}
count
# ans_grid # for viz the cases, helpful to compare against the test case


# Part II: 

# Now we have it where we just need to check each point, and its surrounding corners. 
# I saw others' solutions here, there are 4 possible corner configs, 
# so we can grab the corners from any A, and check if its' corners are in our pattern

# s = readLines("test4")
s = input$x
G = matrix(unlist(strsplit(s, "")), nrow = length(s), byrow = TRUE)
rows = nrow(G)
cols = ncol(G)

corner_pat = "MMSS|MSSM|SSMM|SMMS"
ans_grid = matrix(" ", nrow = rows, ncol = cols)

count = 0
for (r in 2:(rows-1)) {
  for (c in 2:(cols-1)) {
    
    if (G[r,c] == "A") {
      corners = c(G[r-1, c-1], G[r-1, c+1], G[r+1, c+1], G[r+1, c-1])
      if (grepl(paste0(corners, collapse = ""), corner_pat)) {
        count = count + 1
        
        ans_grid[r, c] = G[r, c]
        ans_grid[r-1, c-1] = G[r-1, c-1]
        ans_grid[r-1, c+1] = G[r-1, c+1]
        ans_grid[r+1, c-1] = G[r+1, c-1]
        ans_grid[r+1, c+1] = G[r+1, c+1]
      }
    }
  }
}
count
# ans_grid
