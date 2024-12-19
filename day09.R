# Day 9
input = adventdrob::advent_input(day = 9, year = 2024, parse=F); input
input = input$x
# input = readLines("input9"); input

options(scipen=999) # big number coming out 

# Part I: 
# parse through the character string - alternating between length of file, and length of free space 

# first lets parse the input and make the resulting string 
s = c()
dmap = as.numeric(strsplit(input, "")[[1]])
file_name = 0

for (i in 0:(length(dmap)-1)) {

  if (i %% 2 == 0) {
    s = c(s, rep(file_name, dmap[i+1]))
    file_name = file_name + 1
  } else {
    s = c(s, rep(-1, dmap[i+1])) # use -1 to keep numeric
  }
  
}

# now we want to go in and replace the -1's with the numbers starting from the right 
replace_positions = which(s == -1)
number_positions = which(s != -1)

for (i in 1:length(replace_positions)) {
  s[replace_positions[i]] = s[number_positions[length(number_positions) - i + 1]]
}

# lob off after moving the numbers in - rather than removing 
s = s[1:length(number_positions)]

# compute the checksum - number's position times its value 
sum(s * (0:(length(s)-1)))




# Part II: 
#TODO

# now we can only move the numbers if their batches are available in the free space
# we only try to move the files once each time 

input = readLines("input9"); input
s = c()
dmap = as.numeric(strsplit(input, "")[[1]])
file_name = 0

for (i in 0:(length(dmap)-1)) {
  
  if (i %% 2 == 0) {
    s = c(s, rep(file_name, dmap[i+1]))
    file_name = file_name + 1
  } else {
    s = c(s, rep(-file_name, dmap[i+1])) # use neg of file_name to allow for easier identification of space batches 
  }
  
}; s

nums_to_move = sort(unique(s[s>0]), decreasing = T)

# get the patches of negative spaces

#TODO: need to look for the leftmost available space for each number, not just the leftmost open space 
for (i in nums_to_move) {
  i = 9
  space_position = which(s == max(s[s < 0])) # idx of the largest negative space - i.e. where we move to
  size = sum(grepl(i, s))
  
  if (size <= length(space_position)) {
    s[which(s == i)] = NA
    s[space_position[1:size]] = i
  }
  
}
s
