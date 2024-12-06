# Day 5

# part I
input = adventdrob::advent_input(day = 5, year = 2024, parse=F)
input

rules = input[1:1177, ]$x
pages = input[1178:nrow(input), ]$x

input = readLines("input5")

rules = input[1:21]
pages = input[23:length(input)]

# Part I:
# Need to go through each of the pages and make sure that the position of the first word is always before the position of the second rule. 

# track pages valid here
remaining_pages = pages
correct_pages = list()

for (i in pages) {
  
  flag = TRUE
  for (r in rules) {
    
    first = strsplit(r, "\\|")[[1]][1]
    second = strsplit(r, "\\|")[[1]][2]
    
    idx_1 = which(strsplit(i, ",")[[1]] == first)
    idx_2 = which(strsplit(i, ",")[[1]] == second)
    
    if(any(length(idx_1)==0 | length(idx_2)==0)) {
      next
    }
    
    if (idx_1 > idx_2) {
      flag = FALSE
    }
  }
  if (flag) {
    correct_pages[[length(correct_pages) + 1]] = i 
  }
  
}

# get midpoint from correct_pages 
lapply(correct_pages, \(x) {
  x = strsplit(x[[1]], ",")[[1]]
  x[length(x) %/% 2 + 1] |> as.numeric()
}) |> unlist() |> sum() 


# Part II: 

# reorder the correct pages based on their rules 
# if incorrect, we can swap the positions of the numbers which are incorrect - maybe this will fix things if we run it enough?

incorrect_pages = pages[!pages %in% correct_pages]
updated_pages = list()
for (i in incorrect_pages) {
  
  flag = TRUE
  n = 0
  stop=FALSE
  while(stop == FALSE & n < 100) {
    for (r in rules) {
      
      r = rules[14]
      first = strsplit(r, "\\|")[[1]][1]
      second = strsplit(r, "\\|")[[1]][2]
      
      idx_1 = which(strsplit(i, ",")[[1]] == first)
      idx_2 = which(strsplit(i, ",")[[1]] == second)
      
      if(any(length(idx_1)==0 | length(idx_2)==0)) {
        next
      }
      
      if (idx_1 > idx_2) {
        flag = FALSE
        #TODO: swap the orders of the points here 
        # running into bugs...
        unlist(strsplit(i, ",")[1])[idx_1] = second
        i[idx_2] = first
      }
    }
    updated_pages[[length(updated_pages) + 1]] = updated_page
    n = n + 1
    if (flag) {
      stop = TRUE
    }
  }
}

updated_pages
