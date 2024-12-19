# Day 2

# part I
input = adventdrob::advent_input(day = 2, year = 2024, parse=F)
input |> strsplit(input$x, " ") |> as.matrix()

not_safe = 0
for (l in 1:nrow(input)) {
  
  line = input[l, ]
  nums = strsplit(line$x, " ")[[1]] |> as.numeric()
  
  d = diff(nums)
  if (all(d > 0) | all(d < 0)) {
    if (all(between(abs(d), 1, 3))) {
      next
    }
  }
  
  not_safe = not_safe + 1
  
}
nrow(input) - not_safe


# Part II 

# got stuck and had to rely on drob's code - good practice here for using tidyverse
# source: https://bsky.app/profile/drob.bsky.social/post/3lccf7y5b5c2r
pacman::p_load(purrr, stringr, dplyr)

is_safe = function (report) {
  d = diff(report)
  return((all(d > 0) | all(d < 0)) & all(between(abs(d), 1, 3)))
}

is_safe_2 = function(report) {
  return(is_safe(report) | any(map_lgl(seq_along(report), \(x) is_safe(report[-x]))))
}

input$x |> 
  str_split(" ") |> 
  map(as.numeric) |> 
  map_lgl(is_safe) |> 
  sum()
 

