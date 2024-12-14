library(tidyverse)
input = adventdrob::advent_input(day = 13, year = 2024, parse=F); input

# computer rounding issue led to not picing up integers
options(scipen=999999)

# parse input to get the data 
df = input |> 
  as_tibble() |> 
  extract(
    col = x,
    into = c("name", "X", "Y"),
    regex = "(.*?): X[+=](\\d+), Y[+=](\\d+)",
    convert = TRUE
  ) |> 
  mutate(game = (row_number()-1) %/% 4)

solve_game = function(data) {
  
  # parse and make matrix with the values
  ax = data[1, ]$X
  ay = data[1, ]$Y
  bx = data[2, ]$X
  by = data[2, ]$Y
  px = data[3, ]$X
  py = data[3, ]$Y
  
  X = matrix(c(ax, bx, ay, by), nrow=2)
  P = matrix(c(px, py))
  P2 = P + 10000000000000
  
  ans = solve(t(X)) %*% P
  ans2 = solve(t(X)) %*% P2
  
  return(
    data.frame(cx = ans[1], cy = ans[2], 
               cx2 = ans2[1], cy2 = ans2[2])
  )
    
}

df %>%
  split(.$game) %>%
  map(solve_game) |> 
  bind_rows() |> 
  mutate(cx = round(cx, 1), 
         cy = round(cy, 1), 
         keep_1 = cx %% 1 == 0 & cy %% 1 == 0 & cx <= 100 & cy <= 100, 
         keep_2 = cx2 %% 1 == 0 & cy2 %% 1 == 0) |> 
  summarise(part_1 = sum(keep_1 * (3*cx + cy)), 
            part_2 = sum(keep_2 * (3*cx2 + cy2)))

