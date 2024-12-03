input = adventdrob::advent_input(day = 3, year = 2020, parse=F)
input

# TODO: not working 

# Part I
input |> 
  mutate(idx = row_number() * 3 - 2, 
         idx = ifelse(idx > 31, idx %% 31, idx),
         path = substr(x, idx, idx), 
         valid = path == "#") |> 
  # print(n = 323)
  summarise(sum(valid))

path = input$x |> 
  strsplit("") |> 
  unlist()

idx = seq(1, length(path), 34)
sum(path[idx] == "#")


nchar(input[1, ]$x) # 31 items 


"." "#" "." "#" "#" "#" "." "#" "." "#" "#" "."