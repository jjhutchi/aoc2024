library(tidyverse)
options(scipen=999)
# parse the main input
input = adventdrob::advent_input(day = 24, year = 2024, parse=F); input 
# input = readLines("input24") |> 
#   as_tibble() |> 
#   rename(x = value)

# parse input 
values = input |> 
  slice(1:90) |> 
  extract(x, into = c("name", "value"), 
          regex = "(.*): (.*)", convert = TRUE)

mappings = input |> 
  slice(92:313) |> 
  extract(x, into = c("name1", "operator", "name2", "result"), 
          regex = "(.*) (.*) (.*) -> (.*)", convert = TRUE)

# Part I: 
# loop through mappings pulling out values we already know 
remaining_rows = nrow(mappings)
while (remaining_rows > 0) {
  
  for (i in 1:nrow(mappings)) {
    candidate = mappings[i,]
    if (candidate$name1 %in% values$name & candidate$name2 %in% values$name) {
      
      value1 = values$value[values$name == candidate$name1]
      value2 = values$value[values$name == candidate$name2]
      result = switch(candidate$operator, 
                      "AND" = value1 & value2, 
                      "OR" = value1 | value2, 
                      "XOR" = value1 != value2)
      
      values = add_row(values, name = candidate$result, value = result)
      mappings = mappings[-i,]
    } else {
      next
    }
    remaining_rows = nrow(mappings)
  }
  
}

binary = values |> 
  filter(grepl("^z", name)) |> 
  arrange(desc(name)) |> 
  pull(value) |> 
  paste0(collapse="") 
  
# manually convert binary to decimal 
sum(as.numeric(rev(strsplit(binary, "")[[1]])) * 2^(0:(nchar(binary) - 1)))