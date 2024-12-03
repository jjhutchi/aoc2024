input = adventdrob::advent_input(day = 2, year = 2020, parse=F)
input

# Part I
input |> 
  tidyr::extract(col = x, 
                 regex = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)", 
                 c("min", "max", "char", "password"), convert = T) |> 
  dplyr::mutate(char_count = stringr::str_count(password, char), 
                is_valid = between(char_count, min, max)) |> 
  summarise(sum(is_valid))
  
# Part II 
input |> 
  tidyr::extract(col = x, 
                 regex = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)", 
                 c("pos1", "pos2", "char", "password"), convert = T) |> 
  dplyr::mutate(char1 = stringr::str_sub(password, pos1, pos1), 
                char2 = stringr::str_sub(password, pos2, pos2), 
                is_valid = xor(char1 == char, char2 == char)) |> 
  summarise(sum(is_valid))
