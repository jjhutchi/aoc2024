# Day 3

# part I
input = adventdrob::advent_input(day = 3, year = 2024, parse=F)
input

# part I 
pacman::p_load(stringr, dplyr)
  
pat = "mul\\(\\d+,\\d+\\)"

tibble(value = unlist(str_extract_all(input$x, pattern = pat))) |> 
  tidyr::extract(value, c("a", "b"), "mul\\((\\d+),(\\d+)", convert = T) |> 
  mutate(value = a * b) |> 
  summarise(part_1 = sum(value))

# part II 

# drop_pat = "don't\\(\\).*?do\\(\\)" # this removes everything between the first and last do - do not want to be greedy
# p2_string = gsub(drop_pat, "", input$x)
# 
# # my guess is that the REGEX is recursive and removes nested don'ts with this method, taking too many out. 
# 
# tibble(value = unlist(str_extract_all(p2_string, pattern = pat))) |>
#   tidyr::extract(value, c("a", "b"), "mul\\((\\d+),(\\d+)", convert = T) |>
#   mutate(value = a * b) |>
#   summarise(part_2 = sum(value))

pat_2 = "mul\\(\\d+,\\d+\\)|don't\\(\\)|do\\(\\)"
str_2 = str_extract_all(input$x, pattern = pat_2)

# got help from Riinu: https://bsky.app/profile/riinu.bsky.social/post/3lcflzs5svc2g
tibble(value = unlist(str_2)) |> 
  mutate(do = str_extract(value, "do\\(\\)|don't\\(\\)")) |> 
  tidyr::fill(do, .direction = "down") |> 
  filter(do != "don't()" | is.na(do)) |> 
  filter(grepl("^mul", value)) |> 
  tidyr::extract(value, c("a", "b"), "mul\\((\\d+),(\\d+)", convert = T) |> 
  summarise(part_2 = sum(a * b))  
