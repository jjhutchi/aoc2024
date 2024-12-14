library(tidyverse)
input = adventdrob::advent_input(day = 14, year = 2024, parse=F); input

floor_width = 101
floor_height = 103

# parse the input 
df = input |> 
  as_tibble() |> 
  extract(
    col = x,
    into = c("px", "py", "vx", "vy"),
    regex = "p=(.*),(.*) v=(.*),(.*)",
    convert = TRUE, remove = FALSE)

compute_position = function (data, steps) {
  data |> 
    mutate(pos_x = px + vx * steps, 
           pos_y = py + vy * steps, 
           pos_x_mod = pos_x %% floor_width, 
           pos_y_mod = pos_y %% floor_height)
}

compute_position(df, 100) |> 
  mutate(
    x_area = case_when(
      pos_x_mod < floor(floor_width/2) ~ "left",
      pos_x_mod > floor(floor_width/2) ~ "right",
      TRUE ~ "center"), 
    y_area = case_when(
      pos_y_mod < floor(floor_height/2) ~ "top",
      pos_y_mod > floor(floor_height/2) ~ "bottom",
      TRUE ~ "center"
  )) |>
  group_by(x_area, y_area) |>
  summarise(n = n()) |> 
  ungroup() |> 
  filter(x_area != "center" & y_area != "center") |> 
  summarise(part_1 = prod(n))


# Part II: 

# use the fact that there will not be any overlapping points in the position with the tree

check_position_counts = function (data) {
  data |> 
    group_by(pos_x_mod, pos_y_mod) |> 
    summarise(n = n()) |> 
    ungroup() |> 
    summarise(max_n = max(n)) |> 
    pull(max_n)
}

i = 0
while (max_n > 1 & i < 10000) {
  df = compute_position(df, i)
  max_n = check_position_counts(df) |> suppressMessages()
  i = i + 1
}

# ans: 7790
df |> 
  ggplot(aes(x = pos_x_mod, y = pos_y_mod)) +
  geom_tile() + 
  scale_y_reverse() + 
  theme_minimal() + 
  coord_fixed() + 
  theme_void()


