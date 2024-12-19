# Day 12

# input = adventdrob::advent_input(day = 12, year = 2024, parse=F); input
# s = strsplit(input$x, "") |> unlist(); s
# G = matrix(s, nrow = nrow(input), byrow = T)

input = readLines("input12")
s = strsplit(input, "") |> unlist(); s
G = matrix(s, nrow = length(input), byrow = T)

# Part I:
# parse the grid looking for each cluster of letters 
# keep track of their outside perimeter and the count of them in each grid

nrow = nrow(G)
ncol = ncol(G)

# store the area and permieters in here 
dims = list()
directions = list(c(-1, 0), c(1, 0), c(0, 1), c(0, -1))

pattern = ""
for (r in 1:nrow) {
  for (c in 1:ncol) {
    
  }
}

# get the letters 
letters = unique(as.vector(G))

# parse the locations of the letters 
letter_loc = which(G == letters[1], arr.ind = T) |> as_tibble()
letter_area = nrow(letter_loc)

# parse the coordinates to get the perimeter 
# this fails when there are disconnected letters - i.e. C
tibble = G |> 
  as_tibble() |> 
  mutate(row = row_number()) |> 
  tidyr::pivot_longer(cols = -row, names_to = "col", values_to = "letter") |> 
  mutate(col = as.numeric(gsub("V", "", col)))

tibble |> 
  filter(letter == "C")

tibble |> 
  mutate(
    left = (letter[row] == letter[row] & letter[col - 1] == letter[lead(col)] & col >= 2),
    right = (letter[row] == letter[row] & letter[col + 1] == letter[lead(col)] & col <= ncol),
    below = (letter[row + 1] == letter[lead(row)] & letter[col] == letter[lead(col)] & row <= nrow),
    top = (letter[row - 1] == letter[lead(row)] & letter[col] == letter[lead(col)] & row >= 2)
  ) |> 
  group_by(letter) |> 
  summarise(
    area = n(), 
    perimeter = sum(!right, !left, !below, !top, na.rm=T)
  )


shared_edges <- letter_loc |>
  mutate(
    left = (row == row & col - 1 == lead(col)),
    right = (row == lead(row) & col + 1 == lead(col)),
    below = (row + 1 == lead(row) & col == lead(col)),
    top = (row - 1 == lead(row) & col == lead(col))
  ) |> 
  summarise(
    external_edges = sum(right, left, below, top, na.rm=T)
    )
  pull(total_shared)

# Calculate perimeter
total_edges <- nrow(letter_loc) * 4  # Each point has 4 edges
perimeter <- total_edges - (2 * shared_edges)  # Subtract shared edges (counted twice)

