# Day 1 

# part 1
input = adventdrob::advent_input(day = 1, year = 2024, parse=F)

left = substr(input$x, 1, 5) |> as.numeric()
right = substr(input$x, 9, 14) |> as.numeric()
sum(abs(sort(left) - sort(right)))


# part 2 
multiplier = lapply(left, \(x) {
  grepl(x, right) |> sum()
}) |> unlist()

left %*% multiplier
