library(tidyverse)
library(igraph) # great tool for the job today 

# parse the main input
input = adventdrob::advent_input(day = 23, year = 2024, parse=F); input 

# Part I: 

g = input |> 
  extract(col = "x", 
          into = c("c1", "c2"), 
          regex = "(.*)-(.*)")

graph = igraph::graph_from_data_frame(g, directed = F) 
graph_of_three = cliques(graph, min = 3, max=3)
sapply(graph_of_three, \(x) any(grepl("^t", names(x)))) |> sum()

# Part II: 

lan_party = largest_cliques(graph)
paste0(sort(names(lan_party[[1]])), collapse=",")
