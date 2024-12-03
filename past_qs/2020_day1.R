input = adventdrob::advent_input(day = 1, year = 2020, parse=F)
input$x = as.numeric(input$x)

for (i in seq_along(input$x)) {
  
  i = input$x[i]
  
  for(j in seq_along(input$x)) {
    
    j = input$x[j]
    
    for(k in seq_along(input$x)) {
      
      k = input$x[k]
      
      if(sum(i, j, k) == 2020) {
        print(i * j * k)
      }
    }
  }
}

