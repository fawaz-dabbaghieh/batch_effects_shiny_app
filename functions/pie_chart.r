pie_chart <- function(metadata, key){
  
  metadata <- as.data.frame(metadata)
  
  if(startsWith(key, "@")){
    validate(
      need(FALSE, message = "Please choose another key that doesn't have @ at the beginning!")
    )
  }
  values <- list()
  
  values[["unique_values"]] <- levels(factor(metadata[[key]]))
  values[["percentages"]] <- NA
  for(i in 1:length(values[[1]])){
    values[[2]][i] <- length(which(metadata[[key]] == values[[1]][i]))/nrow(metadata)*100
  }
  values <- as.data.frame(values)
  
  p <- plot_ly(as.data.frame(values), labels = ~unique_values, values = ~percentages, type = 'pie') %>%
    layout(title = paste("Values Distributin For",key),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  plotly_build(p)
}
