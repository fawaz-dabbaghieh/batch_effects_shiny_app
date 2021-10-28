matrix_summary_table <- function(matrix_summary, col_names){
  for(i in 1:length(matrix_summary)){
    matrix_summary[i] <- gsub(" ","",strsplit(matrix_summary[i], ":")[[1]][2])
  }
  
  summary_table <- matrix(matrix_summary, ncol=6,byrow = TRUE)
  colnames(summary_table) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
  rownames(summary_table) <- col_names
  
  return(as.data.frame(summary_table))
}
