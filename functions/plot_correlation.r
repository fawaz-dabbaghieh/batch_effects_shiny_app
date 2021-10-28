plot_correlation <- function(filtered_score_matrix,
                             project,
                             plot_title){
  
  correlation_matrix <- cor(filtered_score_matrix, use = "pairwise.complete.obs")
  
  if(project == "DEEP"){
    colnames(correlation_matrix) <- attr(filtered_score_matrix, "meta")$DEEP_SAMPLE_ID
    rownames(correlation_matrix) <- colnames(correlation_matrix)
  }else{
    colnames(correlation_matrix) <- attr(filtered_score_matrix, "meta")$experiment
    rownames(correlation_matrix) <- colnames(correlation_matrix)
    
  }

  arguments <- list(correlation_matrix,
                    order = "hclus",
                    title = plot_title,
                    mar = c(1,1,1,1),
                    tl.cex = 0.5,
                    tl.col = "black",
                    diag = FALSE)
  
  do.call(corrplot,args = arguments)

}