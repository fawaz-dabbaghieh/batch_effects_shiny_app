#Function to get the scoring matrix with tiling regions of the genome

score_matrix_tiling_regions <- function(experiments,
                                        experiments_info_meta,
                                        genome, chr,
                                        tiling_size = "5000",
                                        aggregation_function = "mean",
                                        filtering,
                                        variation = "0.05",
                                        user_key){
  
  # if(chr == ""){chr <- NULL}
  
  #Making a list with to call the VALUE column for each of our experiments
  experiments_columns <- list()
  for(experiment in deepblue_extract_names(experiments)) {
    experiments_columns[[experiment]] <- "VALUE"
  }
  
  #Getting the tiling regions
  tiling_regions <- deepblue_tiling_regions(size=tiling_size,
                                            chromosome = chr,
                                            genome= genome,
                                            user_key = user_key)
  
  
  #requesting the score matrix
  request_id <- deepblue_score_matrix(experiments_columns = experiments_columns,
                                      aggregation_function = aggregation_function,
                                      aggregation_regions_id=tiling_regions,
                                      user_key = user_key)
  
  #Checking for the job to finish
  while (deepblue_info(id = request_id, user_key = user_key)$state == "running"){
    
    showNotification("Calculating score matrix, still running",type = "message", duration = 9)
    Sys.sleep("10")
  }
  
  #score matrix
  #In case I wanted to print the messages that are shown on the console
  #I can use withCallingHandlers like done in this link
  #https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui
  score_matrix <- deepblue_download_request_data(request_id = request_id, user_key = user_key)

  #removing the chromosome, start and end columns
  filtered_score_matrix <- as.matrix(score_matrix[,4:ncol(score_matrix)])
  
  #keep only complete cases without NAs
  if(filtering == "filter_non_complete"){
    filtered_score_matrix <- filtered_score_matrix[which(complete.cases(filtered_score_matrix)),]
    
  }
  #optional filtering for variable regions
  else if(filtering == "row_variation"){
    filtered_score_matrix <- filtered_score_matrix[which(rowSums(is.na(filtered_score_matrix)) /
                                                           ncol(filtered_score_matrix) < variation),]
  }

  #keep only regions with more than 0.05 variance
  filtered_score_matrix_rowVars <- rowVars(filtered_score_matrix, na.rm = T)
  filtered_score_matrix <- filtered_score_matrix[which(filtered_score_matrix_rowVars > variation),]
  
  #rearranging the columns so they match the metadata
  
  filtered_score_matrix <- filtered_score_matrix[,order(match(colnames(filtered_score_matrix),
                                                              experiments_info_meta$experiment))]
  
  # score_matrix_col_sums <- colSums(is.na(filtered_score_matrix)) /
  #   nrow(filtered_score_matrix)
  # problematic_samples <- which(score_matrix_col_sums > 0.1)
  # 
  # if(length(problematic_samples) > 0){
  #   warning(paste("omitted samples with > 10% missing entries:", paste(colnames(filtered_score_matrix)[problematic_samples], collapse = ","), sep= " "))
  #   filtered_score_matrix <- filtered_score_matrix[,-problematic_samples]
  #   cat("% of missing values per sample:")
  #   print(score_matrix_col_sums)
  # }
  
  #using knn-impute for 
  filtered_score_matrix <- impute.knn(as.matrix(filtered_score_matrix), k = 3)
  filtered_matrix <- filtered_score_matrix$data
  
  attr(filtered_matrix, "meta") <- experiments_info_meta
  return(filtered_matrix)
}