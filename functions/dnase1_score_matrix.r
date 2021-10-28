dnase1_score_matrix <- function(dnase1_experiments,
                                experiments_info_meta,
                                chr,
                                user_key){
  
  if(chr == ""){chr <- NULL}
  
  # # get experiments for DNAse data ----
  # DNAse_exps <- deepblue_list_experiments(project = "TEPIC reprocessed IHEC data", user_key = user_key)
  # DNAse_exps <- DNAse_exps[grep(pattern = "*.reg.rpm.bed", deepblue_extract_names(DNAse_exps)),]
  # 
  # # prepare fetching data ----
  # 
  # # values are stored as RPM
  score_matrix_cols <- deepblue_select_column(dnase1_experiments, column = "RPM")
  
  # we already have regions of the regulatory build, so instead of using an
  # annotation we use the regions as they are as boundaries
  DNase_reg_regions <- deepblue_select_regions(deepblue_extract_names(dnase1_experiments)[1],
                                               genome = "GRCh38",
                                               epigenetic_mark = "DNA Accessibility",
                                               chromosomes = chr,
                                               project = "TEPIC reprocessed IHEC data")
  
  # request data ----
  
  # we use the above regions to download the results as a score matrix
  # an aggregation function is absolutely needed but we use max since there will
  # be only one value listed for each region
  score_matrix_query <- deepblue_score_matrix(experiments_columns = score_matrix_cols,
                                              aggregation_function = "max",
                                              aggregation_regions_id = DNase_reg_regions)
  
  #Checking for the job to finish
  while (deepblue_info(id = score_matrix_query, user_key = user_key)$state == "running"){
    showNotification("Calculating score matrix, still running",type = "message", duration = 9)
    Sys.sleep("10")
  }
  
  # we check if this worked
  # deepblue_info(score_matrix_query)$state
  
  # fetch results ----
  # we download the results as a matrix
  score_matrix <- deepblue_get_request_data(score_matrix_query)
  
  #For now I'm converting the character I'm getting into a table
  #Need to be fixed from the DeepBlue server
  score_matrix <- data.table::fread(score_matrix)
  score_matrix <- (score_matrix[,-c(1:3)])
  score_matrix <- as.matrix(score_matrix)

  attr(score_matrix, "meta") <- experiments_info_meta
  return(score_matrix)
}