#mapping sample names to experiments names reprocessed data
tepic_sample_id_mapping <- function(sample_cols,
                                    user_key){
  
  experiments <- deepblue_list_experiments(project = "TEPIC reprocessed IHEC data",
                                           user_key = user_key)
  
  metadata_exp <- deepblue_info(experiments$id, user_key = user_key)
  
  sample_name <- list()
  
  for (i in 1:length(metadata_exp)){
    sample_name[["sample"]][i] <- metadata_exp[[i]]$sample_id
    sample_name[["name"]][i] <- strsplit(metadata_exp[[i]]$name,
                                         split = "[.]")[[1]][1]
  }
  
  all_samples <- list()
  
  for (i in 1:length(sample_cols)){
    if(sample_cols[i] %in% sample_name$sample){
      all_samples[["id"]][i] <- sample_cols[i]
      all_samples[["name"]][i] <- sample_name$name[match(sample_cols[i],
                                                        sample_name$sample)[1]]
      
    }else{
      all_samples[["id"]][i] <- sample_cols[i]
      all_samples[["name"]][i] <- NA
    }
  }
  
  return(all_samples)
}