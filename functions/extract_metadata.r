#making the data.frame from the information
extract_metadata <- function(experiments_info){
  experiments_info_meta <- unique(foreach(sample = experiments_info,
                                          .combine = bind_rows,
                                          .inorder = TRUE) %do% {
                                            meta <- as.data.frame(sample$sample_info)
                                            meta$epigenetic_mark <- sample$epigenetic_mark
                                            meta$data_type <- sample$data_type
                                            meta$genome <- sample$genome
                                            meta$experiment <- sample$name
                                            meta$technique <- sample$technique
                                            meta$format <- sample$format
                                            meta <- cbind(meta, sample$extra_metadata)
                                            return(meta)
                                          })
  return(experiments_info_meta)
}