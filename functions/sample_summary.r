sample_summary <- function(sample_id, user_key){
  
  #If deepblue returns an error it looks bad on the app
  #So I'm catching the error and returning NA then using validate from shiny to return a message to user
  
  x <- tryCatch(
    {
      deepblue_info(id = sample_id, user_key = user_key)
      },
    error = function(cond){
      return("NA")
      }
    )

  
  validate(
    need(x != "NA", "We couldn't find information on this sample ID")
  )
  
  #Making a data frame from the metadata of the sample
  meta <- as.data.frame(x$sample_info)
  meta$type <- x$type
  meta$project <- x$project
  meta$epigenetic_mark <- x$epigenetic_mark
  meta$data_type <- x$data_type
  meta$genome <- x$genome
  meta$experiment <- x$name
  meta$technique <- x$technique
  meta$format <- x$format
  meta <- cbind(meta, x$extra_metadata)
  meta <- cbind(meta, x$upload_info)
  meta <- as.data.frame(t(meta))
  meta$keys <- rownames(meta)
  meta <- meta[,c(2,1)]
  row.names(meta) <- 1:dim(meta)[1]
  colnames(meta) <- c("keys", "vlues")
  
  return(meta)
}
