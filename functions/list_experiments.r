# This function calls for experiments chosen by the user and filter only the files with the "VALUE"
# Column after getting all the info.

list_experiments <- function(genome,
                             epigenetic_mark,
                             project,
                             user_key,
                             filter_coverage = TRUE){

  incProgress(amount = 1, message = "Listing all experiments")
  
  #Getting experiments
  experiments <- deepblue_list_experiments(genome = genome,
                                           epigenetic_mark = epigenetic_mark,
                                           project = project,
                                           user_key = user_key)
  #In case no experiments returned, the function will just return an empty line
  if (experiments[1] == "\n"){
    return(NULL)
  }
  
  #filter tepic experiments
  if(project == "TEPIC reprocessed IHEC data"){
    experiments <- experiments[grep(pattern = "*.reg.rpm.bed", deepblue_extract_names(experiments)),]
  }
  
  #filtering the coverage out
  if(filter_coverage == TRUE){
    coverage_files <- grep("coverage", experiments$name, ignore.case = TRUE)
    if(length(coverage_files) != 0){
      experiments <- experiments[-coverage_files]
    }
    }
  
  #Getting experiments info
  incProgress(amount = 1, message = "Getting information for listed experiments")
  experiments_info <- deepblue_info(id = experiments$id, user_key = user_key)

  #From the list of experiments, I only need to keep the ones with the VALUE column
  format <- "CHROMOSOME,START,END,VALUE"
  experiments_to_keep <- list()
  
  incProgress(amount = 1, message = "Filtering experiments")
  
  for (i in 1:length(experiments_info)){
    if (experiments_info[[i]]$format == format){
      experiments_to_keep[i] <- i
    }
  }
  
  if(length(experiments_to_keep) != 0){
    experiments_info <- experiments_info[unlist(experiments_to_keep)]
    experiments <- experiments[unlist(experiments_to_keep)]
  }
  
  #Filter experiments with missing metadata
  if (length(grep("Warning",
                  experiments_info, ignore.case = TRUE)) == 0){
    experiments <- experiments
  }else{
    to_remove <- as.vector(grep("Warning",experiments_info, ignore.case = TRUE))
    experiments <- experiments[-to_remove,]
    experiments_info <- experiments_info[-to_remove]
  }
  
  #Return a list with two elements, First is the lsit of experiments
  #the second is all the information in a list
  all_experiments <- list(experiments, experiments_info)
  incProgress(amount = 0, message = "Done!!")

  return(all_experiments)
}
