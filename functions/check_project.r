#Function to check which projects corrisponds to selected genome

check_project <- function(genome, user_key){
  
  
  rules <- c("mm9:ENCODE","mm10:CEEHRC","mm10:DEEP (IHEC)",
             "mm10:ENCODE","hg19:CEEHRC","hg19:CREST",
             "hg19:ChIP-Atlas","hg19:DEEP (IHEC)","hg19:ENCODE",
             "hg19:Roadmap Epigenomics","hs37d5:DEEP","GRCh38:BLUEPRINT Epigenome",
             "GRCh38:BLUEPRINT HSC differentiation","GRCh38:CREST","GRCh38:DEEP",
             "GRCh38:ENCODE","GRCm38:DEEP","GRCh38:TEPIC reprocessed IHEC data")
  
  projects <- c()
  for(i in grep(genome, rules)){
    projects <- c(projects, strsplit(rules[i], ":")[[1]][2:length(strsplit(rules[i], ":")[[1]])])
  }
  
  #Checking if the user has access to DEEP
  if("DEEP" %in% deepblue_list_projects(user_key = user_key)$name){
    return(projects)
  }else{
    projects <- projects[-grep("DEEP", projects)]
    
    if(is.null(projects)){
      return(c(""))
    }
    return(projects)
  }
}

