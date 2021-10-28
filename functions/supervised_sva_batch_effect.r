supervised_sva_batch_effect <- function(filtered_score_matrix,
                                        adjustment_var,
                                        interest_var,
                                        project,
                                        outliers = NULL){
  
  
  # if(outliers == ""){outliers = NULL}
  if(project == "TEPIC reprocessed IHEC data"){
    outliers <- unique(c(outliers,"R_ENCBS150HBC_ENCBS376RZJ",
                         "R_ENCBS559QNR_ENCBS568FYY_ENCBS945MCY",
                         "R_ENCBS853LFM","E_520VFV"))
  }
  # filtered_matrix <- filtered_score_matrix$data
  metadata <- attr(filtered_score_matrix, "meta")
  
  #validation of the inptus
  #The variable selected should have more than 1 level
  if(adjustment_var == ""){
    adjustment_var = NULL
  }else{
    for (adj_var in adjustment_var){
      
      validate(
        need(!anyNA(metadata[,adj_var]), message = paste(adj_var, "has NAs and cannot be used to make the model")),
        need(nlevels(as.factor(metadata[,adj_var])) > 1, message = paste(adj_var,"has less than 2 level",
                                                              "check levels using the pie chart"))
      )
    }
  }
  
  if(interest_var == ""){
    validate(
      need(FALSE, message = "You need to choose a variable of interest for the full model in SVA")
    )
  }else{
    for (inter_var in interest_var){
      
      validate(
        need(!anyNA(metadata[,inter_var]), message = paste(inter_var, "has NAs and cannot be used for the model")),
        need(nlevels(as.factor(metadata[,inter_var])) > 1, message = paste(inter_var,"has less than 2 level",
                                                                "check levels using the pie chart"))
      )
    }
  }
  
  #calculating controls
  row_sum<-apply(filtered_score_matrix,1,sum)
  if(!length(which(row_sum == 0)) == 0){
    filtered_score_matrix<-filtered_score_matrix[!(row_sum == 0),]
  }
  if(outliers != ""){
    filtered_score_matrix <- filtered_score_matrix[,!(colnames(filtered_score_matrix) %in% outliers)]
    metadata <- metadata[!(colnames(filtered_score_matrix) %in% outliers),]
  }
  
  if(is.null(adjustment_var)){
    #No interest variable, mod0 is the intercept, full mod is the interest_var
    mod0 <- model.matrix(~1, data = metadata)
    mod <- model.matrix(as.formula(paste0("~", paste(interest_var, collapse = "+"))),
                        data = metadata)
    
  }else{
    mod0 <- model.matrix(as.formula(paste0("~", paste(adjustment_var, collapse = "+"))),
                         data = metadata)
    
    mod <- model.matrix(as.formula(paste0("~", paste(
      paste(interest_var, collapse = " + "),"+", paste(adjustment_var, collapse = " + ")
    )
    )),data = metadata)
  }
  
  
  rankedData<-apply(filtered_score_matrix,2,rank)
  meanRanks<-apply(rankedData,1,mean)
  stdRanks<-apply(rankedData,1,sd)
  rankesOfStd<-rank(stdRanks)
  controlProb<-1-(rankesOfStd/max(rankesOfStd))
  
  n.sv <- max(num.sv(filtered_score_matrix, mod, method = "leek"),10)
  showNotification(paste("The number of latent factors estimated is", n.sv), duration = 3)
  
  tryCatch({
    sva_object <- svaseq(filtered_score_matrix, mod, mod0,
                         n.sv = n.sv,
                         controls = controlProb)    
  },error = function(e){
    error_message <- paste("Sorry! Something went wrong!",
                           "Original error message:",e)
    validate(
      need(FALSE, message = error_message)
    )
  })
  

  
  nmod <- dim(mod)[2]
  mod <- cbind(mod, sva_object$sv)
  gammahat <- (log2(filtered_score_matrix+1) %*% mod %*% solve(t(mod) %*% mod))[, (nmod + 1):(nmod + n.sv)]
  batch_adjusted_matrix <- log2(filtered_score_matrix + 1) - gammahat %*% t(sva_object$sv)
  
  attr(batch_adjusted_matrix, "meta") <- metadata
  
  return(batch_adjusted_matrix)
}