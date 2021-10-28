combat_batch_effect <- function(filtered_score_matrix,
                                batch,
                                adjustment_var,
                                outliers,
                                project,
                                interest_var){
  
  if(project == "TEPIC reprocessed IHEC data"){
    outliers <- unique(c(outliers,"R_ENCBS150HBC_ENCBS376RZJ",
                         "R_ENCBS559QNR_ENCBS568FYY_ENCBS945MCY",
                         "R_ENCBS853LFM","E_520VFV"))
  }
  # filtered_matrix <- filtered_score_matrix$data
  metadata <- as.data.frame(attr(filtered_score_matrix, "meta"))
  
  validate(
    need(!anyNA(metadata[,batch]), message = paste(batch, "has NAs and cannot be used in combat")),
    
    need(nlevels(as.factor(metadata[,batch])) > 1, message = paste(batch,"has less than 2 level",
                                                        "check levels using the pie chart"))
  )
  
  row_sum<-apply(filtered_score_matrix,1,sum)
  if(!length(which(row_sum == 0)) == 0){
    filtered_score_matrix<-filtered_score_matrix[!(row_sum == 0),]
  }
  if(length(outliers) != 0){
    filtered_score_matrix <- filtered_score_matrix[,!(colnames(filtered_score_matrix) %in% outliers)]
    metadata <- metadata[!(colnames(filtered_score_matrix) %in% outliers),]
  }
  
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
    interest_var = NULL
  }else{
    for (inter_var in interest_var){
      
      validate(
        need(!anyNA(metadata[,inter_var]), message = paste(inter_var, "has NAs and cannot be used to make the model")),

        need(nlevels(as.factor(metadata[,inter_var])) > 1, message = paste(inter_var,"has less than 2 level",
                                                        "check levels using the pie chart"))
      )
    }
  }
  
  #building the model matrix for ComBat
  if(is.null(adjustment_var)){
    if(is.null(interest_var)){
      #No adjustment and no interest vars
      modcombat = model.matrix(~1, data = metadata)
    }else{
      #No adjustment but interest vars
      modcombat = model.matrix(as.formula(paste0("~", paste(interest_var, collapse = "+"))), data = metadata)
    }
  }else{
    if(is.null(interest_var)){
      #No interest but adjustment vars
      modcombat = model.matrix(as.formula(paste0("~", paste(adjustment_var, collapse = "+"))), data = metadata)
    }else{
      #adjustment and interest vars
      modcombat = model.matrix(as.formula(paste0("~", paste(
        paste(interest_var, collapse = " + "),"+", paste(adjustment_var, collapse = " + ")
        )
        )),data = metadata)
    }
  }
  
  tryCatch({
    batch_adjusted_matrix <- ComBat(dat=filtered_score_matrix,
                                    batch=as.double(as.factor(metadata[,batch])),
                                    mod=modcombat, par.prior=TRUE, prior.plots=FALSE)    
  },error = function(e){
    error_message <- paste("Sorry! Something went wrong!",
                           "Original error message:",e)
    validate(
      need(FALSE, message = error_message)
    )
  })

  attr(batch_adjusted_matrix, "meta") <- metadata
  
  return(batch_adjusted_matrix)
}