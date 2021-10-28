plotting_rna_seq_ontology_compare <- function(filtered_score_matrix,
                                              batch_adjusted_matrices){
  
  #Loading distance matrices
  ###Read Cosine Distance Matrix###
  cosineDistance<-read.table("../distance_matrices/Cosine-Similiarity-Based-Distance-Matrix.txt", header= T, stringsAsFactors = F)
  ###Generate Similiarity Matrix###
  cosineSimilarity<-(1-cosineDistance)
  
  ###Read Jaccard Distance Matrix###
  jaccardDistance<-read.table("../distance_matrices/Jaccard-Based-Distance-Matrix.txt", header = T, stringsAsFactors = F)
  ###Generate Similiarity Matrix###
  jaccardSimilarity<-(1-jaccardDistance)
  
  
}