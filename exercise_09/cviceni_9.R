rm(list=ls())
library(Biostrings)

Overlap <- function(seq1, seq2) {
  maxOverlap <- min(nchar(seq1), nchar(seq2))
  
  for (i in maxOverlap:1) {
    endIdxSeq1 <- nchar(seq1) - i + 1
    startIdxSeq2 <- 1
    
    if (endIdxSeq1 > 0) {
      if (substr(seq1, endIdxSeq1, nchar(seq1)) == substr(seq2, startIdxSeq2, i)) {
        return(i)
      }
    }
  }
  
  return(0)
}


seq1 <- "CATGC"
seq2 <- "ATGCATC"

result <- Overlap(seq1, seq2)
print(result)


Overlap_Matrix <- function(S) {
  M <- matrix(0, length(S), length(S))
  
  for (k in 1:length(S)) {
    for (l in 1:length(S)) {
      if (k != l) {
        M[k, l] <- Overlap(S[[k]], S[[l]])
      }
    }
  }
  return(M)
}
reads <- DNAStringSet(c("CATGC", "CTAAGT", "GCTA", "TTCA", "ATGCATC"))
result <- Overlap_Matrix(reads)
print(result)

GreedySuperstring <- function(S) {
  while (length(S) > 1) {
    overlapMat <- Overlap_Matrix(S)
    maxOverlap <- max(overlapMat)
    
    
    if (maxOverlap == 0){
      return(S)
    }
    else{
    indices <- which(overlapMat == maxOverlap, arr.ind = TRUE)
    
    seq1 <- S[[indices[1, 1]]]
    seq2 <- S[[indices[1, 2]]]
    
    mergedSeq <- paste(seq1, substr(seq2, maxOverlap + 1, nchar(seq2)), sep = "")
    S <- c(S, mergedSeq)
    S <- S[-c(indices[1, 1], indices[1, 2])]
    }
  }
  return(S)
}

#reads <- DNAStringSet(c("CATGC", "CTAAGT", "GCTA", "TTCA", "ATGCATC"))
reads <- DNAStringSet(c("ATC", "TCAGAG", "ATG", "AGCCAT", "TGCAT"))

result <- GreedySuperstring(reads)
print(result)
