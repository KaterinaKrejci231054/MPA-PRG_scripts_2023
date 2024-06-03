#' Align two sequences globally using Hirschberg's algorithm
#' 
#' @param X DNAString object representing NT or AA sequence to align
#' @param Y DNAString object representing NT or AA sequence to align
#' @param align A list of DNAString objects with alignment of input sequences
#' @param match An integer value of a score for matching bases
#' @param mismatch An integer value of score for mismatching bases
#' @param gap An integer value of penalty for gap insertion

require(Biostrings)

HirschbergTemplate <- function(X, Y, align, match, mismatch, gap){
  
  first_align_row <- align[[1]] # initialize the first row of alignment
  second_align_row <- align[[2]] # initialize the second row of alignment
  
  
  if (length(X) == 0) # length of X is equal to zero
  {for (i in (1:length(Y))){
    first_align_row = c(first_align_row, DNAString('-'))
    second_align_row = c(second_align_row, Y[i])
    align <- c(DNAStringSet(first_align_row), DNAStringSet(second_align_row))
    print(align)
  }
  }
  else if (length(Y) == 0) # length of Y is equal to zero
  {for (i in (1:length(X))) # for each character in X
  {
    first_align_row <- c(first_align_row, X[i]) # add character from X
    second_align_row <- c(second_align_row, DNAString('-'))# add gap
  }
    align <- c(DNAStringSet(first_align_row), DNAStringSet(second_align_row))
    print(align)
  }
  else if (length(X) == 1 & length(Y) == 1)# length of X and Y is equal to 1
  { first_align_row <- c(first_align_row, X[1]) # add character from X
  second_align_row <- c(second_align_row, Y[1]) # add character from Y
  align <- c(DNAStringSet(first_align_row), DNAStringSet(second_align_row))
  print(align)
  }
  else
  {
    xlen <- length(X) # length of X
    xmid <- length(X)/2 # half of the length of X
    ylen <- length(Y) # length of Y
    
    left_score <- NWScore(X[1:xmid], Y, match, mismatch, gap) # NW score for the first half of X and the whole Y
    right_score <- NWScore(rev(X[(xmid+1):xlen]), rev(Y), match, mismatch, gap) # NW score for the second half of X and the whole Y (both are reversed)
    ymid <- which.max (left_score + rev(right_score)) - 1 # index of division for Y
    
    # The first half
    if (ymid == 0) # index of division for Y is equal to 0
    {
      align <- HirschbergTemplate(X[1:xmid], DNAString(), align, match, mismatch, gap) # call Hirschberg function for the first half of X and for an empty DNAString object
    }
    else
    {
      align <- HirschbergTemplate(X[1:xmid], Y[1:ymid], align, match, mismatch, gap) # call Hirschberg function for the first half of X and for the first part of Y
    }
    
    # The second half
    if ((xmid + 1) > xlen) # X cannot be further divided
    {
      align <- HirschbergTemplate(DNAString(),Y[(ymid+1):ylen], align, match, mismatch, gap) # call Hirschberg function for an empty DNAString object and the second half of Y
    }
    else if ((ymid + 1) > ylen) # Y cannot be further divided
    {
      align <-HirschbergTemplate(X[(xmid+1):xlen], DNAString(), align, match, mismatch, gap) # call Hirschberg function for the second half of X and for an empty DNAString object
    }
    else 
    {
      align <- HirschbergTemplate(X[(xmid+1):xlen], Y[(ymid+1):ylen], align, match, mismatch, gap) # call hirschberg function for the second half of X and the second part of Y
    }
  }
  return(align)
}

NWScore <- function(X, Y, match, mismatch, gap){
  m <- length(X)
  n <- length(Y)
  S <- (0:n) * gap
  for (i in 2:(m+1)) {
    s <- S[1]
    c <- S[1] + gap
    S[1] <- c
    for (j in 2:(n+1)){
      if (X[i-1]==Y[j-1]){pom <- match} else {pom <- mismatch}
      c <- max(S[j]+gap,c+gap,s+pom)
      s <- S[j]
      S[j] <- c
    }
  }
  return(S)
}


seq1 = DNAString("AGTACGCA")
seq2 = DNAString("TATGC")
align = list(DNAString(''), DNAString(''))
match = 2
mismatch = -1
gap = -2

example = HirschbergTemplate(seq1, seq2, align, match, mismatch, gap)
print(example)

