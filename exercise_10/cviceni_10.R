library(Biostrings)

SuffixArray <- function(DNA_string){
  DNA_string <- paste0(DNA_string, "$")
  shortenedStrings <- c(DNA_string)
  positions <- c(1)
  for (i in 2:(nchar(DNA_string))) {
    positions <- c(positions, i)
    DNA_string <- substr(DNA_string, 2, nchar(DNA_string))
    shortenedStrings <- c(shortenedStrings, DNA_string)
  }
  print(shortenedStrings)
  order_index <- order(shortenedStrings)
  shortenedStrings <- shortenedStrings[order_index]
  positions <- positions[order_index]
  
  result_all <- data.frame(Position = positions, Substring = shortenedStrings)
  result <- positions
  return(result)
}

DNA_string <- DNAString("GCATCAT")

suffix_array <- SuffixArray(DNA_string)
print(suffix_array)

InverseSuffixArray <- function(suffix_array){
  position_vector <- numeric(length(suffix_array))
  for (i in 1:length(suffix_array)) {
    min_index <- which.min(suffix_array)
    position_vector[i] <- min_index
    suffix_array[min_index] <- Inf
  }
  print(position_vector)
}

inverse_array <- InverseSuffixArray(suffix_array)
print(inverse_array)

LCPArray <- function(text, SA, ISA) {
  m <- length(text)
  LCP <- numeric(m + 1) 
  LCP[1] <- -1
  LCP[m + 1] <- -1
  l <- 0
  
  for (i in 1:m) {
    j <- ISA[i]
    
    if (j > 1) {
      k <- SA[j - 1]
      
      while (k + l <= m && i + l <= m && substr(text, k + l, k + l) == substr(text, i + l, i + l)) {
        l <- l + 1
      }
      
      LCP[j] <- l
      l <- max(l - 1, 0)
    }
  }
  LCP[m+1] <- -1
  return(LCP)
}

LCP <- LCPArray(DNA_string, suffix_array, inverse_array)
print(LCP)

BinarySearchSA <- function(Pattern, Text, SuffixArray) {
  minIndex <- 1
  maxIndex <- nchar(Text) 
  
  while (minIndex < maxIndex) {
    midIndex <- floor((minIndex + maxIndex) / 2)
    suffix <- substring(Text, SuffixArray[midIndex])
    
    if (Pattern <= suffix) {
      maxIndex <- midIndex
    } else {
      minIndex <- midIndex + 1
    }
  }
  First <- minIndex
  
  minIndex <- 1
  maxIndex <- nchar(Text)
  
  while (maxIndex > minIndex) {
    midIndex <- floor((minIndex + maxIndex) / 2)
    suffix <- substring(Text, SuffixArray[midIndex])
    
    if (suffix <= Pattern) {
      minIndex <- midIndex + 1
    } else {
      maxIndex <- midIndex
    }
  }
  Last <- maxIndex - 1
  
  if (Last < First) {
    return('Pattern není v Textu')
  } else {
    return(c(First, Last))
  }
}

Pattern <- "ARA"
Text <- "CARACAS"
SuffixArray <- SuffixArray(DNAString(Text))

vysledek <- BinarySearchSA(Pattern, Text, SuffixArray)
print(vysledek)
