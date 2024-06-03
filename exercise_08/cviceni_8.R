FindSorted <- function(permutations) {
  for (i in seq_along(permutations)) {
    if (permutations[i] != i - 1) {
      return(i)
    }
  }
}

IndicateAscending <- function(permutations) {
  length_perm <- length(permutations)
  
  indication <- rep(0, length_perm)
  
  indication[c(1, length_perm)] <- 1
  
  for (i in 1:(length_perm - 1)) {
    if (permutations[i + 1] == permutations[i] + 1) {
      indication[c(i, i + 1)] <- 1
    }
  }
  return(indication)
}

ReverseBlock <- function(permutations, start, end) {
  permutations[start:end] <- rev(permutations[start:end])
  return(permutations)
}

BreakpointSort <- function(permutations, reverse_block = FALSE) {
  FS <- FindSorted(permutations)
  IA <- IndicateAscending(permutations)
  
  pos <- 1
  smallest <- Inf
  
  for (i in seq_along(IA)) {
    if (IA[i] == 0 && permutations[i] < smallest) {
      smallest <- permutations[i]
      pos <- i
    }
  }
  
  if (smallest != Inf) {
    if (reverse_block) {
      permutations <- ReverseBlock(permutations, FS, pos)
    } else {
      permutations[c(FS, pos)] <- permutations[c(pos, FS)]
    }
    permutations <- BreakpointSort(permutations, reverse_block)
  }
  
  return(permutations)
}

# Example usage:
permutations <- c(0, 4, 5, 3, 2, 1, 6, 7, 8)

# Reverse the entire block between the first and last breakpoint
output_BS_reverse_block <- BreakpointSort(permutations, reverse_block = TRUE)
print(output_BS_reverse_block)

