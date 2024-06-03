PartialDigestProblem <- function(deltaX){
  width <- max(deltaX)
  deltaX <- deltaX[deltaX != width]
  X <- c(0, width)
  Place(deltaX, X, width)
  }


Remove <- function(input_1, input_2) {
  for (elem in input_2) {
    input_1 <- input_1[-which(input_1 == elem)[1]]
  }
  return(input_1)
}


Place <- function(deltaX, X, width){
  if (length(deltaX) == 0){
    print(X)
    return()
  }

  y <- max(deltaX)
  delta_y_x <- abs(X-y)
  width_y <- abs(width-y)
  delta_width_y_x <- abs(width_y-X)
    
    
  if (all(delta_y_x %in% deltaX)){
    
    X <- c(X,y)
    deltaX <- Remove(deltaX, delta_y_x)
    Place(deltaX, X, width)
    X <- Remove(X, y)
    deltaX <- c(deltaX, delta_y_x)
  }
  
  if (all(delta_width_y_x %in% deltaX)){
    X <- c(X, width_y)
    deltaX <- Remove(deltaX, delta_width_y_x)
    Place(deltaX, X, width)
    X <- Remove(X, width_y)
    deltaX <- c(deltaX, delta_width_y_x)
  }
  return()
}
  

pdp <- PartialDigestProblem(c(2,2,3,3,4,5,6,7,8,10))
print(pdp)

