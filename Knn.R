my_knn <- function(x_pred, x, y, k){
  m <- length(x_pred)
  predict_knn <- rep(0, m)
  for (i in 1:m) {
    
    # Calculate the absolute distance between x_pred[i] and x
    dist <- abs(x_pred[i] - x)
    
    # Apply order() to dist, sort_index will contain 
    # the indices of elements in the dist vector, in 
    # ascending order. This means sort_index[1:k] will
    # return the indices of the k-nearest neighbors.
    sort_index <- order(dist)    
    
    # Apply mean() to the responses of the k-nearest neighbors
    predict_knn[i] <- mean(y[sort_index[1:k]])    
    
  }
  return(predict_knn)
}