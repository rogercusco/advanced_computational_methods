kNN <- function(features, labels, k = 1, p = 1) {
  
      # test the inputs
      if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
      not_empty(features); not_empty(labels); 
      assert_that(nrow(features) == length(labels))
      is.count(k); assert_that(p %in% c(1, 2, Inf))
      
      noObs <- nrow(features)
      matrix(NA, noObs, noObs)
      features <- as.matrix(features)
      
      # create the distance matrix
      if (p == 1) distanceM = as.matrix(dist(features, method = 'euclidean'))
      if (p == 2) distanceM = as.matrix(dist(features, method = 'manhattan'))
      if (p==Inf) distanceM = as.matrix(dist(features, method = 'maximum'))
      
      # Sort the distances in increasing numerical order and pick the first 
      # k elements
      neighbors <- apply(distanceM, 1, order) 
      
      # Compute and return the most frequent class in the k nearest neighbors
      probabilities <- rep(NA, noObs)
      predLabels <- rep(NA, noObs)
      
      for (obs in 1:noObs) {
        probabilities[obs] <- sort(table(labels[neighbors[1:k, obs]])/noObs,decreasing=TRUE)[1]
        predLabels[obs] <- as.numeric(names(sort(table(labels[neighbors[1:k, obs]]),decreasing=TRUE)))[1]
      }
      
      
      return(list(predLabels = predLabels,
                  prob = probabilities))
  
}