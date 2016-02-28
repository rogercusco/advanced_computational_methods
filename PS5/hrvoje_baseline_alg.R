findThreshold <- function(x, y) {
  
  noPoints <- length(x)
  errors <- rep(NA, noPoints-1)
  thresholds <- rep(NA, noPoints-1)
  splitLabels <- matrix(NA, ncol=2, nrow=noPoints-1)
  
  # we go sequentially over each point and cut between that point and the
  # closest neighbor
  for (idx in 1:(noPoints-1)) {
    
    # locate a potential threshold, a split between two points
    potThres <- mean(x[idx:(idx+1)])
    
    # check the classification error, when both sides, 
    # are classified with mean label
    predictedClasses <- rep(NA, noPoints)
    meanLeft <- mean(y[x < potThres])
    meanRight <- mean(y[x >= potThres])
    if (meanLeft < 0.5) {
      predictedClasses[x < potThres] <- 0
    } else {
      predictedClasses[x < potThres] <- 1
    }
    if (meanRight < 0.5) {
      predictedClasses[x > potThres] <- 0
    } else {
      predictedClasses[x > potThres] <- 1
    }
    # error of this split
    misError <- mean(predictedClasses != y)
    
    # recording the accuracy, thresholds and labels of 
    # the splitted interval
    errors[idx] <- misError
    thresholds[idx] <- potThres
    splitLabels[idx,] <- c(predictedClasses[x < potThres][1],
                           predictedClasses[x > potThres][1])
  }
  # print(cbind(errors, thresholds, splitLabels))
  
  # next we find the minimum and the best threshold
  minError <- min(errors)
  bestThreshold <- thresholds[which(errors==minError)]
  # if more than 1 threshold has the same accuracy we choose one randomly
  bestThreshold <- sample(bestThreshold, 1)
  
  # what are the final labels of the best split?
  labels <- splitLabels[which(thresholds==bestThreshold),]
  # print(cbind(minError, bestThreshold, labels))
  
  return(list(thres = bestThreshold, 
              err = minError, 
              labels = labels))
}


# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
# based on a global error

simpleClassTree <- function(K, X, Y) {
  
  # setting up the initial boundaries - whole interval, with each
  # iteration of k this will expand
  boundaries <- c(0, 1)
  
  # iterating for K times, i.e. we iteratively split input space in an 
  # empirically greedy way, keeping only the best split and adding it 
  # to the boundaries, we stop after doing this K times
  for (k in 1:K) {
    
    # first we subset our input space according to the boundaries 
    # found so far
    intervals <- cut(X, boundaries, include.lowest = TRUE)
    noIntervals <- length(levels(intervals))
    
    # then we go over each subset and see what is the best splitting
    # point locally in this subset, using the findThreshold function
    thresholds <- rep(NA, noIntervals)
    errors <- rep(NA, noIntervals)
    splitLabels <- matrix(NA, ncol=2, nrow=noIntervals)
    for (iter in 1:noIntervals) {
      x <- X[intervals==levels(intervals)[iter]]
      y <- Y[intervals==levels(intervals)[iter]]
      # we skip if there is a single element in the interval
      # nothing to split there
      if (length(y)>1) {
        # find the local splitting point
        results <- findThreshold(x, y)
        thresholds[iter] <- results$thres
        splitLabels[iter,] <- results$labels
        
        # add the potential threshold to our list of boundaries
        boundariesHH <- c(boundaries, abs(results$thres))
        boundariesHH <- sort(boundariesHH)
        
        # add the signs of the new threshold which indicates what 
        # is the label of the of newly splitted interval
        if (k==1) {
          signsHH <- results$labels
        } else {
          signsHH <- append(signs, results$labels[1], 
                            after=which(boundariesHH==results$thres)-2)
          signsHH[which(boundariesHH==results$thres)] <- 
            results$labels[2]
        }
        
        # now we compute predictions with new boundaries based on the 
        # potential split
        predictedClasses <- cut(X, boundariesHH)
        levels(predictedClasses) <- signsHH 
        
        # we compute a global, overall error rate for this local
        # modification, we do not use the local error to evaluate 
        # the splitting point
        errors[iter] <- mean(predictedClasses != Y)
      }
    }
    
    # find the best threshold in this iteration, greedy strategy
    minError <- min(errors, na.rm=TRUE)
    bestThreshold <- thresholds[which(errors==minError)]
    bestThreshold <- sample(bestThreshold, 1)
    labels <- splitLabels[which(thresholds==bestThreshold),]
    
    # add the new threshold to our list of boundaries
    boundaries <- c(boundaries, abs(bestThreshold))
    boundaries <- sort(boundaries)
    
    # add the signs of the new threshold which indicates what is the label of the newly splitted interval
    if (k==1) {
      signs <- labels
    } else {
      signs <- append(signs, labels[1], 
                      after=which(boundaries==bestThreshold)-2)
      signs[which(boundaries==bestThreshold)] <- labels[2]
    }
  }
  
  # get the final predicted classes
  predictedClasses <- cut(X, boundaries)
  levels(predictedClasses) <- signs 
  
  # now we evaluate the final accuracy, after K iterations
  misError <- mean(predictedClasses != Y)
  
  return(list(predictedClasses = predictedClasses, 
              misError = misError,
              boundaries = boundaries,
              signs = signs))
}

intervals_1D <- function(noObs, intervals, epsilon = 0.3, 
                         seed = round(runif(1)*100000)) {
  set.seed(seed)
  
  # create 1D input points, where each observation is drawn independently
  # from the uniform distribution, interval from 0 to 1
  X <- runif(noObs)
  X <- sort(X)
  
  # create intervals dynamically with the help of parsing expressions,
  # and verify which inputs fall into them
  condition <- character()
  for (i in 1:length(intervals)) {
    if (i==1) {
      express <- paste0("(X > ", intervals[[i]][1], " & X < ",
                        intervals[[i]][2],")" )
    } else {
      express <- paste0(" | (X > ", intervals[[i]][1], " & X < ",
                        intervals[[i]][2],")" )
    } 
    condition <- paste0(condition, express)
  }
  withinIntervals <- eval(parse(text = condition))
  
  
  # create classes
  # 1 if within the intervals with probability 0.5 + epsilon, 
  # 0 if outside of the intervals with probability 0.5 - epsilon
  Y <- rep(NA, noObs)
  Y[withinIntervals]  <- rbinom(sum(withinIntervals),  1, 0.5 + epsilon)
  Y[!withinIntervals] <- rbinom(sum(!withinIntervals), 1, 0.5 - epsilon)
  
  return(list(X=X, Y=Y))
}

