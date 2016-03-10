# roger's adaBoost training (and predicting in sample) function 

adaBoost <- function(formula, data, depth = 30, noTrees = 10) {
  
    if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
    assert_that(class(formula) == "formula")
    assert_that(is.count(depth)); assert_that(is.count(noTrees))
    

    df = model.frame(formula, data = data)
    dep = as.character(df[,1])
    X = cbind(df[,-1], dep)
    colnames(X) = colnames(data)
    y = ncol(X)
    X[,y] = as.factor(X[,y])
  
    if (!require("rpart")) install.packages("rpart"); library(rpart)

    N <- nrow(data)
  
    weights_obs <- rep(1/N, N)
    alpha <- rep(NA, noTrees)
    g_of_x <- list()
    final_predictions <- rep(NA, N)
    error <- rep(NA, noTrees)
    
    environment(formula) <- environment()
    
    for(i in 1:noTrees) {
      
      # X$weights_obs <- weights_obs
      fitted_model <- rpart(formula, data = X, weights = weights_obs,
                            control = rpart.control(maxdepth = depth),
                            method = "class")
      
      truth <- as.numeric(levels(X[,y]))[X[,y]]
      g_of_x[[i]] <- predict(fitted_model, newdata = X, type="class")
      predicted <-  as.numeric(levels(g_of_x[[i]]))[g_of_x[[i]]]
      error[i] <- sum( weights_obs[predicted != truth] )/sum(weights_obs)

      alpha[i] <- log((1 - error[i])/error[i])
      weights_obs <- weights_obs*exp(alpha[i]*(predicted != truth))

      
    }
    
    matrix_ag <- matrix(NA, nrow = N, ncol = noTrees)
    for(i in 1:noTrees) {
       matrix_ag[,i] <- alpha[i] * as.numeric(levels(g_of_x[[i]]))[g_of_x[[i]]]
       for(j in 1:N) {
       final_predictions[j] <- sign(sum(matrix_ag[j,]))
       }
    }
    
    return(list(predLabels=final_predictions))
}

    

# adaboost function to predict out of sample (for the spam part of PS6)

adaBoost.test <- function(formula, data, depth = 30, noTrees = 10, test) {
  
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  assert_that(class(formula) == "formula")
  assert_that(is.count(depth)); assert_that(is.count(noTrees))
  
  
  df = model.frame(formula, data = data)
  dep = as.character(df[,1])
  X = cbind(df[,-1], dep)
  colnames(X) = colnames(data)
  y = ncol(X)
  X[,y] = as.factor(X[,y])
  
  library(rpart)
  
  N <- nrow(data)
  
  weights_obs <- rep(1/N, N)
  alpha <- rep(NA, noTrees)
  g_of_x <- list()
  final_predictions <- rep(NA, nrow(test))
  error <- rep(NA, noTrees)
  fitted_model <- list()
  
  environment(formula) <- environment()
  
  for(i in 1:noTrees) {
    
    #X$weights_obs <- weights_obs
    fitted_model[[i]] <- rpart(formula, data = X, weights = weights_obs,
                          control = rpart.control(maxdepth = depth),
                          method = "class")
    
    truth <- as.numeric(levels(X[,y]))[X[,y]]
    g_of_x[[i]] <- predict(fitted_model[[i]], newdata = X, type="class")
    predicted <-  as.numeric(levels(g_of_x[[i]]))[g_of_x[[i]]]
    error[i] <- sum( weights_obs[predicted != truth] )/sum(weights_obs)
    
    alpha[i] <- log((1 - error[i])/error[i])
    weights_obs <- weights_obs*exp(alpha[i]*(predicted != truth))
  }
  
  # predict
  
  for(i in 1:noTrees) {
  
    g_of_x[[i]] <- predict(fitted_model[[i]], newdata = test, type="class")
    
  }
  
  matrix_ag <- matrix(NA, nrow = nrow(test), ncol = noTrees)
  for(i in 1:noTrees) {
    matrix_ag[,i] <- alpha[i] * as.numeric(levels(g_of_x[[i]]))[g_of_x[[i]]]
    for(j in 1:nrow(test)) {
      final_predictions[j] <- sign(sum(matrix_ag[j,]))
    }
  }
  
  return(list(predLabels=final_predictions))
}
