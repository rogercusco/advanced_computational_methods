# set working directory to one containing the spam dataset 

setwd("~/Desktop/advanced_computational_methods/PS5")
source("~/Desktop/advanced_computational_methods/PS6/adaBoost.R")

# load require libraries

library(caTools)
library(gbm)

# read in the data

spamdb <- read.table("spambase.data", sep=",")

# test accuracy of gbm's package adaboost algorithm in-sample

data <- spamdb
set.seed(3000)
spl = sample.split(data, SplitRatio = 0.7)
spamdb_train = subset(data, spl==TRUE)
spamdb_test = subset(data, spl==FALSE)

sequence <- seq(1,20,2)
gbm_accuracy_insample <- rep(NA, length(sequence))
counter = 0

for(i in sequence) {
  
  counter = counter + 1
  noIterations <- i
  boost <-gbm(formula = V58 ~.,
              distribution = "adaboost",
              data = rbind(spamdb_train, spamdb_test),
              n.trees = noIterations,
              interaction.depth = 1,
              shrinkage = 1,
              bag.fraction = 1,
              train.fraction = 7/10)
  
  # boostAcc <- mean((sign(predict(boost, spamdb_train)) != (ifelse(spamdb_train[,ncol(spamdb_train)] == 1, 1, -1))))
  gbm_accuracy_insample[counter] <- sum(diag(table(sign(predict(boost, spamdb_train)), spamdb_train$V58)/nrow(spamdb_train))) 
}

# test accuracy of roger's adaboost algorithm in-sample

  data[, ncol(data)] <- ifelse(data[,ncol(data)] == 1, 1, -1)
  set.seed(3000)
  spl = sample.split(data, SplitRatio = 0.7)
  spamdb_train = subset(data, spl==TRUE)
  spamdb_test = subset(data, spl==FALSE)
  
  sequence <- seq(1,20,2)
  roger_accuracy_insample <- rep(NA, length(sequence))
  counter = 0
  for(i in sequence){
    counter = counter + 1
    nt = i
    result <- adaBoost(V58 ~., spamdb_train, depth = 30, noTrees = nt)
    roger_accuracy_insample[counter] <- sum(diag(table(unlist(result), spamdb_train$V58)/nrow(spamdb_train))) 
  }

# test accuracy of gbm's package adaboost algorithm out-of-sample

  data <- spamdb
  set.seed(3000)
  spl = sample.split(data, SplitRatio = 0.7)
  spamdb_train = subset(data, spl==TRUE)
  spamdb_test = subset(data, spl==FALSE)
  
  library(gbm)
  
  sequence <- seq(1,20,2)
  gbm_accuracy_osample <- rep(NA, length(sequence))
  counter = 0
  
  for(i in sequence) {
    
    counter = counter + 1
    noIterations <- i
    boost <-gbm(formula = V58 ~.,
                distribution = "adaboost",
                data = rbind(spamdb_train, spamdb_test),
                n.trees = noIterations,
                interaction.depth = 1,
                shrinkage = 1,
                bag.fraction = 1,
                train.fraction = 7/10)
    
    # boostAcc <- mean((sign(predict(boost, spamdb_train)) != (ifelse(spamdb_train[,ncol(spamdb_train)] == 1, 1, -1))))
    gbm_accuracy_osample[counter] <- sum(diag(table(sign(predict(boost, spamdb_test)), spamdb_test$V58)/nrow(spamdb_test))) 
  }
  
# test accuracy of roger's adaboost algorithm out-of-sample
  
  data[, ncol(data)] <- ifelse(data[,ncol(data)] == 1, 1, -1)
  set.seed(3000)
  spl = sample.split(data, SplitRatio = 0.7)
  spamdb_train = subset(data, spl==TRUE)
  spamdb_test = subset(data, spl==FALSE)
  
  sequence <- seq(1,20,2)
  roger_accuracy_osample <- rep(NA, length(sequence))
  counter = 0
  for(i in sequence){
    counter = counter + 1
    nt = i
    result <- adaBoost.test(V58 ~., spamdb_train, depth = 30, noTrees = nt, spamdb_test)
    roger_accuracy_osample[counter] <- sum(diag(table(unlist(result), spamdb_test$V58)/nrow(spamdb_test))) 
  }

####################################################################################  
# GENERATE PLOTS
####################################################################################  
  
  pdf("adaBoost.pdf")
  
  plot(sequence, 100*roger_accuracy_insample, type='l', 
       col='blue', ylim=c(70,100), xlab='iterations', ylab='accuracy')
  lines(sequence, 100*gbm_accuracy_insample, col='red')
  legend('bottomright', col=c('blue', 'red'), legend=c("roger", "gbm"), lty=2, bty='n')
  
  
  plot(sequence, 100*roger_accuracy_osample, type='l', 
       col='blue', ylim=c(70,100), xlab='iterations', ylab='accuracy')
  lines(sequence, 100*gbm_accuracy_osample, col='red')
  legend('bottomright', col=c('blue', 'red'), legend=c("roger", "gbm"), lty=2, bty='n')
  
  dev.off()
