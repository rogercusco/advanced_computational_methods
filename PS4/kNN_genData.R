# Creating the predictions.csv file and plot.pdf using Denitsa's spiral data
source("kNN.R")

genSpirals <- function(N = 2000,
                       degrees = 570,
                       location = 90,
                       blend = 0.2,
                       saveData = TRUE, 
                       savePlot = TRUE) {
  
  # Generate two-spiral data 
  # idea of the problematic dataset: http://www.benmargolis.com/compsci/ai/two_spirals_problem.htm
  # N - number of observations
  # degrees - length of the spiral 
  # location - how far away from the origin
  # blend<-blending together
  
  #necessary packages
  # if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  
  # define some variables
  degrees2rad <- (2*pi)/360 #convert degrees to radiant
  location <- location*degrees2rad #how far away from 00 the spiral starts
  
  N1 <- floor(N/2)
  N2 <- N-N1
  
  #spiral 1 
  #we indicate it by 0 in V3
  n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d1 <- t(rbind(-1*n*cos(n)+runif(N1)*blend, sin(n)*n+runif(N1)*blend, rep(0,N1)))
  
  #the second spiral we indicate by 1 in V3
  n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d2 <-t(rbind(n*cos(n)+runif(N1)*blend, -1*sin(n)*n+runif(N1)*blend, rep(1,N1))) 
  
  #combine the data 
  data <- data.frame(rbind(d1, d2))
  names(data) <- c("x1", "x2", "y")
  
  return(data)
}

# Generating predictions.csv file

dataset_spiral <- genSpirals(2000, saveData=FALSE, savePlot= FALSE)
spiral_features <- dataset_spiral[ ,1:2]
spiral_labels <- dataset_spiral[ ,3]

spiral_knn = kNN(spiral_features, spiral_labels, k = 5)
predictions = as.data.frame(cbind(dataset_spiral, spiral_knn$prob, spiral_knn$predLabels))
colnames(predictions) = c("Feature_1", "Feature_2", "True_Label", "Probability", "Predicted_Label")
write.csv(predictions, file = "predictions.csv", row.names = FALSE)


# Plotting boundaries

if (!require("nnet")) install.packages("nnet"); library("nnet")

p <- spiral_features
xp <- seq(min(spiral_features$x1), max(spiral_features$x1), length = 50); np <- length(xp)
yp <- seq(min(spiral_features$x2), max(spiral_features$x2), length = 50)
tp <- spiral_labels


pt <- expand.grid(x1 = xp, x2 = yp)

distanceMspiral = as.matrix(dist(rbind(spiral_features, pt), method = 'euclidean'))
distanceMspiral = distanceMspiral[(nrow(spiral_features) +1):nrow(distanceMspiral), 
                                  1:nrow(spiral_features)]
spiral_neighbors <- apply(distanceMspiral, 1, order)

k = 1
Z <- rep(NA, nrow(distanceMspiral))

for (obs in 1:nrow(distanceMspiral)) {
  Z[obs] <- as.numeric(names(sort(table(spiral_labels[spiral_neighbors[1:k, obs]]),decreasing=TRUE)))[1]
}

zp<-class.ind(Z)[,1] - class.ind(Z)[,2]

pdf("dataPlot.pdf")
plot(spiral_features$x1, spiral_features$x2, xlab = "x1", ylab = "x2", col=as.numeric(tp)+2)
contour(xp, yp, matrix(zp, np), add = T, levels = 0, labex = 0)
dev.off()