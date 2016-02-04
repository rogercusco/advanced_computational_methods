MNIST_train = read.csv("MNIST_training.csv")
MNIST_test = read.csv("MNIST_test.csv")
 
  if (!require("class")) install.packages("class"); library("class")
  if (!require("caTools")) install.packages("caTools"); library("caTools")
 
   # split the training set to get out of sample acuracy of our trained model for different 
   # parameters 
       
    set.seed(3000)
    spl = sample.split(MNIST_train, SplitRatio = 0.6)
    Train = subset(MNIST_train, spl==TRUE)
    Test = subset(MNIST_train, spl==FALSE)
 
    train_labels = Train[, 1]; test_labels = Test[, 1]
    train_features = Train[, -1]; test_features = Test[, -1]
   
     # we see that the performance of the knn algorithm is best when k = 1
     
     # for (i in 1:7) {
       
     #  labels_hat = knn(train_features, test_features, train_labels, k = i)
     #  performance = sum(diag(table(labels_hat, test_labels)/length(test_labels)))
     #  print(paste(i, performance))
       
     #}
     
     
labels_hat = knn(MNIST_train[, -1], MNIST_test, MNIST_train[, 1], k = 1)
labels_hat = as.data.frame(labels_hat)
colnames(labels_hat) <- "predicted_label"
write.csv(labels_hat, file = "MNIST_predictions.csv", row.names= FALSE)