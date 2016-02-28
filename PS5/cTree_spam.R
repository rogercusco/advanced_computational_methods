
setwd("~/Desktop/advanced_computational_methods/PS5")
source("~/Desktop/advanced_computational_methods/PS5/cTree.R")

spamdb <- read.table("spambase.data", sep=",")


#####################################################################
# rpart's TREE
#####################################################################

library(caTools)
set.seed(3000)
spl = sample.split(spamdb, SplitRatio = 0.7)
spamdb_train = subset(spamdb, spl==TRUE)
spamdb_test = subset(spamdb, spl==FALSE)


library(rpart)

score_rpart = rep(NA, 5)
count = 0

for(j in c(1,2,3,4,5)){
    
      count = count + 1
      rpart_tree <- rpart(V58 ~ V1+V2+V3+V4, data = spamdb_test, 
                          control = rpart.control(minbucket=20, maxdepth= j ), 
                          method="class")
      
      predictions <- predict(rpart_tree, newdata = spamdb_test, type="class")
      
      score_rpart[count] = sum(diag(table(predictions, spamdb_test$V58)/nrow(spamdb_test)))

}

#####################################################################
# rpart's TREE
#####################################################################


score_roger = rep(NA, 5)
count = 0

for(j in c(1,2,3,4,5)){
  
  count = count + 1
  result = cTree(V58 ~ V1+V2+V3+V4, spamdb_test, j, 20, "ME")
  
  score_roger[count] = sum(diag(table(result$predLabels, spamdb_test$V58)/nrow(spamdb_test)))
  
}

# I could have a bug in fixing the depth. Not realistic to beat rpart...

pdf("cTree.pdf")
plot(x=1:5, y=score_rpart, type='l', col='red', xlab="depth", ylab="% accuracy")
lines(x=1:5, y=score_roger, type='l', col='blue')
title("in sample")
legend("bottomright", cex=0.7, legend= c("roger",
                                        "rpart"), lty = c(1,1), col=c("blue", "red"))

dev.off()





