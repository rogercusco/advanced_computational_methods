
setwd("~/Desktop/advanced_computational_methods/PS5")
source("~/Desktop/advanced_computational_methods/PS5/cTree.R")

spamdb <- read.table("spambase.data", sep=",")


#####################################################################
# rpart's TREE in sample performance 
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
# Roger's TREE in sample preformance 
#####################################################################


score_roger = rep(NA, 5)
count = 0

for(j in c(1,2,3,4,5)){
  
  count = count + 1
  result = cTree(V58 ~ V1+V2+V3+V4, spamdb_test, j, 20, "ME")
  
  score_roger[count] = sum(diag(table(result$predLabels, spamdb_test$V58)/nrow(spamdb_test)))
  
}

# I could have a bug in fixing the depth. Not realistic to beat rpart...

##########################################################################
# some code I didn't have time to clean for the out of sample performance
##########################################################################

oscore_roger = rep(NA, 5)
count = 0

for(k in 1:5) {
count = count + 1
  
buildtree <- function(y, X, depth, minPoints, cf, store=c(), K = 0, l=2) {
  
  if ( nrow(X) <= minPoints || K >= depth) { return(c(col=NA, value=NA, set1=NA, 
                                                      set2=NA, tf=l, set = nrow(X), 
                                                      parent_K = K))
    
  } else {
    
    current_score = cFnc(X, y, cf)
    
    best_gain <- 0.0
    best_criteria <- NA
    best_sets <- NA
    
    column_count <- ncol(X) - 1
    
    for (col in 1:column_count) {
      
      sorted = X[order(X[, col]), ]
      
      for (row in 1:(nrow(X)-1)) {
        
        value <- mean(sorted[row,col], sorted[row+1,col])
        set1 <- divideset(sorted, col, value)$set1
        set2 <- divideset(sorted, col, value)$set2
        
        if(nrow(set1) <= minPoints | nrow(set2) <= minPoints) next
        
        # information gain
        p <- nrow(set1)/nrow(sorted)
        gain <- current_score - p*cFnc(set1, y, cf) - (1 - p)*cFnc(set2, y, cf)
        
        if (gain > best_gain) {
          
          best_gain <- gain
          best_criteria <- c(col, value)
          best_sets <- list(set1, set2)
          
        }
      }
    }
    
    if(best_gain <= 0 ) return(c(col=NA, value=NA, set1=NA, 
                                 set2=NA, tf=l, set = nrow(X), 
                                 parent_K = K))
    #if(is.null(best_sets[[1]]) | is.null(best_sets[[1]]) ) return(c())
    
    result <- c(col=best_criteria[1],
                value=best_criteria[2],
                #best_gain=best_gain,
                set1 = nrow(best_sets[[1]]),
                set2 = nrow(best_sets[[2]]),
                tf = l,
                set = nrow(X),
                parent_K = K)
    
    K = K + 1
    
    return(cbind(result, 
                 buildtree(y, best_sets[[1]], depth, minPoints, cf, result, K, 0),
                 buildtree(y, best_sets[[2]], depth, minPoints, cf, result, K, 1)))
  } 
}

df = model.frame(V58 ~ V1 + V2 + V3 + V4, data = spamdb_test)
dep = as.character(df[,1])
X = cbind(df[,-1],dep)
y = ncol(X)

tree = buildtree(y, X, k, 20, "ME")

df = model.frame(V58 ~ V1 + V2 + V3 + V4, data = spamdb_train)
dep = as.character(df[,1])
X = cbind(df[,-1],dep)
y = ncol(X)

classify <- function(y, X, tree) {
  
  final_node = rep(NA, nrow(X))
  # true_label = rep(NA, nrow(X))
  ot = tree[, order(tree[7, ], tree[5,])]
  
  for(i in 1:nrow(X)) {
    
    size = nrow(spamdb_test); parent = -1; last_node = FALSE; obs = X[i,]
    
    for(j in 1:ncol(tree)) {
      
      if(is.na(tree[1,j]) & size == tree[6,j] & parent + 1 == tree[7,j]) {
        final_node[i] = j 
        break
      }
      if((last_node == TRUE & tree[5,j] == 1) | 
         size != tree[6,j] | parent + 1 != tree[7,j] 
         
      ) { next }
      
      if((last_node == FALSE & tree[5,j] == 0) | 
         size != tree[6,j] | parent + 1 != tree[7,j] 
         
      ) { next }
      
      col = tree[1,j]; value = tree[2,j]; tf = tree[5,j]; 
      test = obs[col] > value
      
      if(test == TRUE) {last_node = TRUE; size = tree[3,j]; 
      parent = tree[7,j]
      
      } else { last_node = FALSE; size = tree[4,j]; parent = tree[7,j]} 
    }
  }
  return(final_node)
}

final_node <- classify(y, X, tree)
terminal_names <- names(table(final_node))
probability_node <- rep(NA, length(terminal_names))
label_node <- rep(NA, length(terminal_names))
count = 0

for(i in as.numeric(terminal_names)) {
  
  count = count + 1
  subset_node = X[which(final_node == i),]
  probability_node[count] = max(uniquecounts(subset_node,y)$probability)
  label_node[count] = names(sort(table(subset_node[,y])/nrow(subset_node), decreasing=TRUE))[1]
  # label_node[count] = (uniquecounts(subset_node,y)$labels)[1]
  #print(sort(table(subset_node[,y])/nrow(subset_node), decreasing=TRUE))
  
}

code = as.data.frame(cbind(terminal_names, label_node, probability_node))
ob <- as.data.frame(final_node)
colnames(ob) <- "terminal_names"
final_label <- (merge(ob, code, by = "terminal_names", sort=FALSE))

code = as.data.frame(cbind(terminal_names, label_node, probability_node))
ob <- cbind(as.data.frame(final_node), 1:length(final_node))
colnames(ob) <- c("terminal_names","or")
final_label <- (merge(ob, code, by = "terminal_names", sort=FALSE))
final_label <- final_label[order(final_label[,2]),]

oscore_roger[count] <- sum(diag(table(spamdb_train$V58, final_label$label_node)/nrow(spamdb_train)))

}
#####################################################################

oscore_rpart = rep(NA, 5)
count = 0

for(j in c(1,2,3,4,5)){
  
  count = count + 1
  rpart_tree <- rpart(V58 ~ V1+V2+V3+V4, data = spamdb_test, 
                      control = rpart.control(minbucket=20, maxdepth= j ), 
                      method="class")
  
  predictions <- predict(rpart_tree, newdata = spamdb_train, type="class")
  
  oscore_rpart[count] = sum(diag(table(predictions, spamdb_train$V58)/nrow(spamdb_train)))
  
}

os <- na.omit(oscore_roger)


#######################################################################
# Plots
#######################################################################


pdf("cTree.pdf")
plot(x=1:5, y=score_rpart, type='l', col='red', xlab="depth", ylab="% accuracy")
lines(x=1:5, y=score_roger, type='l', col='blue')
title("in sample")
legend("bottomright", cex=0.7, legend= c("roger",
                                         "rpart"), lty = c(1,1), col=c("blue", "red"))

plot(x=1:5, y=oscore_rpart, type='l', col='red', xlab="depth", ylab="% accuracy")
lines(x=1:5, y=os, type='l', col='blue') # see results in oscore_roger
title("out of sample")
legend("bottomright", cex=0.7, legend= c("roger","rpart"), lty = c(1,1), col=c("blue", "red"))
                                         

dev.off()

