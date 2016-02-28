##########################################################################
# ROGER'S CLASSIFICATION TREE :)
##########################################################################

# tested with iris dataset:
# result = cTree(Species ~ ., iris, 5, 5, "ME")
# table(result$predLabels, iris$Species)

cTree <- function( formula, data, depth, minPoints, costFnc = "Entropy") {
  
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  assert_that(class(formula) == "formula")
  assert_that(is.count(depth)); assert_that(is.count(minPoints))
  assert_that(costFnc %in% c("ME", "Gini", "Entropy"))

  df = model.frame(formula, data = data)
  # X = model.matrix(formula, data = data)[,-1]
  # dependent = model.response(df, type = "factor")
  dep = df[,1]
  X = cbind(df[,-1],dep)
  y = ncol(X)
  
    divideset <- function(X, col, value) {
      
      set1 = X[X[,col] > value, ]
      set2 = X[X[,col] <= value, ]
      return(list(set1=set1, 
                  set2=set2))
    }
    
    uniquecounts <- function(X, y) {
      
      probabilities = table(X[,y])/nrow(X)
      return(list(labels=names(probabilities),
                  probability=unname(probabilities)))
    } 
    
    
    cFnc <- function(X, y, Fnc) {
      
      p = uniquecounts(X,y)$probability
      if (Fnc == "ME") return(1 - max(p)) 
      if (Fnc == "Gini") return(sum(sapply(p, function(x) x*(1-x))))
      if (Fnc == "Entropy") return(-sum(sapply(p, function(x) x*log(x+0.0001))))
      
    }
    
    buildtree <- function(y, X, depth, minPoints, cf, store=c(), K = 0, l=2) {
    
      if ( nrow(X) <= minPoints || K >= depth) { return(c())
    
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
      
        if(best_gain ==0 ) return(c(col=NA, value=NA, set1=NA, 
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
    
    tree = buildtree(y, X, depth, minPoints, costFnc)
    
    classify <- function(y, X, tree) {
    
      final_node = rep(NA, nrow(X))
      # true_label = rep(NA, nrow(X))
      ot = tree[, order(tree[7, ], tree[5,])]
    
      for(i in 1:nrow(X)) {
        
        size = nrow(X); parent = -1; last_node = FALSE; obs = X[i,]
    
        for(j in 1:ncol(tree)) {
          
          if(is.na(tree[1,j]) & size == tree[6,j] & parent + 1 == tree[7,j]) {
            final_node[i] = j 
            break
          }
          if((last_node == TRUE & tree[5,j] == 1) | size != tree[6,j] | parent + 1 != tree[7,j] 
            # |is.na(tree[1,j]) 
             ) { next }
          
          if((last_node == FALSE & tree[5,j] == 0) | size != tree[6,j] | parent + 1 != tree[7,j] 
            # | is.na(tree[1,j]) 
            ) { next }
          
          col = tree[1,j]; value = tree[2,j]; tf = tree[5,j]; test = obs[col] > value
          
          if(test == TRUE) {last_node = TRUE; size = tree[3,j]; parent = tree[7,j]
    
          } else {last_node = FALSE; size = tree[4,j]; parent = tree[7,j]} 
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
      label_node[count] = names(sort(table(subset_node[,y]), decreasing=TRUE))[1]
    }

    code = as.data.frame(cbind(terminal_names, label_node, probability_node))
    ob <- as.data.frame(final_node)
    colnames(ob) <- "terminal_names"
    print(tree)
    print(head(ob,5))
    print(table(ob))
    print(code)
    final_label <- (merge(ob, code, by = "terminal_names", sort=FALSE))
    print(nrow(final_label))
 return(list(predLabels=final_label$label_node,
       prob=final_label$probability_node))

}






