data(iris)
str(iris)
y = iris[,5]
X = iris[, 1:4]
value = mean(iris$Sepal.Length)
col = 1

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


costFnc <- function(X, y, costFnc) {
  
  p = uniquecounts(X,y)$probability
  if (costFnc == "ME") return(1 - max(p)) 
  if (costFnc == "Gini") return(sum(sapply(p, function(x) x*(1-x))))
  if (costFnc == "Entropy") return(-sum(sapply(p, function(x) x*log(x))))
  
}

minPoints = 4
cf = "ME"
depth = 5
y = 5 # index of the predicted column
X = iris


buildtree <- function(formula, X, depth, minPoints, cf) {
  
  if (nrow(X) <= minPoints || is.null(nrow(X))) return(list(results=uniquecounts(X, y)))
  current_score = costFnc(X, y, cf)
  
  best_gain = 0.0
  best_criteria = NA
  best_sets = NA
  
  column_count = ncol(X) - 1
  
  for (col in 1:column_count) {
    for (row in 1:(nrow(X))) {
      
        value = (X[row,col] + X[row+1,col])/2
        set1 = divideset(X, col, value)$set1
        set2 = divideset(X, col, value)$set2
        
        # information gain
        p = nrow(set1)/nrow(X)
        gain = current_score - p*costFnc(set1, y, cf)
                - (1 - p)*costFnc(set2, y, cf)
        # print(paste(col,row))
          if (gain > best_gain & nrow(set1)>minPoints & nrow(set2)>minPoints ) {
            
            best_gain = gain
            best_criteria = c(col, value)
            best_sets = list(set1, set2)
          }
    }
  }
  
  if (best_gain > 0) {
    
    trueBranch=buildtree(y, best_sets[1], depth, minPoints, cf)
    falseBranch=buildtree(y, best_sets[2], depth, minPoints, cf)
    return(list(col=best_criteria[1],
                        value=best_criteria[2],
                        tb=trueBranch,
                        fb=falseBranch))
  } else {
   
    return(list(results=uniquecounts(X, y))) 
  }
  
}

def buildtree(rows,scoref=entropy): #rows is the set, either whole dataset or part of it in the recursive call, 
        #scoref is the method to measure heterogeneity. By default it's entropy.
      if len(rows)==0: return decisionnode() #len(rows) is the number of units in a set
      current_score=scoref(rows)
      
      # Set up some variables to track the best criteria
      best_gain=0.0
      best_criteria=None
      best_sets=None
      
      column_count=len(rows[0])-1   #count the # of attributes/columns. 
      #It's -1 because the last one is the target attribute and it does not count.
      for col in range(0,column_count):
        # Generate the list of all possible different values in the considered column
        global column_values        #Added for debugging
        column_values={}            
        for row in rows:
          column_values[row[col]]=1   
        # Now try dividing the rows up for each value in this column
        for value in column_values.keys(): #the 'values' here are the keys of the dictionnary
          (set1,set2)=divideset(rows,col,value) #define set1 and set2 as the 2 children set of a division
        
      # Information gain
          p=float(len(set1))/len(rows) #p is the size of a child set relative to its parent
          gain=current_score-p*scoref(set1)-(1-p)*scoref(set2) #cf. formula information gain
          if gain>best_gain and len(set1)>0 and len(set2)>0: #set must not be empty
            best_gain=gain
          best_criteria=(col,value)
          best_sets=(set1,set2)
      
      # Create the sub branches   
      if best_gain>0:
        trueBranch=buildtree(best_sets[0])
        falseBranch=buildtree(best_sets[1])
        return decisionnode(col=best_criteria[0],value=best_criteria[1],
                            tb=trueBranch,fb=falseBranch)
      else:
        return decisionnode(results=uniquecounts(rows))

