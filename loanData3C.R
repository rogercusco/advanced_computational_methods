loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved, 
                     sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  
  
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target = c(rep(0, noApproved), rep(1, noDenied), rep(2, noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

noApproved <- 50; noDenied <- 50; noUndecided <- 50

loanDf <- loanData(noApproved, noDenied, noUndecided, c(4, 150), c(10, 100), c(20,200),
                   c(1,20), c(2,30), c(2,10), -0.1, 0.6, 0.1, 1221)

# illustrating the data, note that with ggplot we need to additionally 
# specify font family
ggplot(data = loanDf, 
       aes(x = solvency, y = PIratio, colour=deny, fill=deny)) + 
  geom_point() +
  xlab("solvency") +
  ylab("PIratio") +
  theme_bw() +
  theme(text=element_text(family="Arial"))

#loanDf <- cbind(loanDf, 
#               target1 = c(rep(0, noApproved), rep(1, noDenied)),
#              target2 = c(rep(1, noApproved), rep(0, noDenied)) 
#)


# analytical solution
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                     loanDf[,c("PIratio", "solvency")]))
Y <- cbind(target1 = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided)),
           target2 = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
           target3 = c(rep(0, noApproved + noDenied), rep(1, noUndecided))
)

# probably should be done in three steps??
weights <- solve(t(X)%*%X) %*% t(X) %*% Y

# compute predictions
predictions <- X %*% weights
predictedLabel = c()
#denied <- (predictions==apply(predictions, 1, max))[,1]
#predictedLabels <- ifelse(denied, "Denied", "Approved")

for ( i in 1:nrow(predictions)) {
  
    if (max(predictions[i,1], predictions[i,2], predictions[i,3]) == predictions[i,1]) {
      predictedLabel[i] = "Undecided"
    }
    if (max(predictions[i,1], predictions[i,2], predictions[i,3]) == predictions[i,2]) {
      predictedLabel[i] = "Approved"
    }
    if (max(predictions[i,1], predictions[i,2], predictions[i,3]) == predictions[i,3]) {
      predictedLabel[i] = "Denied"
  }
}

confMatrixFreq <- table(loanDf$deny, predictedLabel)
confMatrixFreq
confMatrixProp <- prop.table(confMatrixFreq, 1)
confMatrixProp

# when plotting, a more general solution is to use geom_line()
x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), 
         length.out = nrow(loanDf))

#y <- -(weights["PIratio"]/weights["solvency"])*x + 
#  (0.5-bias)/weights["solvency"]

y <- (weights[1,1] - weights[1,2])/(weights[3,2] - weights[3,1]) +
  x*(weights[2,1] - weights[2,2])/(weights[3,2] - weights[3,1])

y1 <- (weights[1,3] - weights[1,1])/(weights[3,1] - weights[3,3]) +
  x*(weights[2,3] - weights[2,1])/(weights[3,1] - weights[3,3])

y2 <- (weights[1,3] - weights[1,2])/(weights[3,2] - weights[3,3]) +
  x*(weights[2,3] - weights[2,2])/(weights[3,2] - weights[3,3])

# careful, colnames have to match!
boundaryDf <- data.frame(PIratio=x, solvency=y, 
                         deny=rep("Boundary", length(x)))
boundaryDf1 <- data.frame(PIratio=x, solvency=y1, 
                          deny=rep("Boundary2", length(x)))
boundaryDf2 <- data.frame(PIratio=x, solvency=y2, 
                          deny=rep("Boundary3", length(x)))

# now plotting again, but with geom_line(), and we create a plot function, 
plotDiscFnc <- function() {
  ggplot(data = loanDf, 
         aes(x = solvency, y = PIratio, colour=deny)) + 
    geom_point() +
    xlab("solvency") +
    ylab("PI ratio") +
    theme_bw() + 
    geom_line(data=boundaryDf) +
    geom_line(data=boundaryDf1) +
    geom_line(data=boundaryDf2) +
    xlim(20,250)+
    scale_color_manual("", 
                       values = c("Boundary" = "orange", 
                                  "Approved" = "blue", "Denied" = "red", "Undecided"
                                  = "green", "Boundary2" = "pink", "Boundary3" = "black"))
}
plotDiscFnc()