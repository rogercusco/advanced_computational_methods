loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved, 
                     sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  
  sigmaXY <- function(rho, sdX, sdY) {
    covTerm <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                       2, 2, byrow = TRUE)
    return(VCmatrix)
  }
  
  genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
  }
  
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

# generate data with new category "Undecided"
noApproved <- 50; noDenied <- 50; noUndecided <- 50
loanDf <- loanData(noApproved, noDenied, noUndecided, c(4, 150), c(10, 100), c(20,200),
                   c(1,20), c(2,30), c(2,10), -0.1, 0.6, 0.1, 1221)

# compute weights
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                     loanDf[,c("PIratio", "solvency")]))
Y <- cbind(target1 = c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided)),
           target2 = c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided)),
           target3 = c(rep(0, noApproved + noDenied), rep(1, noUndecided))
)

weights <- solve(t(X)%*%X) %*% t(X) %*% Y

# compute predictions
predictions <- X %*% weights
predictedLabel = c()

for ( i in 1:nrow(predictions)) {
  
  if (max(predictions[i,1], predictions[i,2], predictions[i,3]) == predictions[i,1]) {
    predictedLabel[i] = "Approved"
  }
  if (max(predictions[i,1], predictions[i,2], predictions[i,3]) == predictions[i,2]) {
    predictedLabel[i] = "Denied"
  }
  if (max(predictions[i,1], predictions[i,2], predictions[i,3]) == predictions[i,3]) {
    predictedLabel[i] = "Undecided"
  }
}

dataPred <- data.frame(loanDf, predictedLabel)
write.csv(dataPred, file = "predictions.csv")

# compute the boundary lines, including the intercept
x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), 
         length.out = nrow(loanDf))

intx = ((weights[1,1] - weights[1,2])/(weights[3,2] - weights[3,1]) -
          (weights[1,3] - weights[1,1])/(weights[3,1] - weights[3,3]))/
  ((weights[2,3] - weights[2,1])/(weights[3,1] - weights[3,3]) - 
     (weights[2,1] - weights[2,2])/(weights[3,2] - weights[3,1]))

inty = (weights[1,1] - weights[1,2])/(weights[3,2] - weights[3,1]) +
  intx*(weights[2,1] - weights[2,2])/(weights[3,2] - weights[3,1]) 

x <- c(x,intx)

y <- (weights[1,1] - weights[1,2])/(weights[3,2] - weights[3,1]) +
  x*(weights[2,1] - weights[2,2])/(weights[3,2] - weights[3,1])

y1 <- (weights[1,3] - weights[1,1])/(weights[3,1] - weights[3,3]) +
  x*(weights[2,3] - weights[2,1])/(weights[3,1] - weights[3,3])

y2 <- (weights[1,3] - weights[1,2])/(weights[3,2] - weights[3,3]) +
  x*(weights[2,3] - weights[2,2])/(weights[3,2] - weights[3,3])

X2 = cbind(rep(1, length(x)),x,y)
X3 = cbind(rep(1, length(x)),x,y1)
X4 = cbind(rep(1, length(x)),x,y2)

pred2 = X2 %*% weights
pred3 = X3 %*% weights
pred4 = X4 %*% weights

B2 = cbind(X2, pred2)
B3 = cbind(X3, pred3)
B4 = cbind(X4, pred4)

bo2 = data.frame()
bo3 = data.frame()
bo4 = data.frame()

for (i in 1:151) {
  if (((round(B2[i,4],11) == round(B2[i,5],11)) & (round(B2[i,5],11) >= round(B2[i,6],11)))) 
  {bo2 = rbind(bo2,B2[i,])}
}
for (i in 1:151) {
  if (((round(B3[i,4],11) == round(B3[i,6],11)) & (round(B3[i,6],11) >= round(B3[i,5],11)))) 
  {bo3 = rbind(bo3,B3[i,])}
}
for (i in 1:151) {
  if (((round(B4[i,5],11) == round(B4[i,6],11)) & (round(B4[i,6],11) >= round(B4[i,4],11)))) 
  {bo4 = rbind(bo4,B4[i,])}
}

colnames(bo2) = colnames(B2)
colnames(bo3) = colnames(B3)
colnames(bo4) = colnames(B4)

boundaryDf <- data.frame(PIratio=bo2[,2], solvency=bo2[,3], 
                         deny=rep("Boundary1", length(bo2[,2])))
boundaryDf1 <- data.frame(PIratio=bo3[,2], solvency=bo3[,3], 
                          deny=rep("Boundary2", length(bo3[,2])))
boundaryDf2 <- data.frame(PIratio=bo4[,2], solvency=bo4[,3], 
                          deny=rep("Boundary3", length(bo4[,2])))

# plot the data and the boundries
plotDiscFnc <- function() {
  pdf("discFunction3C.pdf", width = 10)
  print(
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
                         values = c("Boundary1" = "orange", 
                                    "Approved" = "blue", "Denied" = "red", "Undecided"
                                    = "green", "Boundary2" = "pink", "Boundary3" = "black"))
  )
  dev.off()
}
plotDiscFnc()