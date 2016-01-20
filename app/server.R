library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
          # create small wrapper functions
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

      # creating a function for all of this
      loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                           sdDenied, rhoApproved, rhoDenied, seed=1111) {
        sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
        sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
        approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
        denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
        loanDf <- as.data.frame(rbind(approved,denied))
        deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
        target = c(rep(0, noApproved), rep(1, noDenied))
        loanDf <- data.frame(loanDf, deny, target)
        colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
        return(loanDf)
      }

  output$distPlot <- renderPlot({

    if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)


      # generating some data
      loanDf <- loanData(noApproved=50, noDenied=50, c(input$mu1a, input$mu2a), c(input$mu1d, input$mu2d), 
                         c(input$sd1a,input$sd2a), c(input$sd1d,input$sd2d), -0.1, 0.6, 1221)
    

        datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
        summary(datafit)

        # grabbing the coefficients
        weights <- coef(datafit)[c("solvency", "PIratio")]
        bias <- coef(datafit)[1]

        x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), 
                 length.out = nrow(loanDf))
        y <- -(weights["PIratio"]/weights["solvency"])*x + 
            (0.5-bias)/weights["solvency"]

        # careful, colnames have to match!
        boundaryDf <- data.frame(PIratio=x, solvency=y, 
                                 deny=rep("Boundary", length(x)))

    plotDiscFnc <- function() {
        ggplot(data = loanDf, 
               aes(x = solvency, y = PIratio, colour=deny)) + 
          geom_point() +
          xlab("solvency") +
          ylab("PI ratio") +
          theme_bw() + 
          geom_line(data=boundaryDf) + 
          scale_color_manual("", 
                             values = c("Boundary" = "grey", 
                                        "Approved" = "blue", "Denied" = "red"))
      }
    plotDiscFnc()



  })
  
  output$confusion <- renderTable({

          # generating some data
      loanDf <- loanData(noApproved=50, noDenied=50, c(input$mu1a, input$mu2a), c(input$mu1d, input$mu2d), 
                         c(input$sd1a,input$sd2a), c(input$sd1d,input$sd2d), -0.1, 0.6, 1221)
    

        noApproved <- 50; noDenied <- 50
        loanDf <- cbind(loanDf, 
                   target1 = c(rep(0, noApproved), rep(1, noDenied)),
                   target2 = c(rep(1, noApproved), rep(0, noDenied)) 
                   )

        # analytical solution
        X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                       loanDf[,c("PIratio", "solvency")]))
        Y <- cbind(target1 = c(rep(0, noApproved), rep(1, noDenied)),
                   target2 = c(rep(1, noApproved), rep(0, noDenied)) 
                   )
        weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y

        # compute predictions
        predictions <- X %*% weightsOptim

        # classify according to the argmax criterion
        denied <- (predictions==apply(predictions, 1, max))[,1]
        predictedLabels <- ifelse(denied, "Denied", "Approved")

        # classification algorithm performance
        confMatrixFreq <- table(loanDf$deny, predictedLabels)
        confMatrixProp <- prop.table(confMatrixFreq, 1)
        confMatrixProp
    })

})