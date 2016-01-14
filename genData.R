############################################################
# DISCRIMINANT FUNCTIONS: DATA GENERATION
############################################################

# The following function generates binary data. 
# The idea for spiral data comes from: https://www.classes.cs.uchicago.edu/archive/2015/winter/12200-1/assignments/pa5/index.html

spiralData <- function(N = 2000, save.data = TRUE, save.plot = TRUE) { 
  
    x=c(); y=c(); counter = 0
    
    # Generate category 1
    for (theta in seq(0,5*pi, length.out = N/2)) {
      
      counter = counter +1
      r = ((theta)**2)
      x[counter] = rnorm(1, mean= r*cos(theta), sd=10)
      y[counter] = rnorm(1, mean=r*sin(theta), sd=15)
    }
    
    x2=c(); y2=c(); counter2 = 0
    
    # Generate category 2
    for (theta in seq(0,5*pi, length.out = N/2)) {
      
      counter2 = counter2 +1
      r = ((theta)**2)
      x2[counter2] = rnorm(1, mean= r*cos(theta)*0.65, sd=5)
      y2[counter2] = rnorm(1, mean=r*sin(theta)*0.65, sd=10)
    }
    
    # create dataframe
    category1 <- cbind(y,x) 
    category2 <- cbind(y2,x2)
    data <- as.data.frame(rbind(category1, category2))
    label <- c(rep("Category 1", N/2), rep("Category 2", N/2))
    target <- c(rep(0, N/2), rep(1, N/2))
    data <- data.frame(data, label, target)
    colnames(data) <- c("Y", "X", "Label", "Target")
    
    # create plot
    #ggplot(data = data, 
     #      aes(x = X, y = Y, colour=Label, fill=Label)) + 
    #  geom_point() +
    #  xlab("X") +
    #  ylab("Y") +
    #  theme_bw() +
    #  theme(text=element_text(family="Helvetica"))
    
    if (save.data == TRUE) {write.csv(data, file = "dataset.csv")}
    if (save.plot == TRUE) {
        pdf("dataPlot.pdf", width=10)
        print(ggplot(data = data, 
               aes(x = X, y = Y, colour=Label, fill=Label)) + 
          geom_point() +
          xlab("X") +
          ylab("Y") +
          theme_bw() +
          theme(text=element_text(family="Helvetica")))
       dev.off()
    }
}