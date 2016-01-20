library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Discriminant functions: Visualizing decision boundaries"),
  h5("By Roger Cuscó"),
  br(),
  p("Customize your inputs for the two categories: Approved and Denied"),
  br(),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

              h4("Approved", style='color:blue'),
              sliderInput("mu1a",
                          "Average PI ratio:",
                          min = 1,
                          max = 20,
                          value = 4),
              sliderInput("mu2a",
                          "Average Solvency:",
                          min = 1,
                          max = 200,
                          value = 150),
              sliderInput("sd1a",
                          "Standard deviation PI ratio:",
                          min = 1,
                          max = 5,
                          value = 1),
              sliderInput("sd2a",
                          "Standard deviation Solvency:",
                          min = 1,
                          max = 50,
                          value = 20),
              br(),
              h4("Denied", style='color:red'),
              sliderInput("mu1d",
                          "Average PI ratio:",
                          min = 1,
                          max = 20,
                          value = 10),
              sliderInput("mu2d",
                          "Average Solvency:",
                          min = 1,
                          max = 200,
                          value = 100),
              sliderInput("sd1d",
                          "Standard deviation PI ratio:",
                          min = 1,
                          max = 5,
                          value = 2),
              sliderInput("sd2d",
                          "Standard deviation Solvency:",
                          min = 1,
                          max = 50,
                          value = 30)
            ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Scatterplot of the data and decision boundry"),
      plotOutput("distPlot"),
      br(),
      h4("Confusion Matrix"),
      tableOutput("confusion")
    )
  )
))