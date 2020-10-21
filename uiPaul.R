library(shiny)
library(shinyWidgets)
library(plotly)
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  #Grafica #1
  # Application title
  #headerPanel("Fallecidos por año/mes"),
  
  # Sidebar with a slider input for number of observations
  #sidebarPanel(
  #  setSliderColor(c("DeepPink ", "#FF4500"), c(1, 2, 4)),
  #  sliderInput("year", 
  #              "Años", 
   #             min = 2016,
  #              max = 2018, 
  #              value = 2017),
  #  sliderInput("month", 
  #              "Meses", 
  #              min = 1,
  #              max = 12, 
  #              value = 6)
  #),
  
  headerPanel("Fallecidos por año"),
  #Grafica #2
  sidebarPanel(
    setSliderColor(c("DeepPink ", "#FF4500"), c(1, 2, 4)),
    sliderInput("span", 
                "Suavizado", 
                min = 0.1,
                max = 0.5, 
                value = 0.3),
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
))