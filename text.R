
library(shiny)
#runExample(example = "08_html")
library(shinyWidgets)
library(plotly)
# Define UI for application that plots random distributions 
ui <- shinyUI(fluidPage(

  headerPanel("Visualización Dinámica Importación y Accidentes de tráfico en Guatemala"),
  mainPanel(
    
    tags$div(
      tags$ul(
        tags$li("Paul Belches"),
        tags$li("José Cifuentes"),
        tags$li("Oscar Juárez")
      )
    ),
    p("Según el Instituto Nacional de Ciencias Forenses, la segunda mayor causa de muertes en Guatemala, son los accidentes 
      de tránsito. En 2018, en las calles y carreteras del país, ocurrieron diariamente alrededor de 17 accidentes de tránsito,
      que resultaron en la defunción de los implicados (1). Conforme la población del país aumente, lo mismo sucederá con la
      demanda de vehículos, lo cual incrementará la cantidad de estos circulando por las calles. Tomando esto en cuenta, es 
      razonable asumir, que la cantidad de accidentes crecerá proporcionalmente a la cantidad de personas conduciendo en el país.
      ",align = "justify"),
    p("A continuación, se presenta una serie de visualizaciones interactivas que buscan proveer soporte a aquellos que 
      estén interesados en conocer los fenómenos antes descritos. Se realizo un enfoque en el estudio de las motocicletas 
      en específico por su naturaleza, más vulnerable. Utilizando las importaciones anules y los accidentes de transito 
      como base. ",align = "justify"),
    p("(1) Ola, A. (12/01/2019) Accidentes de tránsito son la segunda causa de muerte en el país. Prensa Libre",font = "italic")
  )
))
server <- function(input, output) {
  library(plotly)
  library(lubridate)
}
shinyApp(ui, server)
