## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(lubridate)

ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = "Laboratorio 9"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Analisis de datos", tabName = "datos", icon = icon("table")),
      menuItem("Proyecciones", tabName = "proyecciones", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab de inicio
      tabItem(tabName = "inicio",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Tab de datos
      tabItem(tabName = "datos",
              h2("Analisis de datos")
      ),
      
      # Tab de proyecciones
      tabItem(tabName = "proyecciones",
              h2("Proyecciones"),
              fluidRow(
                box(
                  plotOutput("", height = 250),
                )
              ),
              fluidRow(
                box(
                  plotlyOutput("distPlot", height = 250),
                  sidebarPanel(
                    setSliderColor("#F39C12",c(1,2,3,4,5,6)),
                    sliderInput("span", 
                                "Suavizado", 
                                min = 0.1,
                                max = 1, 
                                value = 0.3),
                  )
                ),
                box(
                  plotlyOutput("plotAccidentes", height =250),
                  sidebarPanel(
                    setSliderColor(c("#F39C12","#F39C12","#F39C12","#F39C12","#F39C12","#F39C12"),c(1,2,3,4,5,6)),
                    sliderInput("span2", 
                                "Suavizado", 
                                min = 0.1,
                                max = 1, 
                                value = 0.3),
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # Grafica de Jose
  output$plotAccidentes <- renderPlotly({
    accidentes<-read.csv("./Data/HechoTransito.csv",header = TRUE,sep=",")
    accidentesPorMes<-as.data.frame(table(accidentes$mes_ocu,accidentes$año_ocu))
    accidentesPorMes<-accidentesPorMes[with(accidentesPorMes,order(accidentesPorMes$Var2)),]
    
    data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
    line.fmt = list(dash="solid", width = 1.5, color=NULL)
    
    ll.smooth = loess(y~x, span=input$span2,data.frame(x=as.integer(rownames(accidentesPorMes)),y=accidentesPorMes$Freq))
    
    p.glob = plot_ly(x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
    p.glob = add_lines(p.glob, x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=predict(ll.smooth), line=line.fmt, name="LOESS")
    p.glob = layout(p.glob, title = "Accidentes de motos por mes",xaxis = list(title = "Fechas"),yaxis = list (title = "Cantidad de accidentes"))
    plot1<-p.glob
  })
  
  output$distPlot <- renderPlotly({
    accidentes<-read.csv("./Data/FallecidosLesionados.csv",header = TRUE,sep=",")
    accidentes <- filter(accidentes,accidentes$año_ocu >= 2016)
    
    accidentesPorMes<-as.data.frame(table(accidentes$mes_ocu,accidentes$año_ocu))
    accidentesPorMes<-accidentesPorMes[with(accidentesPorMes,order(accidentesPorMes$Var2)),]
    
    data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
    line.fmt = list(dash="solid", width = 1.5, color=NULL)
    
    ll.smooth = loess(y~x, span=input$span,data.frame(x=as.integer(rownames(accidentesPorMes)),y=accidentesPorMes$Freq))
    
    p.glob = plot_ly(x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
    p.glob = add_lines(p.glob, x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=predict(ll.smooth), line=line.fmt, name="LOESS")
    p.glob = layout(p.glob, title = "Predicción accidentes fatales (2016-2019)",xaxis = list(title = "Fechas"),yaxis = list (title = "Cantidad de accidentes"))
    plot1<-p.glob
  })
}

shinyApp(ui, server)
