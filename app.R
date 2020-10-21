## app.R ##
library(shinydashboard)

ui <- dashboardPage(
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
                  plotOutput("", height = 250)
                ),
                box(
                  plotlyOutput("plotAccidentes", height = 250)
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  library(plotly)
  library(lubridate)
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
    
    ll.smooth = loess(y~x, span=0.3,data.frame(x=as.integer(rownames(accidentesPorMes)),y=accidentesPorMes$Freq))
    
    #p.glob = plot_ly(x=as.integer(rownames(accidentesPorMes)), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
    p.glob = plot_ly(x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
    p.glob = add_lines(p.glob, x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=predict(ll.smooth), line=line.fmt, name="LOESS(0.3)")
    p.glob = layout(p.glob, title = "Accidentes de motos por mes")
    plot1<-p.glob
  })
}

shinyApp(ui, server)
