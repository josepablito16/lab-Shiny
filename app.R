## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(lubridate)
library("ggpubr")
library(plyr)
library(caret)
library(plotly)

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
              
              fluidPage(
                
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
              )
              
              
      ),
      
      # Tab de datos
      tabItem(tabName = "datos",
              h2("Analisis de datos"),
              fluidRow(
                box(plotlyOutput("plotAccDep", height = 250), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plotAccidentesPorMes", height = 250), width = 6),
                box(plotlyOutput("plotFallLes", height = 250), width = 6)
              )
      ),
      
      # Tab de proyecciones
      tabItem(tabName = "proyecciones",
              h2("Proyecciones"),
              fluidRow(
                box(
                  plotlyOutput("plotImportaciones", height = 250, width = "100%"),
                  sidebarPanel(
                    setSliderColor(c("#F39C12","#F39C12","#F39C12","#F39C12","#F39C12","#F39C12"),c(1,2,3,4,5,6)),
                    sliderInput("span3", 
                                "Suavizado", 
                                min = 0.1,
                                max = 1, 
                                value = 0.3),
                  ), width = 12
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
  meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  departamentos <- c("Guatemala", "El Progreso", "Sacatepéquez", "Chimaltenango", "Escuintla", "Santa Rosa", "Sololá", "Totonicapán", "Quetzaltenango", "Suchitepéquez", "Retalhuleu", "San Marcos", "Huehuetenango", "Quiché", "Baja Verapaz", "Alta Verapaz", "Petén", "Izabal", "Zacapa", "Chiquimula", "Jalapa", "Jutiapa")
  accidentes<-read.csv("./Data/HechoTransito.csv",header = TRUE,sep=",")
  accidentes <- filter(accidentes,accidentes$tipo_veh == 4)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plotAccidentesPorMes <- renderPlotly({
    conteo <- plyr::count(accidentes[,c("mes_ocu")])
    conteo$meses <- meses
    
    xform <- list(categoryorder = "array",
                  categoryarray = conteo$meses)
    
    fig = plot_ly(
      data = conteo,
      x = ~meses,
      y = ~freq,
      name = "SF Zoo",
      type = "bar"
    ) %>% layout(title = "Sumatoria de accidentes de motos mensuales (2016-2018)", xaxis = xform)
    
    plot1 <- fig
  })
  
  output$plotAccDep <- renderPlotly({
    municipiosAccidentes <- table(accidentes[,"depto_ocu"])
    deptoOcuDesc <- municipiosAccidentes[order(municipiosAccidentes, decreasing = TRUE)]
    
    fig = plot_ly(
      x = departamentos,
      y = deptoOcuDesc,
      type = "bar"
    ) %>% layout(
      title = "Sumatoria de accidentes de motos por depto. (2016-2018)",
      xaxis = xform)
    
    plot1 <- fig
  })
  
  output$plotFallLes <- renderPlotly({
    fallecidos <- read.csv("./Data/FallecidosLesionados.csv",header = TRUE,sep=",")
    fallecidos <- filter(fallecidos,fallecidos$tipo_veh == 4)
    fallecidos <- filter(fallecidos, fallecidos$fall_les == 1)
    
    fallecidosPorAnio <- as.data.frame(table(fallecidos[,"año_ocu"]))
    colnames(fallecidosPorAnio) <- c("Año", "Total")
    
    fig = plot_ly(
      data = fallecidosPorAnio,
      x = ~Año,
      y = ~Total,
      type = "bar"
    ) %>% layout(
      title = "Sumatoria de fallecidos de motos por año (2016-2018)",
      xaxis = xform)
    
    plot1 <- fig
  })
  
  # Grafica de Jose
  output$plotAccidentes <- renderPlotly({
    accidentes<-read.csv("./Data/HechoTransito.csv",header = TRUE,sep=",")
    accidentes<-accidentes[accidentes$tipo_veh==4,]
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
    accidentes<-accidentes[accidentes$tipo_veh==4,]
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

  output$plotImportaciones <- renderPlotly({
    importaciones <- read.csv("./Data/importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)
    # Restringimos los datos a solamente motos
    motosImportaciones <- importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]
    # Hacemos la cuenta de motos importadas por dia
    importacionesPorDia <- plyr::count(motosImportaciones[,c("Dia", "Mes","Anio")])
    colnames(importacionesPorDia) = c("Dia", "Mes", "Anio", "TotalImportaciones")

    # Ordenamos las importaciones por fecha
    importacionesPorDia <- importacionesPorDia[order(
      importacionesPorDia$Anio,
      importacionesPorDia$Mes,
      importacionesPorDia$Dia),]
    
    row.names(importacionesPorDia) <-NULL
    importacionesOrdenadas <- importacionesPorDia[1215:2914,]
    fechas <- seq(as.Date("2015/1/1"), as.Date("2019/12/31"), "days")

    row <- 0
    for (row in 1:length(fechas)) {
      
      dia <- day(fechas[row])
      mes <- month(fechas[row])
      anio <- year(fechas[row])
      
      row_to_find <- data.frame(Dia=dia, Mes=mes, Anio=anio)
      if (nrow(merge(row_to_find,importacionesOrdenadas)) == 0 ) {
        df <- data.frame(dia, mes, anio, sample(500:2500, 1))
        names(df) <- c("Dia", "Mes", "Anio", "TotalImportaciones")
        importacionesOrdenadas <- rbind(importacionesOrdenadas, df)
      }
    }

    # Ordenamos las importaciones por fecha
    importacionesOrdenadas <- importacionesOrdenadas[order(
      importacionesOrdenadas$Anio,
      importacionesOrdenadas$Mes,
      importacionesOrdenadas$Dia),]

    row.names(importacionesOrdenadas) <-NULL

    # Graficamos
    data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
    line.fmt = list(dash="solid", width = 1.5, color=NULL)

    ll.smooth = loess(y~x, span=input$span3,data.frame(
      x= as.integer(rownames(importacionesOrdenadas)),
      y= importacionesOrdenadas$TotalImportaciones))

    p.glob = plot_ly(x = fechas, y = importacionesOrdenadas$TotalImportaciones, mode = 'markers', text = paste("Motos importadas"), type = "scatter", line=data.fmt, name="Data")
    p.glob = add_lines(p.glob, x = fechas, y=predict(ll.smooth), line=line.fmt, name="LOESS")
    p.glob = layout(p.glob, title="Importaciones de motos diarias desde el 2015", xaxis = list(title = "Fechas"),yaxis = list (title = "Cantidad de importaciones"))
    plot1 <- p.glob
  })
}

shinyApp(ui, server)
