library(shiny)
library(dplyr)
library(plotly)
library(lubridate)
# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  #Gráfica 1
  #FallecidosLesionados<-read.csv("./Data/FallecidosLesionados.csv", encoding="UTF-8")
  
  #output$distPlot <- renderPlot({
  #  filtro <- filter(FallecidosLesionados,a.o_ocu == input$year & mes_ocu <= input$month)
  #  barplot(table(filtro[,"mes_ocu"]),
  #          main = "Cantidad de accidentes fatales  al año",
  #          xlab = "Mes", ylab = "Cantidad de accidentes",
  #          col = "darkred")
  #})
  
  #Gráfica 2
  accidentes<-read.csv("./Data/FallecidosLesionados.csv", encoding="UTF-8")

  output$distPlot <- renderPlot({
    accidentes <- filter(FallecidosLesionados,a.o_ocu >= 2016)
    
    accidentesPorMes<-as.data.frame(table(accidentes$mes_ocu,accidentes$a.o_ocu))
    accidentesPorMes<-accidentesPorMes[with(accidentesPorMes,order(accidentesPorMes$Var2)),]
    
    loessMod30 = loess(y~x, span=input$span,data.frame(x=as.integer(rownames(accidentesPorMes)),y=accidentesPorMes$Freq))
    smoothed30 <- predict(loessMod30)
    plot(x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=accidentesPorMes$Freq, type="l", main="Predicción accidentes fatales (2016-2019) ", xlab="Fecha", ylab="Cantidad de accidentes")
    lines(smoothed30, x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), col="red")
  })
  
  
})