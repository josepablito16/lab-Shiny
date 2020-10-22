library("ggpubr")
library("corrplot")
library(plyr)
library(caret)
library(plotly)

importaciones <- read.csv("./Data/importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)

# Restringimos los datos a solamente motos
motosImportaciones <- importaciones[importaciones$Tipo.de.Vehiculo == "MOTO",]

# Hacemos la cuenta de motos importadas por día
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
ll.smooth = loess(y~x, span=0.3,data.frame(
  x= as.integer(rownames(importacionesOrdenadas)),
  y= importacionesOrdenadas$TotalImportaciones))

fig = plot_ly(x = ~fechas, y = importacionesOrdenadas$TotalImportaciones, mode = 'markers', text = paste("Motos importadas"), type = "scatter", line=data.fmt)
fig = add_lines(fig, x = fechas, y=predict(ll.smooth), line=line.fmt, name="LOESS(0.3)")
fig = layout(fig, title="Importaciones de motos diarias desde el 2015")

