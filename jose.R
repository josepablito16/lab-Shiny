accidentes<-read.csv("./Data/HechoTransito.csv",header = TRUE,sep=",")
accidentesPorMes<-as.data.frame(table(accidentes$mes_ocu,accidentes$año_ocu))
accidentesPorMes<-accidentesPorMes[with(accidentesPorMes,order(accidentesPorMes$Var2)),]



require(plotly)
library(lubridate)

#p.glob = plot_ly(x=as.integer(rownames(accidentesPorMes)), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
p.glob = plot_ly(x=c(accidentesPorMes$Var2,accidentesPorMes$Var1), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
#p.glob = add_lines(p.glob, x=tt, y=predict(m1), line=line.fmt, name="Linear")
#p.glob = add_lines(p.glob, x=tt, y=predict(m2), line=line.fmt, name="Quadratic")
#p.glob = add_lines(p.glob, x=tt, y=predict(m3), line=line.fmt, name="Cubic")
#p.glob = layout(p.glob, title = "Global smoothers")
print(p.glob)
