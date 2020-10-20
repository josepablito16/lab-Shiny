require(plotly)
library(lubridate)
accidentes<-read.csv("./Data/HechoTransito.csv",header = TRUE,sep=",")
accidentesPorMes<-as.data.frame(table(accidentes$mes_ocu,accidentes$año_ocu))
accidentesPorMes<-accidentesPorMes[with(accidentesPorMes,order(accidentesPorMes$Var2)),]

data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color=NULL)

ll.smooth = loess(y~x, span=0.3,data.frame(x=as.integer(rownames(accidentesPorMes)),y=accidentesPorMes$Freq))

#p.glob = plot_ly(x=as.integer(rownames(accidentesPorMes)), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
p.glob = plot_ly(x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=accidentesPorMes$Freq, type="scatter", mode="markers", line=data.fmt, name="Data")
p.glob = add_lines(p.glob, x=seq(ymd("2016-1-1"), ymd("2018-12-1"), by = "months"), y=predict(ll.smooth), line=line.fmt, name="Span = 0.3")
p.glob = layout(p.glob, title = "Global smoothers")
print(p.glob)
