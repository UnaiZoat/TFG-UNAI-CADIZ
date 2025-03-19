datosCadizResultados <- read.csv(file.choose(),header = TRUE, sep = ",")

attach(datosCadizResultados)
names(datosCadizResultados)

datosCadizResultados$Resultado <- factor(datosCadizResultados$Resultado, levels = c("V","E","D"))

library(ggplot2)

ggplot(datosCadizResultados, aes(x=Local.Visitante, fill =Resultado)) +
  geom_bar(position = "dodge") +
  labs(title = "Resultados en Casa vs Fuera", x= "Condicion", y= "Cantidad de Partidos")

ggplot(datosCadizResultados, aes(x=Posesión, fill=Resultado)) +
  geom_histogram(binwidth = 5, position= "dodge", color="black")+
  labs(title= "Posesión según Resultado", x="Posesión(%)", y= "Cantidad de Partidos")

ggplot(datosCadizResultados,aes(x=Posesión, y=GF, color=Resultado))+
  geom_point(size=3)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title= "Relación entre Posesión y Goles Marcados", x="Posesión(%)", y= "Goles")

ggplot(datosCadizResultados,aes(x=Posesión, y=GC, color=Resultado))+
  geom_point(size=3)+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title= "Relación entre Posesión y Goles en Contra", x="Posesión(%)", y= "Goles")

ggplot(datosCadizResultados, aes(x=xG, y=GF, color=Resultado))+
  geom_point(size=3)+
  geom_abline(slope = 1,intercept = 0,linetype="dashed",color="black")+
  labs(title = "Comparacion entre xG y Goles Marcados", x="xG (Goles Esperados", y = "Goles Marcados")

