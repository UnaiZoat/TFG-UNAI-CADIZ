datosGolesafavor <- read.csv("golesafavorCadizcfLimpio.csv",header= TRUE, sep= ",")

attach(datosGolesafavor)

names(datosGolesafavor)

library(ggplot2)
library(dplyr)

datosGolesafavor$Minute <- as.numeric(datosGolesafavor$Minute)

ggplot(datosGolesafavor, aes(x=Sedes, fill = Sedes))+
  geom_bar() +
  labs(title = "Goles en Casa vs Fuera", x="Sede", y="Goles")+
  theme_minimal()


ggplot(datosGolesafavor, aes(x=Parte.del.cuerpo, fill = Parte.del.cuerpo))+
  geom_bar() +
  labs(title = "Goles con cada parte del cuerpo", x="Parte del cuerpo", y="Goles")+
  theme_minimal()


ggplot(datosGolesafavor,aes(x=Minute))+
  geom_histogram(binwidth = 5, fill="blue", color="black")+
  labs(title = "Distribución goles por minuto", x="Minuto", y="Cantidad Goles")+
  theme_minimal()


ggplot(datosGolesafavor, aes(x=Distance))+
  geom_histogram(binwidth= 5,fill="blue", color="black") +
  labs(title = "Distancia de los Goles", x="Distancia(metros)",y="Cantidad Goles")+
  theme_minimal()

