datosTirosJugador <- read.csv(file.choose(),header = TRUE, sep = ",")

attach(datosTirosJugador)

names(datosTirosJugador)

library(ggplot2)
library(dplyr)

top_goleadores <- datosTirosJugador %>%
  arrange(desc(Goles)) %>%
  slice_head(n=7)

ggplot(top_goleadores, aes(x=Goles.Disparo, y=Goles.DisparoPuerta, color=Nombre, label=Nombre))+
  geom_point(size=3)+
  geom_smooth(method = "lm",se=FALSE,color="red")+
  geom_text(hjust=0.5,vjust=-1,size=3)+
  labs(title = "Goles por disparo vs Goles por disparo a puerta",
       x="Goles/Disparo", y="Goles/DisparoPuerta")+
  theme_minimal()

ggplot(top_goleadores, aes(x=Disparos.cada.90min, y=Goles, color=Nombre,label=Nombre))+
  geom_point(size=2)+
  geom_smooth(method="lm",se=FALSE,color="blue")+
  geom_text(hjust=0.5,vjust=-1,size=3)+
  labs(title="Relacion entre Disparos cada 90min y Goles",
       x="Disparos cada 90min", y="Goles")+
  theme_minimal()

ggplot(top_goleadores,aes(x=xG,y=Goles,color=Nombre,label=Nombre))+
  geom_point(size=3)+
  geom_abline(slope = 1,intercept = 0,linetype="dashed",color="red")+
  geom_text(hjust=0.5,vjust=1,size=3)+
  labs(title = "Comparacion de Goles vs xG",
       x="xG",y="Goles")+
  theme_minimal()

ggplot(top_goleadores,aes(x=Edad, y=Goles.DisparoPuerta,color=Nombre,label=Nombre))+
  geom_point(size=3)+
  geom_smooth(method="lm",se=FALSE,color="red")+
  geom_text(hjust=0.5,vjust=-1,size=3)+
  labs(title = "Efectividad por edad",
       x="Edad",y="Goles por disparo a puerta")+
  theme_minimal()







  
  