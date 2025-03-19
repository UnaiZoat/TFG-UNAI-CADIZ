datosTirosEnContra <- read.csv(file.choose(),header = TRUE, sep = ",")

attach(datosTirosEnContra)

datosTirosEnContra <- datosTirosEnContra[-nrow(datosTirosEnContra),]

names(datosTirosEnContra)

library(ggplot2)

ggplot(datosCadizTiros, aes(x=Disparos, y=GF)) + 
  geom_point(color="blue",size= 3, alpha=0.7) +
  geom_smooth(method= "lm", color="red", se=FALSE) +
  labs(title="Relación entre Disparos Recibidos y Goles", x="Disparos Recibidos", y="Goles en Contra")+theme_minimal()
