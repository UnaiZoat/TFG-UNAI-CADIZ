datosCadizTiros <- read.csv(file.choose(),header = TRUE, sep = ",")

attach(datosCadizTiros)

datosCadizTiros <- datosCadizTiros[-nrow(datosCadizTiros),]

names(datosCadizTiros)

library(ggplot2)
library(dplyr)

ggplot(datosCadizTiros, aes(x=Disparos, y=GF)) + 
  geom_point(color="blue",size= 3, alpha=0.7) +
  geom_smooth(method= "lm", color="red", se=FALSE) +
  labs(title="Relación entre Disparos y Goles", x="Disparos", y="Goles")+theme_minimal()

ggplot(datosCadizTiros, aes(x=xG, y=xG...nopenalty)) + 
  geom_point(color="blue",size= 3, alpha=0.7) +
  geom_abline(slope = 1, intercept = 0,linetype="dashed", color="red") +
  labs(title="Comparacion de xG con y sin penaltis", x="xG", y="xG sin penaltis")+theme_minimal()
