#### Iniciando el analicis de los datos ####

rm(list = ls())
setwd(dir = "C:/Users/DANIPZA/Desktop/Tareas/Tarea1ProgramacionR")
getwd()

##### ESPECIFICACIONES DE LA EVALUACION DE LA CLASE 5 ####

# Describir modelos AR(2) , graficarlos para valores diferentes 
# de los argumentos (ar = c(p1,p2))
# AR(2)
AR2 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.2)) ,
                 n = 100,sd = 0.1)
# Probar varias combinaciones de p1 y p2 , graficar las series de tiempo
# simuladas, y sus correspondientes funciones de autocorrelacion simple
# y funciones de autocorrelacion parcial 

# Repetir lo mismo para los procesos MA(2) 

#### ACTIVAR LIBRARY S NECESARIOS ####

library(ggplot2)
library(tseries)
library(forecast) 
help(arina.sim)

             #### Fijando una semilla ####
                    set.seed(666)

########################Simulando Valores Con N = 150 (grande) #############################

AR2.1 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.9)), sd = 0.1)
AR2.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.6)), sd = 0.1)
AR2.3 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.3)), sd = 0.1)
AR2.4 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.1)), sd = 0.1)
AR2.5 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.1)), sd = 0.1)
AR2.6 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.3)), sd = 0.1)
AR2.7 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.6)), sd = 0.1)
AR2.8 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.9)), sd = 0.1)
AR2.9 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.9)), sd = 0.1)
AR2.10 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.6)), sd = 0.1)
AR2.11 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.3)), sd = 0.1)
AR2.12 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, -0.1)), sd = 0.1)
AR2.13 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.1)), sd = 0.1)
AR2.14 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.3)), sd = 0.1)
AR2.15 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.6)), sd = 0.1)
AR2.16 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.9, 0.9)), sd = 0.1)

AR2.17 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.9)), sd = 0.1)
AR2.18 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.6)), sd = 0.1)
AR2.19 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.3)), sd = 0.1)
AR2.20 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.1)), sd = 0.1)
AR2.21 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.1)), sd = 0.1)
AR2.22 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.3)), sd = 0.1)
AR2.23 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.6)), sd = 0.1)
AR2.24 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.9)), sd = 0.1)
AR2.25 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.9)), sd = 0.1)
AR2.26 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.6)), sd = 0.1)
AR2.27 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.3)), sd = 0.1)
AR2.28 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, -0.1)), sd = 0.1)
AR2.29 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.1)), sd = 0.1)
AR2.30 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.3)), sd = 0.1)
AR2.31 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.6)), sd = 0.1)
AR2.32 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.6, 0.9)), sd = 0.1)

AR2.33 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.9)), sd = 0.1)
AR2.34 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.6)), sd = 0.1)
AR2.35 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.3)), sd = 0.1)
AR2.36 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.1)), sd = 0.1)
AR2.37 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.1)), sd = 0.1)
AR2.38 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.3)), sd = 0.1)
AR2.39 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.6)), sd = 0.1)
AR2.40 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.9)), sd = 0.1)
AR2.41 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.9)), sd = 0.1)
AR2.42 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.6)), sd = 0.1)
AR2.43 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.3)), sd = 0.1)
AR2.44 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, -0.1)), sd = 0.1)
AR2.45 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.1)), sd = 0.1)
AR2.46 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.3)), sd = 0.1)
AR2.47 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.6)), sd = 0.1)
AR2.48 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.3, 0.9)), sd = 0.1)

AR2.49 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.9)), sd = 0.1)
AR2.50 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.6)), sd = 0.1)
AR2.51 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.3)), sd = 0.1)
AR2.52 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.1)), sd = 0.1)
AR2.53 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.1)), sd = 0.1)
AR2.54 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.3)), sd = 0.1)
AR2.55 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.6)), sd = 0.1)
AR2.56 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.9)), sd = 0.1)
AR2.57 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.9)), sd = 0.1)
AR2.58 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.6)), sd = 0.1)
AR2.59 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.3)), sd = 0.1)
AR2.60 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, -0.1)), sd = 0.1)
AR2.61 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.1)), sd = 0.1)
AR2.61 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.3)), sd = 0.1)
AR2.63 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.6)), sd = 0.1)
AR2.64 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(-0.1, 0.9)), sd = 0.1)

AR2.1.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.9)), sd = 0.1)
AR2.2.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.6)), sd = 0.1)
AR2.3.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.3)), sd = 0.1)
AR2.4.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.1)), sd = 0.1)
AR2.5.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.1)), sd = 0.1)
AR2.6.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.3)), sd = 0.1)
AR2.7.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.6)), sd = 0.1)
AR2.8.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.9)), sd = 0.1)
AR2.9.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.9)), sd = 0.1)
AR2.10.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.6)), sd = 0.1)
AR2.11.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.3)), sd = 0.1)
AR2.12.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, -0.1)), sd = 0.1)
AR2.13.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.1)), sd = 0.1)
AR2.14.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.3)), sd = 0.1)
AR2.15.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.6)), sd = 0.1)
AR2.16.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.9, 0.9)), sd = 0.1)

AR2.17.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.9)), sd = 0.1)
AR2.18.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.6)), sd = 0.1)
AR2.19.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.3)), sd = 0.1)
AR2.20.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.1)), sd = 0.1)
AR2.21.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.1)), sd = 0.1)
AR2.22.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.3)), sd = 0.1)
AR2.23.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.6)), sd = 0.1)
AR2.24.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.9)), sd = 0.1)
AR2.25.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.9)), sd = 0.1)
AR2.26.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.6)), sd = 0.1)
AR2.27.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.3)), sd = 0.1)
AR2.28.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, -0.1)), sd = 0.1)
AR2.29.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.1)), sd = 0.1)
AR2.30.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.3)), sd = 0.1)
AR2.31.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.6)), sd = 0.1)
AR2.32.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.9)), sd = 0.1)

AR2.33.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.9)), sd = 0.1)
AR2.34.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.6)), sd = 0.1)
AR2.35.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.3)), sd = 0.1)
AR2.36.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.1)), sd = 0.1)
AR2.37.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.1)), sd = 0.1)
AR2.38.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.3)), sd = 0.1)
AR2.39.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.6)), sd = 0.1)
AR2.40.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.9)), sd = 0.1)
AR2.41.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.9)), sd = 0.1)
AR2.42.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.6)), sd = 0.1)
AR2.43.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.3)), sd = 0.1)
AR2.44.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, -0.1)), sd = 0.1)
AR2.45.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.1)), sd = 0.1)
AR2.46.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.3)), sd = 0.1)
AR2.47.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.6)), sd = 0.1)
AR2.48.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.3, 0.9)), sd = 0.1)

AR2.49.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.9)), sd = 0.1)
AR2.50.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.6)), sd = 0.1)
AR2.51.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.3)), sd = 0.1)
AR2.52.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.1)), sd = 0.1)
AR2.53.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.1)), sd = 0.1)
AR2.54.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.3)), sd = 0.1)
AR2.55.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.6)), sd = 0.1)
AR2.56.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.9)), sd = 0.1)
AR2.57.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.9)), sd = 0.1)
AR2.58.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.6)), sd = 0.1)
AR2.59.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.3)), sd = 0.1)
AR2.60.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, -0.1)), sd = 0.1)
AR2.61.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.1)), sd = 0.1)
AR2.61.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.3)), sd = 0.1)
AR2.63.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.6)), sd = 0.1)
AR2.64.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.9)), sd = 0.1)

# Observemos que todos los valores son estacionarios  como el AR2.64.2
AR2.64.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.9)), sd = 0.1)

########## CORRIGUIENDO LOS VALORES y PRIORIZANDO LOS VALORES ALTOS-MEDIOS-BAJOS ######################


AR2.1 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.8, -0.8)), sd = 0.1)
AR2.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.8, -0.5)), sd = 0.1)
AR2.3 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.8, -0.3)), sd = 0.1)
AR2.4 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.8, -0.1)), sd = 0.1)

AR2.52 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.1)), sd = 0.1)
AR2.54 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.3)), sd = 0.1)
AR2.56 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.2)), sd = 0.1)
AR2.58<- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.6, 0.234)), sd = 0.1)

AR2.61.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.1)), sd = 0.1)
AR2.61.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.3)), sd = 0.1)
AR2.63.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.6)), sd = 0.1)
AR2.64.2 <- arima.sim(n = 150, model = list(order(2, 0, 0), ar = c(0.1, 0.8)), sd = 0.1)

class(AR2.64.2)
# [1] "ts"
##################### GRAFICAR LA SERIE DE IEMPO SIMULADA  PEQUEÑA  ##############################

graphics.off()

png(filename = "primera_imagen_de_N=150_grande.png")
 
# Eliguiendo los mas adecuados 

par(mfrow = c(3,4))

ylm <- c(min(AR2.1, AR2.2, AR2.3, AR2.4,
             AR2.52, AR2.54, AR2.56, AR2.58,
             AR2.61.2, AR2.64.2, AR2.63.2, AR2.64.2),
         max(AR2.1, AR2.2, AR2.3, AR2.4,
             AR2.52, AR2.54, AR2.56, AR2.58,
             AR2.61.2, AR2.64.2, AR2.63.2, AR2.64.2))


plot.ts(AR2.1, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=0.8")
plot.ts(AR2.2, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=-0.5")
plot.ts(AR2.3, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=-0.3")
plot.ts(AR2.4, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=-0.1")

plot.ts(AR2.52, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.8")
plot.ts(AR2.54, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.6")
plot.ts(AR2.56, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.5")
plot.ts(AR2.58, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.1")

plot.ts(AR2.61.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=0.8")
plot.ts(AR2.64.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=0.5")
plot.ts(AR2.63.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=0.3")
plot.ts(AR2.64.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=-0.1")

graphics.off()

png(filename = "sEGUNDA_imagen_de_N=150_grande.png")

# Eliguiendo los mas adecuados 

par(mfrow = c(3,4))

ylm <- c(min(AR2.1, AR2.2, AR2.3, AR2.4,
             AR2.52, AR2.54, AR2.56, AR2.58,
             AR2.61.2, AR2.64.2, AR2.63.2, AR2.64.2),
         max(AR2.1, AR2.2, AR2.3, AR2.4,
             AR2.52, AR2.54, AR2.56, AR2.58,
             AR2.61.2, AR2.64.2, AR2.63.2, AR2.64.2))


plot.ts(AR2.1, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=0.8")
plot.ts(AR2.2, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=-0.5")
plot.ts(AR2.3, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=-0.3")
plot.ts(AR2.4, ylim = ylm, main = "phi[1] = 0.8 , phi[2]=-0.1")

plot.ts(AR2.52, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.8")
plot.ts(AR2.54, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.6")
plot.ts(AR2.56, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.5")
plot.ts(AR2.58, ylim = ylm, main = "phi[1] = 0.6 , phi[2]=0.1")

plot.ts(AR2.61.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=0.8")
plot.ts(AR2.64.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=0.5")
plot.ts(AR2.63.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=0.3")
plot.ts(AR2.64.2, ylim = ylm, main = "phi[1] = 0.1 , phi[2]=-0.1")

