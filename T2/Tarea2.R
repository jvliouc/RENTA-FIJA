#------------------------------------------------------------------------------#
#librerias tipicas:
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(xtable)
#------------------------------------------------------------------------------#
#PREGUNTA 1 simulacion
#a) Primero declaramos variables
a=0.1
b=0.05 
delta_t=0.5
sigma=0.015
r_0=0.039695

#simulaciones
set.seed(666)#semilla para que no me cambien los graficos a cada rato
caminos=5
tiempo=20/0.5
matriz<-matrix(0,nrow=tiempo+1,ncol=caminos)
for (i in 1:caminos) {
  #coore del 1 al 5
  for (j in 1:(tiempo+1)) {
    if (j==1) {
      matriz[j,i]=r_0
    }else{
      r_t=matriz[j-1,i]
      matriz[j,i]=max(0.0001,r_t+a*(b-r_t)*delta_t+sigma*sqrt(r_t*delta_t)*rnorm(1))
    }
  }
}
t<-seq(0,20,by=0.5)
matplot(t, matriz, type = "l", lty = 1, lwd = 2,
        col = rainbow(caminos),
        xlab = "Tiempo (años)", ylab = "Tasa",
        main = "Simulación de Caminos Estocásticos para la Tasa de Interés")
legend("topright", legend = paste("Camino", 1:caminos), col = rainbow(caminos), lty = 1, lwd = 2)
#------------------------------------------------------------------------------#
#b)1
df1<-data.frame(semestre=t)
df1<-df1 %>%
  mutate(
    tasa_s1=matriz[,1],
    saldo_capital=a,
    VP_pagos=a,
    duration=a,
    P_bonosc=a,
    
  )
   

