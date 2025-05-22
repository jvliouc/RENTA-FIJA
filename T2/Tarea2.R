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
#en millones
monto=100
tasacupon=0.04
f=2 #frecuencia
cupon=monto*tasacupon/f
pagos=monto*f*(exp(r_0/f)-1)/(1-exp(-r_0*20))
df1<-data.frame(semestre=t)
#Lista: tasasimulada[CHEK], saldo capital, VPflujosfuturopension [CHEK],duracionpensión[chek],
#precio_bono_c[CHEKC], Duración_bono_cupon[CHEK], prop_bonos,
precio_bono_expr <- function(tas, semestre) {
  tiempos <- seq(semestre + 0.5, 25, by = 0.5)
  cupones <- cupon * sum(exp(-tas * (25 - tiempos)))
  nominal <- 100 * exp(-tas * (25 - semestre))
  return(cupones + nominal)
}
df1 <- df1 %>%
  mutate(
    tasa_s1 = matriz[,1],
    VP_flujos_pension = pagos * (1 - exp(-tasa_s1 * (20 - semestre))) / (f * (exp(tasa_s1 / f) - 1)),
    Duracion_pension = (1 / f) * (
      (exp(tasa_s1 / f) / (exp(tasa_s1 / f) - 1)) -
        ((exp(-tasa_s1 * (20 - semestre)) * ((20 - semestre) * f)) / (1 - exp(-tasa_s1 * (20 - semestre))))),
    Precio_bono = mapply(precio_bono_expr, tasa_s1, semestre)
  )
#tengo que hacer una funcion igual para la duracion
duracion_bono <- function(tasa, semestre, cupon = 2, FV = 100, vencimiento = 25) {
  tiempos <- seq(semestre + 0.5, vencimiento, by = 0.5)
  flujos <- rep(cupon, length(tiempos))
  flujos[length(flujos)] <- flujos[length(flujos)] + FV  # último flujo incluye el nominal
  
  # Valor presente de cada flujo
  vp_flujos <- flujos * exp(-tasa * (tiempos - semestre))
  
  # Numerador: t * VP
  numerador <- sum((tiempos - semestre) * vp_flujos)
  
  # Denominador: precio del bono
  precio <- sum(vp_flujos)
  
  return(numerador / precio)
}
df1 <- df1 %>%
  mutate(
    Duracion_bono = mapply(duracion_bono, tasa_s1, semestre)
  )

#cupon*sum(exp(-tasa_s1*(25-seq(semestre,25,by=0.5))))
   

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 2 MODELO DE FACTORES
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
BCP <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "BCP")
BCU <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "BCU")
USD <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "USD")
DESEMPLEO <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "Desempleo")
COBRE <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "Cobre")
