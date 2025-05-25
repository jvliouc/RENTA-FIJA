#------------------------------------------------------------------------------#
#librerias tipicas:
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(xtable)
library(purrr)
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
matplot(t, round(100*matriz,2), type = "l", lty = 1, lwd = 2,
        col = rainbow(caminos),
        xlab = "Tiempo (años)", ylab = "Tasa",
        main = "Diferentes caminos para la tasa de interés")
legend("topright", legend = paste("Camino", 1:caminos), col = rainbow(caminos), lty = 1, lwd = 2, cex=0.5)
#------------------------------------------------------------------------------#
#b)1
#en millones
monto <- 100
r_0 <- 0.039695
f <- 2
T <- 20
pagos <- monto * f * (exp(r_0 / f) - 1) / (1 - exp(-r_0 * T))
pagos_semestrales=pagos/f
df1<-data.frame(semestre=t)
#Lista: tasasimulada[CHEK], saldo capital, VPflujosfuturopension [CHEK],duracionpensión[chek],
#precio_bono_c[CHEKC], Duración_bono_cupon[CHEK], prop_bonos,
precio_bono_expr <- function(tasa, semestre) {
  tiempos <- seq(semestre + 0.5, 25, by = 0.5)
  cupones <- cupon * sum(exp(-tasa * (tiempos - semestre)))  # descuentos relativos al semestre actual
  nominal <- 100 * exp(-tasa * (25 - semestre))
  return(cupones + nominal)
}

df1 <- df1 %>%
  mutate(
    tasa_s1 = matriz[,1],
    VP_flujos_pension = pagos * (1 - exp(-tasa_s1 * (20 - semestre))) / 
      (f * (exp(tasa_s1 / f) - 1)),
    Duracion_pension = (1 / f) * (
      (exp(tasa_s1 / f) / (exp(tasa_s1 / f) - 1)) -
        ((exp(-tasa_s1 * (20 - semestre)) * ((20 - semestre) * f)) / 
           (1 - exp(-tasa_s1 * (20 - semestre))))
    ),
    Duracion_pension = ifelse(semestre == 20, 0, Duracion_pension),
    Precio_bono = mapply(precio_bono_expr, tasa_s1, semestre)
  )
#tengo que hacer una funcion igual para la duracion
duracion_bono_expr <- function(r, s) {
  tiempos <- seq(s + 0.5, 25, by = 0.5)
  flujos <- rep(2, length(tiempos))
  flujos[length(flujos)] <- flujos[length(flujos)] + 100  # último flujo incluye nominal
  vp <- flujos * exp(-r * (tiempos - s))  # valor presente descontado desde t = s
  duracion <- sum((tiempos - s) * vp) / sum(vp)
  return(duracion)
}

df1 <- df1 %>%
  mutate(
    Duracion_bono = mapply(duracion_bono_expr, tasa_s1, semestre),
    
    # Protegemos contra divisiones por cero
    X_t = ifelse(abs(Duracion_bono - 0.5) < 1e-6, 0, 
                 round((Duracion_pension - 0.5) / (Duracion_bono - 0.5), 4)),
    
    # Flujos generados en el semestre actual
    intereses_nolag = 100 * (1 - X_t) * tasa_s1 * 0.5,  # depósitos a tasa r semestral
    cupones_nolag = 2 * X_t,                            # bonos pagan 2 semestral
    
    # Lag para usar los flujos en el semestre siguiente (se reciben al inicio)
    intereses = lag(intereses_nolag, default = 0),
    cupones = lag(cupones_nolag, default = 0)
  )
#quiero calcular w_t, asi que pasare esto a excel 

library(writexl)

write_xlsx(df1, "resultado1.xlsx")
#------------------------------------------------------------------------------#
#para la tasa 2
df2<-data.frame(semestre=t)
df2 <- df2 %>%
  mutate(
    tasa_s2 = matriz[,2],
    VP_flujos_pension = pagos * (1 - exp(-tasa_s2 * (20 - semestre))) / 
      (f * (exp(tasa_s2 / f) - 1)),
    Duracion_pension = (1 / f) * (
      (exp(tasa_s2 / f) / (exp(tasa_s2 / f) - 1)) -
        ((exp(-tasa_s2 * (20 - semestre)) * ((20 - semestre) * f)) / 
           (1 - exp(-tasa_s2 * (20 - semestre))))
    ),
    Duracion_pension = ifelse(semestre == 20, 0, Duracion_pension),
    Precio_bono = mapply(precio_bono_expr, tasa_s2, semestre)
  )
df2 <- df2 %>%
  mutate(
    Duracion_bono = mapply(duracion_bono_expr, tasa_s2, semestre),
    
    # Protegemos contra divisiones por cero
    X_t = ifelse(abs(Duracion_bono - 0.5) < 1e-6, 0, 
                 round((Duracion_pension - 0.5) / (Duracion_bono - 0.5), 4)),
    
    # Flujos generados en el semestre actual
    intereses_nolag = 100 * (1 - X_t) * tasa_s2 * 0.5,  # depósitos a tasa r semestral
    cupones_nolag = 2 * X_t,                            # bonos pagan 2 semestral
    
    # Lag para usar los flujos en el semestre siguiente (se reciben al inicio)
    intereses = lag(intereses_nolag, default = 0),
    cupones = lag(cupones_nolag, default = 0)
  )
write_xlsx(df2, "resultado2.xlsx")
#df3
df3<-data.frame(semestre=t)
df3 <- df3 %>%
  mutate(
    tasa_s3 = matriz[,3],
    VP_flujos_pension = pagos * (1 - exp(-tasa_s3 * (20 - semestre))) / 
      (f * (exp(tasa_s3 / f) - 1)),
    Duracion_pension = (1 / f) * (
      (exp(tasa_s3 / f) / (exp(tasa_s3 / f) - 1)) -
        ((exp(-tasa_s3 * (20 - semestre)) * ((20 - semestre) * f)) / 
           (1 - exp(-tasa_s3 * (20 - semestre))))
    ),
    Duracion_pension = ifelse(semestre == 20, 0, Duracion_pension),
    Precio_bono = mapply(precio_bono_expr, tasa_s3, semestre)
  )
df3 <- df3 %>%
  mutate(
    Duracion_bono = mapply(duracion_bono_expr, tasa_s3, semestre),
    
    # Protegemos contra divisiones por cero
    X_t = ifelse(abs(Duracion_bono - 0.5) < 1e-6, 0, 
                 round((Duracion_pension - 0.5) / (Duracion_bono - 0.5), 4)),
    
    # Flujos generados en el semestre actual
    intereses_nolag = 100 * (1 - X_t) * tasa_s3 * 0.5,  # depósitos a tasa r semestral
    cupones_nolag = 2 * X_t,                            # bonos pagan 2 semestral
    
    # Lag para usar los flujos en el semestre siguiente (se reciben al inicio)
    intereses = lag(intereses_nolag, default = 0),
    cupones = lag(cupones_nolag, default = 0)
  )
write_xlsx(df3, "resultado3.xlsx")
#s4
df4 <- data.frame(semestre = t)

df4 <- df4 %>%
  mutate(
    tasa_s4 = matriz[, 4],
    VP_flujos_pension = pagos * (1 - exp(-tasa_s4 * (20 - semestre))) / 
      (f * (exp(tasa_s4 / f) - 1)),
    Duracion_pension = (1 / f) * (
      (exp(tasa_s4 / f) / (exp(tasa_s4 / f) - 1)) -
        ((exp(-tasa_s4 * (20 - semestre)) * ((20 - semestre) * f)) / 
           (1 - exp(-tasa_s4 * (20 - semestre))))
    ),
    Duracion_pension = ifelse(semestre == 20, 0, Duracion_pension),
    Precio_bono = mapply(precio_bono_expr, tasa_s4, semestre)
  )

df4 <- df4 %>%
  mutate(
    Duracion_bono = mapply(duracion_bono_expr, tasa_s4, semestre),
    
    # Protegemos contra divisiones por cero
    X_t = ifelse(abs(Duracion_bono - 0.5) < 1e-6, 0, 
                 round((Duracion_pension - 0.5) / (Duracion_bono - 0.5), 4)),
    
    # Flujos generados en el semestre actual
    intereses_nolag = 100 * (1 - X_t) * tasa_s4 * 0.5,
    cupones_nolag = 2 * X_t,
    
    # Lag para usar los flujos en el semestre siguiente (se reciben al inicio)
    intereses = lag(intereses_nolag, default = 0),
    cupones = lag(cupones_nolag, default = 0)
  )

write_xlsx(df4, "resultado4.xlsx")

#s5
df5 <- data.frame(semestre = t)

df5 <- df5 %>%
  mutate(
    tasa_s5 = matriz[, 5],
    VP_flujos_pension = pagos * (1 - exp(-tasa_s5 * (20 - semestre))) / 
      (f * (exp(tasa_s5 / f) - 1)),
    Duracion_pension = (1 / f) * (
      (exp(tasa_s5 / f) / (exp(tasa_s5 / f) - 1)) -
        ((exp(-tasa_s5 * (20 - semestre)) * ((20 - semestre) * f)) / 
           (1 - exp(-tasa_s5 * (20 - semestre))))
    ),
    Duracion_pension = ifelse(semestre == 20, 0, Duracion_pension),
    Precio_bono = mapply(precio_bono_expr, tasa_s5, semestre)
  )

df5 <- df5 %>%
  mutate(
    Duracion_bono = mapply(duracion_bono_expr, tasa_s5, semestre),
    
    # Protegemos contra divisiones por cero
    X_t = ifelse(abs(Duracion_bono - 0.5) < 1e-6, 0, 
                 round((Duracion_pension - 0.5) / (Duracion_bono - 0.5), 4)),
    
    # Flujos generados en el semestre actual
    intereses_nolag = 100 * (1 - X_t) * tasa_s5 * 0.5,
    cupones_nolag = 2 * X_t,
    
    # Lag para usar los flujos en el semestre siguiente
    intereses = lag(intereses_nolag, default = 0),
    cupones = lag(cupones_nolag, default = 0)
  )

write_xlsx(df5, "resultado5.xlsx")

#=(G2*L2*E3/E2)+(1-G2)*L2*(1+B2/2)
#esa es la formula en excel