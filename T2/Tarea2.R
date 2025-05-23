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
monto=100
tasacupon=0.04
f=2 #frecuencia
cupon=monto*tasacupon/f
pagos=monto*f*(exp(r_0/f)-1)/(1-exp(-r_0*20))
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
  vp <- flujos * exp(-r * (tiempos - s))  # descontar desde el semestre actual (t = 0)
  duracion <- sum((tiempos - s) * vp) / sum(vp)
  return(duracion)
}

df1 <- df1 %>%
  mutate(Duracion_bono = mapply(duracion_bono_expr, tasa_s1, semestre),
         X_t=round((Duracion_pension-0.5)/(Duracion_bono-0.5),4),#duraicion del deposito es siempre 0.
         intereses=100*(1-X_t)*tasa_s1*0.5,
         cupones=2*X_t,
         intereses = lag(intereses, default = 0),
         cupones = lag(cupones, default = 0)
    )

df1 <- df1 %>%
  mutate(
    pago = pagos
  ) %>%
  arrange(semestre) %>%
  mutate(
    saldo_capital = accumulate2(
      .x = pago,
      .y = tasa_s1,
      .init = 100,  # capital inicial
      .f = function(saldo_anterior, pago, tasa) {
        nuevo_saldo <- (saldo_anterior - pago) * exp(tasa * 0.5)
        return(nuevo_saldo)
      }
    )[-1]  # remover el valor inicial que es solo para partir
  )



#cupon*sum(exp(-tasa_s1*(25-seq(semestre,25,by=0.5))))
   

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 2 MODELO DE FACTORES
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#tratar los datos, borre las primeras filas que al pasarlo a R daba problemas, le 
#cambie el nombre a las columnas y las paso a numericas y crea una fecha para ver yo
#si esta bien, no es relevante para el informe
BCP <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "BCP")
colnames(BCP) <- c("periodo", "1_año", "2_años", "5_años","10_años")
BCP<-BCP[-c(1, 2), ]
BCP$fecha <- as.Date(as.numeric(BCP$periodo), origin = "1899-12-31")
BCP$`1_año` <- as.numeric(BCP$`1_año`)
BCP$`2_años` <- as.numeric(BCP$`2_años`)
BCP$`5_años` <- as.numeric(BCP$`5_años`)
BCP$`10_años` <- as.numeric(BCP$`10_años`)

# cambio los NA que son muchos por el promedio de la columna
#no quiero tener problemas de NA mas adelante, podrpia haberlos dejado
BCP <- BCP %>%
  mutate(
    `1_año` = ifelse(is.na(`1_año`), mean(`1_año`, na.rm = TRUE), `1_año`),
    `2_años` = ifelse(is.na(`2_años`), mean(`2_años`, na.rm = TRUE), `2_años`),
    `5_años` = ifelse(is.na(`5_años`), mean(`5_años`, na.rm = TRUE), `5_años`),
    `10_años` = ifelse(is.na(`10_años`), mean(`10_años`, na.rm = TRUE), `10_años`)
  )
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#mismo tratamiento a BCU
BCU <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "BCU")
colnames(BCU)<- c("periodo", "bonos_1_años", "bonos_2_años", "bonos_5_años","bonos_10_años","bonos_20_años","bonos_30_años")
BCU<-BCU[-c(1, 2), ]# Convertir columna de fecha
BCU$fecha <- as.Date(as.numeric(BCU$periodo), origin = "1899-12-31")


BCU$bonos_1_años <- as.numeric(BCU$bonos_1_años)
BCU$bonos_2_años <- as.numeric(BCU$bonos_2_años)
BCU$bonos_5_años <- as.numeric(BCU$bonos_5_años)
BCU$bonos_10_años <- as.numeric(BCU$bonos_10_años)
BCU$bonos_20_años <- as.numeric(BCU$bonos_20_años)
BCU$bonos_30_años <- as.numeric(BCU$bonos_30_años)


BCU <- BCU %>%
  mutate(
    bonos_1_años = ifelse(is.na(bonos_1_años), mean(bonos_1_años, na.rm = TRUE), bonos_1_años),
    bonos_2_años = ifelse(is.na(bonos_2_años), mean(bonos_2_años, na.rm = TRUE), bonos_2_años),
    bonos_5_años = ifelse(is.na(bonos_5_años), mean(bonos_5_años, na.rm = TRUE), bonos_5_años),
    bonos_10_años = ifelse(is.na(bonos_10_años), mean(bonos_10_años, na.rm = TRUE), bonos_10_años),
    bonos_20_años = ifelse(is.na(bonos_20_años), mean(bonos_20_años, na.rm = TRUE), bonos_20_años),
    bonos_30_años = ifelse(is.na(bonos_30_años), mean(bonos_30_años, na.rm = TRUE), bonos_30_años)
  )
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
USD <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "USD")
colnames(USD)<-c("periodo", "dolar_observado")
USD<-USD[-c(1, 2), ]
USD$fecha <- as.Date(as.numeric(USD$periodo), origin = "1899-12-31")
#
DESEMPLEO <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "Desempleo")
colnames(DESEMPLEO)<-c("periodo", "valor")
DESEMPLEO<-DESEMPLEO[-c(1), ]
DESEMPLEO$fecha <- as.Date(as.numeric(DESEMPLEO$periodo), origin = "1899-12-31")
#cobre no se le hace nada :D
COBRE <- read_excel("RENTA-FIJA/T2/Datos Tarea 2.xlsx", 
                            sheet = "Cobre")
COBRE$fecha <- as.Date(as.numeric(COBRE[[1]]), origin = "1899-12-31")
colnames(COBRE)[2] <- "cobre"

#a) calcular los factores
BCP$`1_año` <- as.numeric(BCP$`1_año`)
BCP$`5_años` <- as.numeric(BCP$`5_años`)
BCP$`10_años` <- as.numeric(BCP$`10_años`)
BCP <- BCP %>%
  mutate(
    nivel = `10_años`,
    pendiente = `10_años` - `1_año`, #aplico las formulas de clases r10-r1
    curvatura = 2 * `5_años` - (`1_año` + `10_años`) #-corto(1) + 2mediano(5)-largo(10)
  )
#grafico
plot(BCP$fecha, BCP$nivel, type = "l", col = "blue", ylim = range(BCP$nivel, BCP$pendiente, BCP$curvatura, na.rm = TRUE),
     xlab = "Fecha", ylab = "Valor del factor", main = "Evolución de Nivel, Pendiente y Curvatura")
lines(BCP$fecha, BCP$pendiente, col = "darkgreen")
lines(BCP$fecha, BCP$curvatura, col = "darkred")
legend("topright", legend = c("Nivel", "Pendiente", "Curvatura"),
       col = c("blue", "darkgreen", "darkred"), lty = 1, cex=0.5)


#b) las regresiones con las diferencias
#necesito calcular las diferencias, asi:
BCP <- BCP %>%
  mutate(
    d_1a = `1_año` - lag(`1_año`),
    d_2a = `2_años` - lag(`2_años`),
    d_5a = `5_años` - lag(`5_años`),
    d_10a = `10_años` - lag(`10_años`),
    
    d_nivel = nivel - lag(nivel),
    d_pendiente = pendiente - lag(pendiente),
    d_curvatura = curvatura - lag(curvatura)
  )

modelo_1a <- lm(d_1a~ d_nivel + d_pendiente + d_curvatura, data = BCP)
modelo_2a <- lm(d_2a ~ d_nivel + d_pendiente + d_curvatura, data = BCP)
modelo_5a <- lm(d_5a ~ d_nivel + d_pendiente + d_curvatura, data = BCP)
modelo_10a <- lm(d_10a ~ d_nivel + d_pendiente + d_curvatura, data = BCP)

summary(modelo_1a)
summary(modelo_2a)
summary(modelo_5a)
summary(modelo_10a)
#
#la inflacion 
inflacion <- data.frame(
  fecha = BCP$fecha,
  inflacion_esperada = BCP$`10_años` - BCU$bonos_10_años
)

# Gráfico de la inflación esperada
plot(inflacion$fecha, inflacion$inflacion_esperada, type = "l",
     col = "purple", lwd = 2,
     xlab = "Fecha", ylab = "Inflación Esperada (10 años)",
     main = "Evolución de la Inflación Esperada a 10 años")
abline(h = 0, col = "gray", lty = 2)

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# Tipo de cambio
plot(USD$fecha, USD$dolar_observado, type = "l", col = "blue", lwd = 2,
     xlab = "Fecha", ylab = "CLP/USD", main = "Tipo de Cambio Observado")

# Desempleo
plot(DESEMPLEO$fecha, as.numeric(DESEMPLEO$valor), type = "l", col = "darkgreen", lwd = 2,
     xlab = "Fecha", ylab = "Desempleo (%)", main = "Tasa de Desempleo")

# Cobre
plot(COBRE$observation_date, as.numeric(COBRE$cobre), type = "l", col = "darkred", lwd = 2,
     xlab = "Fecha", ylab = "USD", main = "Precio del Cobre")

#
#
#
#
