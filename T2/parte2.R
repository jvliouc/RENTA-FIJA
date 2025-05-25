#------------------------------------------------------------------------------#
#librerias tipicas:
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(xtable)
library(purrr)
library(broom)
library(tidyr)
#----------------
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
#y el resto
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
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#a
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
#-------------------------------------------------------------------------------

# Lista de modelos con sus nombres crear tabla
modelos <- list(
  `1 año` = modelo_1a,
  `2 años` = modelo_2a,
  `5 años` = modelo_5a,
  `10 años` = modelo_10a
)

# tabla para latex
tabla_resumen <- map_dfr(names(modelos), function(nombre_modelo) {
  modelo <- modelos[[nombre_modelo]]
  coefs <- tidy(modelo) %>% 
    filter(term != "(Intercept)") %>%  # sin intercepto que no dice nada
    select(term, estimate)
  r2 <- glance(modelo)$r.squared
  
  coefs %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(R2 = r2, Modelo = nombre_modelo)
})
#------------------------------------------------------------------------------#
#fin de la tabla
#------------------------------------------------------------------------------#
#la inflacion 
inflacion <- data.frame(
  fecha = BCP$fecha,
  inflacion_esperada = BCP$`10_años` - BCU$bonos_10_años
)

# Gráfico de la inflación esperada
plot(inflacion$fecha, inflacion$inflacion_esperada, type = "l",
     col = "black", lwd = 2,
     xlab = "Fecha", ylab = "Inflación Esperada (10 años)",
     main = "Evolución de la Inflación Esperada a 10 años")
abline(h = 0, col = "gray", lty = 2)

plot(USD$fecha, USD$dolar_observado, type = "l", col = "blue", lwd = 2,
     xlab = "Fecha", ylab = "CLP/USD", main = "Tipo de Cambio Observado")


# media movil con la libreria zoo
DESEMPLEO <- DESEMPLEO %>%
  mutate(deseasonalized = zoo::rollmean(valor, k = 12, fill = NA, align = "right"))

# graf
ggplot(DESEMPLEO, aes(x = fecha)) +
  geom_line(aes(y = valor, color = "Desempleo Original"), size = 1) +
  geom_line(aes(y = deseasonalized, color = "Media Móvil 12 Meses"), size = 1) +
  scale_color_manual(values = c("Desempleo Original" = "red", "Media Móvil 12 Meses" = "blue")) +
  labs(
    title = "Tasa de Desempleo en el Tiempo",
    x = "Fecha",
    y = "Tasa de Desempleo (%)",
    color = "Serie"
  ) +
  theme_minimal()


# Cobre
plot(COBRE$observation_date, as.numeric(COBRE$cobre), type = "l", col = "darkred", lwd = 2,
     xlab = "Fecha", ylab = "USD", main = "Precio del Cobre")
