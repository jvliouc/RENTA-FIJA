#Codigo tarea
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
BILL <- read_excel("RENTA-FIJA/T1/DATOS.xlsx", 
                   sheet = "MARKET BASED BILL")
NOTE <- read_excel("RENTA-FIJA/T1/DATOS.xlsx", 
                   sheet = "MARKET BASED NOTE")
BOND <- read_excel("RENTA-FIJA/T1/DATOS.xlsx", 
                   sheet = "MARKET BASED BOND")
#En el contexto de la tarea, estamos en el 14 de ferbrero, es decir que el primer
#t-note que debe ir en la matriz es el mes 8
# transformo el formato de las fechas a algo que se pueda trabajar en R
NOTE <- NOTE %>%
  mutate(`MATURITY DATE` = as.Date(`MATURITY DATE`))
BOND <- BOND %>%
  mutate(`MATURITY DATE` = as.Date(`MATURITY DATE`))
#-------------------------------------------------------------------------------
#creo las fechas que necesito

fechas  <- seq(from = as.Date("2025-08-15"), by = "6 months", length.out = 16)
#-------------------------------------------------------------------------------
# bindeo los dos df y luego selecciono las 16 fechas seguidas eliminando duplicados
# ademas le pongo el nombre del metodo que vamos a ocupar
ByN <- bind_rows(BOND,NOTE)
boot <- ByN%>%
  arrange(`MATURITY DATE`) %>%                #ordenando por fecha
  filter(`MATURITY DATE` %in% fechas)%>%      #esta linea selecciona por fechas
  distinct(`MATURITY DATE`, .keep_all = TRUE) #esta linea elimina duplicadas
cuantas <- table(boot$`SECURITY TYPE`) #solo quería saber cuantas tengo de cada una :D
print(cuantas)
#-------------------------------------------------------------------------------
#finalmente creo columnas con los nombres que me pide la tarea y limpio el resto
boot <- boot %>%
  mutate('FECHA VENCIMIENTO'= `MATURITY DATE`)%>%
  mutate(PLAZO = round(as.numeric(difftime(`MATURITY DATE`, as.Date("2025-02-14"), 
                                           units = "days")) / 365.25, 1))%>%
  mutate('TASA CUPON'=RATE)%>%
  mutate(PRECIO=(BUY+SELL)/2)
boot_clean <- boot %>%
  select(-names(NOTE))
#-------------------------------------------------------------------------------
#voy a crear la matriz de pago p
C <- matrix(0, nrow = nrow(boot_clean), ncol = nrow(boot_clean))
for (i in 1:nrow(boot_clean)){
  for (j in 1:i){
    if (j<i){
      C[i,j]<-(boot_clean$`TASA CUPON`[i])*100/2
    }else if (j==i){
      C[i,j]<-(boot_clean$`TASA CUPON`[i])*100/2+100
    }
  }
}
P0 <- boot_clean$PRECIO
#-------------------------------------------------------------------------------
C_inversa<-solve(C)
Z0<-C_inversa%*%P0
#agrego los factores de descuento y luego agrego la tasa de interes, nos piden 
#continuamente compuesta asi que la formula es -log(Z)/plazo
boot_clean <- boot_clean %>%
  mutate(Z = Z0)%>%
  mutate(R = (-log(Z)/PLAZO)*100)
#-------------------------------------------------------------------------------
ggplot(boot_clean, aes(x = PLAZO, y = Z)) +
  geom_line() +  
  geom_point(color='purple')+
  ggtitle("Curva de descuento") +
  xlab("Plazo[años]") +
  ylab("Factor de Descuento [$]")+
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 10, by = 0.5)) +
  scale_y_continuous(limits = c(0.5, 1))+
  theme_minimal()

ggplot(boot_clean, aes(x = PLAZO, y = R)) +
  geom_line() +  
  geom_point(color='blue')+
  ggtitle("Yield curve") +
  xlab("Plazo[años]") +
  ylab("Rendimiento[%]")+
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 10, by = 0.5)) +
  scale_y_continuous(limits = c(3, 5)) +
  theme_minimal()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#pregunta 2 REGRESION
#ya tengo los dataframe trabajables, tengo las fechas, simplemento no tengo que
#descartar las fechas duplicadas
regresion <- ByN%>%
  arrange(`MATURITY DATE`) %>%                #ordenando por fecha
  filter(`MATURITY DATE` %in% fechas)     #esta linea selecciona por fechas
  #distinct(`MATURITY DATE`, .keep_all = TRUE) #esta linea elimina duplicadas, simplemente la dejo de ocupar
cuantas <- table(regresion$`SECURITY TYPE`) #solo quería saber cuantas tengo de cada una :D x2
print(cuantas)
#finalmente creo columnas con los nombres que me pide la tarea y limpio el resto x2
regresion <- regresion %>%
  mutate('FECHA VENCIMIENTO'= `MATURITY DATE`)%>%
  mutate(PLAZO = round(as.numeric(difftime(`MATURITY DATE`, as.Date("2025-02-14"), 
                                           units = "days")) / 365.25, 1))%>%
  mutate('TASA CUPON'=RATE)%>%
  mutate(PRECIO=(BUY+SELL)/2)
regresion_clean <- regresion %>%
  select(-names(NOTE))
#-------------------------------------------------------------------------------
#voy a crear la matriz de pago p
C_reg <- matrix(0, nrow = nrow(regresion_clean), ncol = as.integer(regresion_clean$PLAZO[30]/0.5))
for (i in 1:nrow(regresion_clean)){
  p=as.integer(regresion_clean$PLAZO[i]/0.5)# me dice cuantos pagos quedan
  for (j in 1:p){
    if (j<p){
      C_reg[i,j]<-(regresion_clean$`TASA CUPON`[i])*100/2
    }else if (j==p){
      C_reg[i,j]<-(regresion_clean$`TASA CUPON`[i])*100/2+100
    }
  }
}

C_reg_trans<-t(C_reg)
C2_inv<-solve(C_reg_trans%*%C_reg)
P0_reg<-regresion_clean$PRECIO
Z0_reg<-C2_inv%*%C_reg_trans%*%P0_reg

# en este caso no puedo agregarlos al dataframe original, asi que creo uno nuevo

df_plazo_z_R <- data.frame(
  PLAZO=seq(0.5, 8, by = 0.5)
) 
df_plazo_z_R <- df_plazo_z_R %>%
  mutate(Z=Z0_reg) %>%
  mutate(R = (-log(Z)/PLAZO)*100)

ggplot(df_plazo_z_R, aes(x = PLAZO, y = Z)) +
  geom_line() +  
  geom_point(color='purple')+
  ggtitle("Curva de descuento") +
  xlab("Plazo[años]") +
  ylab("Factor de Descuento [$]")+
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 10, by = 0.5)) +
  scale_y_continuous(limits = c(0.5, 1))+
  theme_minimal()

ggplot(df_plazo_z_R, aes(x = PLAZO, y = R)) +
  geom_line() +  
  geom_point(color='blue')+
  ggtitle("Yield curve") +
  xlab("Plazo[años]") +
  ylab("Rendimiento[%]")+
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 10, by = 0.5)) +
  scale_y_continuous(limits = c(3, 5)) +
  theme_minimal()
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#pregunta 3 NELSON & SIEGEL
BILL1<-BILL%>%
  arrange(`MATURITY DATE`) %>%
  mutate(`MATURITY DATE` = as.Date(`MATURITY DATE`))%>%
  mutate(PLAZO = round(as.numeric(difftime(`MATURITY DATE`, as.Date("2025-02-14"), 
                                         units = "days")), 0))%>%
  mutate(PRECIO= 100*(1-(`END OF DAY`/100)*PLAZO/360))%>% #d estaba en porcentaje, en laformula vista en clases no
  mutate(yield=100*((100/PRECIO) -1))

BILL_clean<-BILL1%>%
  select(-names(BILL))
#------------------------------------------------------------------------------#
