#Codigo tarea
library(readxl)
BILL <- read_excel("RENTA-FIJA/T1/DATOS.xlsx", 
                   sheet = "MARKET BASED BILL")
NOTE <- read_excel("RENTA-FIJA/T1/DATOS.xlsx", 
                   sheet = "MARKET BASED NOTE")
BOND <- read_excel("RENTA-FIJA/T1/DATOS.xlsx", 
                   sheet = "MARKET BASED BOND")