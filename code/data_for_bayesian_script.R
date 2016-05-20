#### Este archivo transforma las variables de viajes para el analisis
#install.packages('dummies')
library(lubridate)
library(dplyr)
library(dummies)


path_to_project = "/home/denny/github/DPA_Ecobici/"
#path_to_project = "/home/stuka/itam2/arqui/DPA_Ecobici/"
#setwd("/home/stuka/itam2/arqui/DPA_Ecobici/data/ecobici/viajes")
setwd(paste0(path_to_project,"data/ecobici/viajes"))

####Cargar 2015
filename <- dir(".")[grep(".csv",dir("."))]
filename_2015 <- grep("2015",filename)
filename <- filename[filename_2015]
dtype <- c("character","character","character","character","character","character","character","character","character")
tmp <- lapply(filename,read.csv, header = TRUE, colClasses = dtype)
ecobici <- do.call(rbind,tmp)
rm(tmp)
#head(ecobici)
ecobici$Fecha_hora_retiro = ymd_hms(paste(ecobici$Fecha_Retiro, ecobici$Hora_Retiro))
ecobici$Fecha_hora_arribo = ymd_hms(paste(ecobici$Fecha_Arribo, ecobici$Hora_Arribo))
ecobici$date_retiro = ymd(paste(ecobici$Fecha_Retiro))
ecobici$date_arribo = ymd(paste(ecobici$Fecha_Arribo))
ecobici$anio = year(paste(ecobici$Fecha_Retiro))
#Hay observaciones en donde retiran bicicletas desde el anio anterior, 2014, 20 observaciones, se eliminan
ecobici<-ecobici[ecobici$anio!="2014", ]

ecobici$Edad_Usuario <- as.integer(ecobici$Edad_Usuario)
#ecobici$dia <- as.character(substr(ecobici$Fecha_hora_retiro,6,7))
ecobici$dia <- day(ecobici$Fecha_hora_retiro)
ecobici$dia_wd <- wday(ecobici$Fecha_hora_retiro)
#ecobici$hora <- as.character(substr(ecobici$Fecha_hora_retiro,12,13))
ecobici$hora <- hour(ecobici$Fecha_hora_retiro)
ecobici$fem <- ifelse(ecobici$Genero_Usuario=="F", 1, 0)

#Duracion de viajes, se eliminan duracion = 0 de 3,121 obs, total = 9,188,935
ecobici$dur_via <- minute(as.period(difftime(ecobici$Fecha_hora_arribo,ecobici$Fecha_hora_retiro),minute))
ecobici<-ecobici[ecobici$dur_via!="0",]

### Cargar datos distancias cicloestaciones 
#setwd("/home/stuka/itam2/arqui/DPA_Ecobici/data/ecobici/estaciones/")
setwd(paste0(path_to_project,"data/ecobici/estaciones"))
dtype <- c("character","character","numeric")
ecobici_distancias <- read.csv('distancias_estaciones_metros.csv',colClasses = dtype)
names(ecobici_distancias) <- c("Estacion_origen","Estacion_destino","Distancia_metros")
ecobici <- left_join(ecobici,ecobici_distancias,by=c("Ciclo_Estacion_Retiro"="Estacion_origen","Ciclo_Estacion_Arribo"="Estacion_destino"))
rm(ecobici_distancias)
ecobici <- ecobici %>% mutate(Distancia_metros = replace(Distancia_metros, Ciclo_Estacion_Retiro==Ciclo_Estacion_Arribo, 0)) %>% mutate(Distancia_km = Distancia_metros/1000.0) %>% mutate(Distancia_km = round(Distancia_km,2)) %>% select(-Distancia_metros)


#Genero la matriz
startdate = ymd_hms("2015-01-01 00:00:01 UTC")
enddate = ymd_hms("2015-12-31 23:59:59 UTC")
time <- data.frame(seq(startdate,enddate, by = "60 min"))
names(time) <- c("time")
glimpse(time)
time <- filter(time,wday(time)>=2,wday(time)<=6)
time <- filter(time,hour(time)>=6,hour(time)<12)

temp <- ecobici %>% select(Ciclo_Estacion_Retiro) %>% distinct() 
temp2 <- ecobici %>% select(Ciclo_Estacion_Arribo) %>% distinct()
names(temp2) <- names(temp)
estaciones <- distinct(rbind(temp,temp2)) %>% arrange(Ciclo_Estacion_Retiro)
estaciones <- estaciones[1:444,1]

matrix <- merge(time,estaciones,by=NULL) #tamano 695,304
rm(estaciones,temp,temp2,time,dtype)
matrix$date <- as_date(matrix$time)
matrix$wd <- wday(matrix$time)
matrix$hour <- hour(matrix$time)
matrix <- tbl_df(matrix)
head(matrix,20)
#length(unique(matrix$y))

######

viajes_sem_hora_salidas <- data.frame()
viajes_sem_hora_salidas <- (ecobici %>% select(Ciclo_Estacion_Retiro, Genero_Usuario, Edad_Usuario, date_retiro, dia_wd, hora, fem, dur_via, Distancia_km)
                            %>% filter(dia_wd>=2 & dia_wd<=6 & (hora==6 |  hora==7 |  hora==8 |  hora==9 |  hora==10 |  hora==11 ))
                            %>% group_by(Ciclo_Estacion_Retiro, date_retiro, dia_wd, hora) 
                            %>% summarise(viajes_retiro=n(), edad_prom=mean(Edad_Usuario), prop_fem=mean(fem), duracion_prom=mean(dur_via), distancia_prom=mean(Distancia_km)))
names(viajes_sem_hora_salidas)[1] <- "estacion"

viajes_sem_hora_llegadas <- data.frame() #En este caso quiero los viajes que arribaron a la estacion x
viajes_sem_hora_llegadas <- (ecobici %>% select(Ciclo_Estacion_Arribo, date_arribo, dia_wd, hora)
                             %>% filter(dia_wd>=2 & dia_wd<=6 & (hora==6 |  hora==7 |  hora==8 |  hora==9 |  hora==10 |  hora==11 ))
                             %>% group_by(Ciclo_Estacion_Arribo, date_arribo, dia_wd, hora) 
                             %>% summarise(viajes_arribo=n())) 
names(viajes_sem_hora_llegadas)[1] <- "estacion"

master<- data.frame()
master <- left_join(viajes_sem_hora_salidas, viajes_sem_hora_llegadas, by=c("estacion","date_retiro"= "date_arribo", "dia_wd", "hora"))
rm(ecobici)


### Cargar regiones de estaciones
setwd(paste0(path_to_project,"data/ecobici/regiones/"))
dtype <- c("character", "character")
ecobici_regiones <- read.csv('estacion_region.csv',colClasses = dtype)
names(ecobici_regiones) <- c("estacion", "regiones")

m_final<- data.frame()
m_final <- left_join(matrix, master, by=c("y"="estacion","date"= "date_retiro", "hour"="hora"))
m_final$dia_wd <- NULL
#QUEDARON NAs POR LAS ESTACIONES QUE NO TUVIERON MOVIMIENTO EN ESAS HORAS, SE RELLENAN DE CEROS
m_final <-m_final %>%  
  mutate(viajes_retiro = ifelse(is.na(viajes_retiro),0,viajes_retiro)) %>%
  mutate(edad_prom = ifelse(is.na(edad_prom),0,edad_prom)) %>%
  mutate(prop_fem = ifelse(is.na(prop_fem),0,prop_fem)) %>%
  mutate(duracion_prom = ifelse(is.na(duracion_prom),0,duracion_prom)) %>%
  mutate(distancia_prom = ifelse(is.na(distancia_prom),0,distancia_prom)) %>%
  mutate(viajes_arribo = ifelse(is.na(viajes_arribo),0,viajes_arribo))
  
m_final <- left_join(m_final,ecobici_regiones,by=c("y"="estacion"))
# Para corroborar porque hay NAs en regiones, son las regiones 1001, 1002 y 1003, eliminare estas filas
m_final[is.na(m_final$regiones),] #en este paso tenemos 695,304, y tenemos 12 filas con NAs
m_final <- m_final[is.na(m_final$regiones)==FALSE,] #quedan 690,606


#PARA CREAR DUMMIES DE REGIONES
names(m_final$regiones) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
for(level in unique(m_final$regiones)){
  m_final[paste("region", level, sep = "_")] <- ifelse(m_final$regiones == level, 1, 0)
}

#PARA CREAR DUMMIES DE DIAS
names(m_final$wd) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
for(level in unique(m_final$wd)){
  m_final[paste("wd", level, sep = "_")] <- ifelse(m_final$wd == level, 1, 0)
}

#PARA CREAR DUMMIES DE HORA
names(m_final$hour) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
for(level in unique(m_final$hour)){
  m_final[paste("hora", level, sep = "_")] <- ifelse(m_final$hour == level, 1, 0)
}


### Cargar contaminantes y humedad
setwd(paste0(path_to_project,"data/meteorologia/"))
dtype <- c("character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character")
ecobici_met <- read.csv('ecobici_contaminantes_meteorologia_2015.csv',colClasses = dtype)
names(ecobici_met) <- c("fecha_hora","date","hour","O3","PM10","MAX_DIA_O3","MAX_DIA_PM10","RH","TMP","WDR","WSP","MAX_DIA_RH","MAX_DIA_TMP","MAX_DIA_WDR","MAX_DIA_WSP")

ecobici_met$date <- as_date(ecobici_met$date)
ecobici_met$hour <- as.integer(ecobici_met$hour)

#Pegamos los datos de contaminantes y humedad por date y hora
m_final <- left_join(m_final, ecobici_met, by=c("date"= "date", "hour"="hour"))

#Cambio el tipo de varias variables
m_final$O3 <- as.numeric(m_final$O3)
m_final$PM10 <- as.numeric(m_final$PM10)
m_final$MAX_DIA_O3 <- as.numeric(m_final$MAX_DIA_O3)
m_final$MAX_DIA_PM10 <- as.numeric(m_final$MAX_DIA_PM10)
m_final$RH <- as.numeric(m_final$RH)
m_final$TMP <- as.numeric(m_final$TMP)
m_final$WDR <- as.numeric(m_final$WDR)
m_final$WSP <- as.numeric(m_final$WSP)
m_final$MAX_DIA_RH<- as.numeric(m_final$MAX_DIA_RH)
m_final$MAX_DIA_TMP <- as.numeric(m_final$MAX_DIA_TMP)
m_final$MAX_DIA_WDR <- as.numeric(m_final$MAX_DIA_WDR)
m_final$MAX_DIA_WSP <- as.numeric(m_final$MAX_DIA_WSP)

##### Limpieza de variables temporales para no matar la memoria
rm(a,b,c,ecobici,ecobici_regiones,viajes_sem_hora_llegadas,viajes_sem_hora_llegadas,viajes_sem_hora_salidas,viajes_sem_hora_salidas,dtype,filename,filename_2015, master, matrix, ecobici_met)

#### Cambiar estacion a entero
m_final$y <- as.integer(m_final$y)
#La segunda columna "y" es estacion, la renombro
colnames(m_final)[2] <- "estacion"

#### Cargar variable y emulada
setwd(paste0(path_to_project,"data/ecobici/"))
var_y <- tbl_df(read.csv('ecobici_variable_y_emulada_2015.csv'))

### Pegar campos flujo_abs, maxcapacity y flujo_rel a la matriz master por estacion, fecha, dia semana y hora
var_y$dia_fecha <- as_date(var_y$dia_fecha)
master <- left_join(m_final,var_y,by=c("estacion"="estacion","date"="dia_fecha","wd"="wday","hour"="hora")) #al final nos quedamos con 690,606

rm(m_final)
rm(var_y)

setwd(paste0(path_to_project,"data/"))
write.table(master, "master_data.csv", sep = ",", col.names = TRUE, row.names = FALSE)
