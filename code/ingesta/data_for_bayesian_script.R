#### Este archivo transforma las variables de viajes para el analisis
#install.packages('dummies')
library(lubridate)
library(dplyr)
library(dummies)


path_to_project = "/home/stuka/itam2/arqui/DPA_Ecobici/"
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

ecobici$Edad_Usuario <- as.integer(ecobici$Edad_Usuario)
#ecobici$dia <- as.character(substr(ecobici$Fecha_hora_retiro,6,7))
ecobici$dia <- day(ecobici$Fecha_hora_retiro)
ecobici$dia_wd <- wday(ecobici$Fecha_hora_retiro)
#ecobici$hora <- as.character(substr(ecobici$Fecha_hora_retiro,12,13))
ecobici$hora <- hour(ecobici$Fecha_hora_retiro)
ecobici$fem <- ifelse(ecobici$Genero_Usuario=="F", 1, 0)
#ecobici$a <- seq(1,1,nrow(ecobici))

#Duracion de viajes, se eliminan duracion = 0
ecobici$dur_via <- minute(as.period(difftime(ecobici$Fecha_hora_arribo,ecobici$Fecha_hora_retiro),minute))
ecobici<-ecobici[!(ecobici$dur_via=="0"),]

### Cargar datos distancias cicloestaciones 
#setwd("/home/stuka/itam2/arqui/DPA_Ecobici/data/ecobici/estaciones/")
setwd(paste0(path_to_project,"data/ecobici/estaciones"))
dtype <- c("character","character","numeric")
ecobici_distancias <- read.csv('distancias_estaciones_metros.csv',colClasses = dtype)
names(ecobici_distancias) <- c("Estacion_origen","Estacion_destino","Distancia_metros")

ecobici <- left_join(ecobici,ecobici_distancias,by=c("Ciclo_Estacion_Retiro"="Estacion_origen","Ciclo_Estacion_Arribo"="Estacion_destino"))
rm(ecobici_distancias)
ecobici <- ecobici %>% mutate(Distancia_metros = replace(Distancia_metros, Ciclo_Estacion_Retiro==Ciclo_Estacion_Arribo, 0)) %>% mutate(Distancia_km = Distancia_metros/1000.0) %>% mutate(Distancia_km = round(Distancia_km,2)) %>% select(-Distancia_metros)


viajes_sem_hora_salidas <- data.frame()
viajes_sem_hora_salidas <- (ecobici %>% select(Ciclo_Estacion_Retiro, Genero_Usuario, Edad_Usuario, date_retiro, dia_wd, hora, fem, dur_via, Distancia_km)
                            %>% filter(dia_wd>=2 & dia_wd<=6 & (hora==6 |  hora==7 |  hora==8 |  hora==9 |  hora==10 |  hora==11 ))
                            %>% group_by(Ciclo_Estacion_Retiro, date_retiro, dia_wd, hora) 
                            %>% summarise(viajes_retiro=n(), edad_prom=mean(Edad_Usuario), prop_fem=mean(fem), duracion_prom=mean(dur_via), distancia_prom=mean(Distancia_km)))

viajes_sem_hora_salidas_2 <- (viajes_sem_hora_salidas %>% select(Ciclo_Estacion_Retiro, dia_wd, hora, viajes_retiro, edad_prom, prop_fem, duracion_prom, distancia_prom)
                              %>% group_by(Ciclo_Estacion_Retiro, dia_wd, hora)
                              %>% summarise(viajes_retiro_prom=mean(viajes_retiro), edad_p=mean(edad_prom), prop_f=mean(prop_fem), duracion_p=mean(duracion_prom), dist_prom=mean(distancia_prom)))

names(viajes_sem_hora_salidas_2)[1] <- "estacion"

viajes_sem_hora_llegadas <- data.frame()
viajes_sem_hora_llegadas <- (ecobici %>% select(Ciclo_Estacion_Arribo, Genero_Usuario, Edad_Usuario, date_retiro, dia_wd, hora, fem)
                             %>% filter(dia_wd>=2 & dia_wd<=6 & (hora==6 |  hora==7 |  hora==8 |  hora==9 |  hora==10 |  hora==11 ))
                             %>% group_by(Ciclo_Estacion_Arribo, date_retiro, dia_wd, hora) 
                             %>% summarise(viajes_arribo=n()))            

viajes_sem_hora_llegadas_2 <- (viajes_sem_hora_llegadas %>% select(Ciclo_Estacion_Arribo, dia_wd, hora, viajes_arribo)
                               %>% group_by(Ciclo_Estacion_Arribo, dia_wd, hora)
                               %>% summarise(viajes_arribo_prom=mean(viajes_arribo)))

names(viajes_sem_hora_llegadas_2)[1] <- "estacion"


master<- data.frame()
master <- inner_join(viajes_sem_hora_salidas_2, viajes_sem_hora_llegadas_2, by=c("estacion","dia_wd", "hora"))



### Cargar regiones de estaciones
setwd(paste0(path_to_project,"data/ecobici/regiones/"))
setwd(paste0(path_to_project,"data/ecobici/regiones"))
dtype <- c("character", "character")
ecobici_regiones <- read.csv('estacion_region.csv',colClasses = dtype)
names(ecobici_regiones) <- c("estacion", "regiones")

master <- left_join(master,ecobici_regiones,by=c("estacion"="estacion"))
#rm(ecobici_regiones)

#Existen estaciones que tienen NA en region, son las estaciones 1001, 1002, 1003, por lo que eliminamos filas de esas estaciones
nrow(subset(master, is.na(master$regiones)))
unique(subset(master$estacion, is.na(master$regiones)))
master <- master[is.na(master$regiones)==FALSE,]

names(master)[10] <- "region"

#Dummies de region
a <- as.data.frame(dummy(master$region))
names(a) <- gsub("region)", "region_", names(a))
master <- cbind(master,a)

#Dummies de dia
b <- as.data.frame(dummy(master$dia_wd))
names(b) <- gsub("dia_wd)", "dia_", names(b))
master <- cbind(master,b)

#Dummies de hora
c <- as.data.frame(dummy(master$hora))
names(c) <- gsub("hora)", "hora_", names(c))
master <- cbind(master,c)


##### Limpieza de variables temporales para no matar la memoria
rm(a,b,c,ecobici,ecobici_regiones,viajes_sem_hora_llegadas_2,viajes_sem_hora_llegadas,viajes_sem_hora_salidas_2,viajes_sem_hora_salidas,dtype,filename,filename_2015)

#### Cambiar estacion a entero
master$estacion <- as.integer(master$estacion)
#### Cargar variable y emulada
var_y <- tbl_df(read.csv('../ecobici_variable_y_emulada_2015.csv'))
### Pegar campos flujo_abs, maxcapacity y flujo_rel a la matriz master por estacion, fecha, dia semana y hora
master <- left_join(master,var_y,by=c("estacion"="estacion","dia_fecha"="dia_fecha","dia_wd"="wday","hora"="hora"))
write.table(master, "../ecobici_master_var_y_2015.csv", sep = ",", col.names = TRUE, row.names = FALSE)
