#### Este archivo calcula el flujo promedio cada n minutos por estacion por dia por hora entre semana en la mañana

library(lubridate)
library(dplyr)


rm(list = ls())
getwd()
path_to_project = "/home/stuka/itam2/arqui/DPA_Ecobici/"
setwd(paste0(path_to_project,"data/ecobici/viajes/"))
dir()

####Cargar 1 mes
#filename <- "2015-12.csv"
#dtype <- c("factor","numeric","character","numeric","character","character","numeric","character","character")
#ecobici <- read.csv(filename,header=TRUE,colClasses = dtype,na.strings = c("NA","lab009"))
#ecobici = tbl_df(ecobici)

####Cargar 2015
filename <- dir(".")[grep(".csv",dir("."))]
filename_2015 <- grep("2015",filename)
filename <- filename[filename_2015]
dtype <- c("character","character","character","character","character","character","character","character","character")
tmp <- lapply(filename,read.csv, header = TRUE, colClasses = dtype)
ecobici <- do.call(rbind,tmp)
rm(tmp)

head(ecobici)
ecobici = tbl_df(ecobici)
ecobici$Fecha_hora_retiro = ymd_hms(paste(ecobici$Fecha_Retiro, ecobici$Hora_Retiro))
ecobici$Fecha_hora_arribo = ymd_hms(paste(ecobici$Fecha_Arribo, ecobici$Hora_Arribo))
ecobici <- ecobici[,c(1,2,3,4,10,7,11)]
ecobici$Edad_Usuario <- as.integer(ecobici$Edad_Usuario)
ecobici$Ciclo_Estacion_Retiro <- as.integer(ecobici$Ciclo_Estacion_Retiro)
ecobici$Ciclo_Estacion_Arribo <- as.integer(ecobici$Ciclo_Estacion_Arribo)
############################################################################################################################3

head(ecobici)
startdate = ymd_hms("2015-01-01 00:00:01 UTC")
enddate = ymd_hms("2015-12-31 23:59:59 UTC")
time <- data.frame(seq(startdate,enddate, by = "10 min"))
names(time) <- c("time")
glimpse(time)
time <- filter(time,wday(time)>=2,wday(time)<=6)
time <- filter(time,hour(time)>=6,hour(time)<12)

temp <- ecobici %>% select(Ciclo_Estacion_Retiro) %>% distinct() 
temp2 <- ecobici %>% select(Ciclo_Estacion_Arribo) %>% distinct()
names(temp2) <- names(temp)
estaciones <- distinct(rbind(temp,temp2)) %>% arrange(Ciclo_Estacion_Retiro)
estaciones <- estaciones[1:444,1]


matrix <- merge(time,estaciones,by=NULL)
rm(estaciones,temp,temp2,time,dtype)
matrix$date <- as_date(matrix$time)
matrix$hour <- hour(matrix$time)
matrix$minute <- minute(matrix$time)
matrix <- tbl_df(matrix)
head(matrix,20)
length(unique(matrix$minute))

breaks_min <- c(0,10,20,30,40,50,60)
ecobici$minutos_cut_retiro <- cut(minute(ecobici$Fecha_hora_retiro), breaks=breaks_min,right=FALSE)
viajes_salidas <- (ecobici %>% 
                     select(Ciclo_Estacion_Retiro, Fecha_hora_retiro, minutos_cut_retiro) %>% 
                     filter(wday(Fecha_hora_retiro)>=2 & wday(Fecha_hora_retiro)<=6 & hour(Fecha_hora_retiro)>=6 & hour(Fecha_hora_retiro)<=11) %>% 
                     group_by(Ciclo_Estacion_Retiro, as_date(Fecha_hora_retiro), wday(Fecha_hora_retiro), hour(Fecha_hora_retiro),minutos_cut_retiro) %>% 
                     summarise(viajes=n()))

ecobici$minutos_cut_arribo <- cut(minute(ecobici$Fecha_hora_arribo), breaks=breaks_min,right=FALSE)
viajes_llegadas <- (ecobici %>% 
                      select(Ciclo_Estacion_Arribo, Fecha_hora_arribo, minutos_cut_arribo) %>% 
                      filter(wday(Fecha_hora_arribo)>=2 & wday(Fecha_hora_arribo)<=6 & hour(Fecha_hora_arribo)>=6 & hour(Fecha_hora_arribo)<=11) %>% 
                      group_by(Ciclo_Estacion_Arribo, as_date(Fecha_hora_arribo), wday(Fecha_hora_arribo), hour(Fecha_hora_arribo),minutos_cut_arribo) %>% 
                      summarise(viajes=n()))

replace_minute <- data.frame(min_lev = unique(ecobici$minutos_cut_retiro), min_val=breaks_min[1:length(breaks_min)-1])

#head(viajes_salidas)
viajes_salidas <- left_join(viajes_salidas,replace_minute,by = c("minutos_cut_retiro"="min_lev"))
viajes_salidas <- viajes_salidas[,c(1,2,4,7,6)]
names(viajes_salidas) <- c("sal_station","sal_date","sal_hour","sal_minute","sal_viajes")

#plot(viajes_salidas$sal_station,viajes_salidas$sal_viajes)
#hist(viajes_salidas$sal_viajes)
#summary(viajes_salidas$sal_viajes)
#View(filter(viajes_salidas, sal_viajes >= 20))

viajes_llegadas <- left_join(viajes_llegadas,replace_minute,by = c("minutos_cut_arribo"="min_lev"))
viajes_llegadas <- viajes_llegadas[,c(1,2,4,7,6)]
names(viajes_llegadas) <- c("ent_station","ent_date","ent_hour","ent_minute","ent_viajes")

#plot(viajes_llegadas$ent_station,viajes_llegadas$ent_viajes)
#hist(viajes_llegadas$ent_viajes)
#summary(viajes_llegadas$ent_viajes)
#View(filter(viajes_llegadas, ent_viajes >= 20))

sum(viajes_salidas$sal_viajes)  #191,227
sum(viajes_llegadas$ent_viajes) #186,398


matrix_llegadas <- left_join(matrix,viajes_llegadas,by=c("Ciclo_Estacion_Retiro"="ent_station","date"="ent_date","hour"="ent_hour","minute"="ent_minute"))

matrix_completa <- left_join(matrix_llegadas,viajes_salidas,by=c("Ciclo_Estacion_Retiro"="sal_station","date"="sal_date","hour"="sal_hour","minute"="sal_minute"))

sapply(matrix_completa[,6:7],sum,na.rm=TRUE)
##Las que se pierden son de 1001 y 1002 que no existen
rm(viajes_llegadas,viajes_salidas,matrix,matrix_llegadas,replace_minute,breaks_min,ecobici)

matrix_completa <-matrix_completa %>%  
  mutate(ent_viajes = ifelse(is.na(ent_viajes),0,ent_viajes)) %>%
  mutate(sal_viajes = ifelse(is.na(sal_viajes),0,sal_viajes)) %>%
  mutate(viajes_flow = 1.0*(ent_viajes - sal_viajes))

matrix_grouped <- matrix_completa %>% 
  group_by(Ciclo_Estacion_Retiro, date, hour) %>% 
  summarise(flujo_abs=mean(viajes_flow))

rm(matrix_completa)

#matrix_grouped <- matrix_grouped %>% mutate(fecha_hora = ymd_hms(paste0(date," ",hour,":00:00")))
matrix_grouped <- matrix_grouped %>% mutate(wday = wday(dia_fecha))
matrix_grouped <- matrix_grouped %>% select(estacion = Ciclo_Estacion_Retiro, dia_fecha = date,hora = hour, flujo_abs)

### Cargar datos distancias max capacity 
dir("../estaciones/")

max_capacity_estaciones <- tbl_df(read.csv('../estaciones/max_capacity_stations.csv'))

glimpse(max_capacity_estaciones)


variable_y <- left_join(matrix_grouped,max_capacity_estaciones,by=c("estacion"="id"))
variable_y <- variable_y %>% mutate(flujo_rel = flujo_abs/maxcapacity)

variable_y[order(variable_y$flujo_rel,decreasing=TRUE),]
variable_y[order(variable_y$flujo_rel,decreasing=FALSE),]
#### Exportar información a csv
#write.table(variable_y, "../ecobici_variable_y_emulada_2015.csv", sep = ",", col.names = TRUE, row.names = FALSE)
rm(matrix_grouped,max_capacity_estaciones,variable_y,enddate,startdate)
