#### Este archivo calcula el flujo promedio cada n minutos por estacion por dia por hora entre semana en la mañana

library(lubridate)
library(dplyr)
library(tidyr)
library(RCurl)
library(XML)
library(stringr)

if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github('diegovalle/aire.zmvm')
library(aire.zmvm)

rm(list = ls())
getwd()
path_to_project = "/home/stuka/itam2/arqui/DPA_Ecobici/"
setwd(paste0(path_to_project,"data/contaminantes/"))
dir("..")


######################## NO USAR #####################################################################
zz=gzfile('contaminantes_2015.csv.gz','rt')  
contaminantes=read.csv(zz,header=T,skip=10) 
head(contaminantes,50)
contaminantes$unit <- NULL
contaminantes <- contaminantes %>% spread(id_parameter,value)

head(contaminantes,50)
glimpse(contaminantes)
contaminantes$date = as.character(contaminantes$date)

contaminantes$date <-  parse_date_time(contaminantes$date, "%d/%m/%Y %H:%M")
  
 

contaminantes_slim <- (contaminantes %>% 
                     select(date, id_station, O3) %>% 
                     filter(wday(date)>=2 & wday(date)<=6 & hour(date)>=6 & hour(date)<=11))
 
table(contaminantes_slim[which(is.na(contaminantes_slim$O3)),]$id_station)/table(contaminantes_slim$id_station)

length(table(contaminantes_slim$id_station))


#################################Segundo Intento###################################################
###No USAR
url <- "http://www.aire.df.gob.mx/estadisticas-consultas/concentraciones/respuesta.php?"
tipo_query <- "qtipo=HORARIOS&"
variable <- "parametro=o3&"
anio <- "anio=2015&"
mes <- paste("qmes=",c("01","02","03","04","05","06","07","08","09","10","11","12"),sep="")
queries <- paste0(url,tipo_query,variable,anio,mes)
urldata <- getURL(queries[1])
data <- readHTMLTable(urldata,stringsAsFactors = FALSE,header=TRUE)

data <- data[[1]]
#str(data)

data[1,] <- str_trim(gsub("Â","",data[1,]))
names(data) <- data[1,]
data <- data[2:nrow(data),]
head(data)
data[ data == "nr" ] = NA

data <- gather(data,id_station, o3, 3:ncol(data))
data$fecha_hora <- parse_date_time(paste0(data$Fecha," ",data$Hora,":00"), "%d-%m-%Y %H:%M")
contaminantes_slim <- (data %>% 
                         select(fecha_hora, id_station, o3) %>% 
                         filter(wday(fecha_hora)>=2 & wday(fecha_hora)<=6 & hour(fecha_hora)>=6 & hour(fecha_hora)<=11))


table(contaminantes_slim$id_station)
elimina <- which(table(contaminantes_slim[which(is.na(contaminantes_slim$o3)),]$id_station)>50)
str(elimina)
elimina <- as.data.frame(elimina)
rownames(elimina)
contaminantes_slim_validstations <- filter(contaminantes_slim,!id_station %in% rownames(elimina))
View(contaminantes_slim_validstations)



################# USAR ESTE!!!!!!!!!!!!!!!!!!!!!!!
##########################################################################################################
#### Usando el paquete de Diego_Valle

# Download pm10 data since 2008 for all available zones ("TZ")
data <- get_zone_data(criterion = "HORARIOS", # Can be MAXIMOS (daily maximum) or 
                       # HORARIOS (hourly average)
                       pollutant = "TC", # "SO2", "CO", "NO2", "O3", "PM10", 
                       # "TC" (All pollutants)
                       zone = "CE", # "NO", "NE", "CE", "SO", "SE", "TZ" (All zones)
                       start_date = "2015-01-01", # Can't be earlier than 2008-01-01
                       end_date = "2015-12-31") # Can be up to the current date
head(data)
attach(contaminantes_slim)
unique(pollutant)
summary(data)
data <- spread(data,pollutant,value)
glimpse(data)
data <- data %>% mutate(hour = as.numeric(hour))
data$zone <- NULL
data$unit <- NULL
data$fecha_hora <- parse_date_time(paste0(data$date," ",data$hour,":00"), "%Y-%m-%d %H:%M")
contaminantes_slim <- (data %>% 
                         filter(wday(fecha_hora)>=2 & wday(fecha_hora)<=6 & hour(fecha_hora)>=6 & hour(fecha_hora)<=11))
head(contaminantes_slim)
contaminantes_slim <- contaminantes_slim[,c(8,1,2,5,6)]
plot(fecha_hora,PM10)

data <- get_zone_data(criterion = "MAXIMOS", # Can be MAXIMOS (daily maximum) or 
                      # HORARIOS (hourly average)
                      pollutant = "TC", # "SO2", "CO", "NO2", "O3", "PM10", 
                      # "TC" (All pollutants)
                      zone = "CE", # "NO", "NE", "CE", "SO", "SE", "TZ" (All zones)
                      start_date = "2015-01-01", # Can't be earlier than 2008-01-01
                      end_date = "2015-12-31") # Can be up to the current date
data <- spread(data,pollutant,value)
data$zone <- NULL
data$unit <- NULL
data$CO <- NULL
data$NO2 <- NULL
data$SO2 <- NULL
names(data) <- c("date","MAX_DIA_O3","MAX_DIA_PM10")
contaminantes_slim <- left_join(contaminantes_slim,data,by=c("date"="date"))

################################### METEOROLOGIA
setwd(paste0(path_to_project,"data/meteorologia/"))
dir()
zz=gzfile('meteorología_2015.csv.gz','rt')  
meteorologia=read.csv(zz,header=T,skip=10) 
head(meteorologia,20)
stations_of_interest <- c("HGM","MGH","BJU")
meteorologia <- filter(meteorologia,id_station %in% stations_of_interest)

glimpse(meteorologia)
meteorologia$unit <- NULL
meteorologia$date = as.character(meteorologia$date)
meteorologia$date <-  parse_date_time(meteorologia$date, "%d/%m/%Y %H:%M")
meteorologia <- filter(meteorologia,!is.na(date))


meteorologia_horario <- meteorologia %>% 
  group_by(date, id_parameter) %>% 
  summarise(prom = ifelse(all(is.na(value)),NA,base::mean(value, na.rm = TRUE))) 

meteorologia_horario <- meteorologia_horario %>% spread(id_parameter,prom)
meteorologia_horario <- (meteorologia_horario %>% 
                         filter(wday(date)>=2 & wday(date)<=6 & hour(date)>=6 & hour(date)<=11))

meteorologia_max <- meteorologia %>% 
  group_by(as_date(date), id_parameter) %>% 
  summarise(max = ifelse(all(is.na(value)),NA,base::max(value, na.rm = TRUE))) 
meteorologia_max <- meteorologia_max %>% spread(id_parameter,max)
head(meteorologia_max)
View(meteorologia_max)
names(meteorologia_max) <- c("fecha","MAX_DIA_RH","MAX_DIA_TMP","MAX_DIA_WDR","MAX_DIA_WSP")
head(contaminantes_slim)
final <- left_join(contaminantes_slim,meteorologia_horario,by=c("fecha_hora"="date"))
head(final)
final <- left_join(final,meteorologia_max,by=c("date"="fecha"))
write.table(final, "../ecobici_contaminantes_meteorologia_2015.csv", sep = ",", col.names = TRUE, row.names = FALSE)
