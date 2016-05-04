# DataArchitecture_Ecobici

Proyecto para la clase de Métodos de Gran Escala ITAM

El repositorio tiene como objetivo desarrollar la infraestructura de datos para el proyecto Ecobici de la Cd. de México.

## Contenido del Repo

### Data

* Ecobici - Informacion sobre el sistema ecobici
  * Estaciones - Informacion sobre cicloestaciones
    * Ubicacion Ecobicis
    * Distancia entre las cicloestaciones a vuelo de pajaro
    * Capacidad maxima en bicis de cada cicloestacion
  * Viajes - Informacion sobre viajes mensuales 
    * A nivel transaccion(viaje), estaciones origen destino, fecha y hora de salida y llegada, identificador de bicicleta, edad y genero registrado en la tarjeta de usuario. 
 * Meteorologico
 Se puede 
   * Ozono
   * PM 10
   * Precipitacion

### Code

  * Ingesta - Aqui se guarda todo el codigo que obtiene la informacion
   * capacidad maxima - usa el API ecobici para derivar la capacidad maxima de cada cicloestacion
   * ubicacion de ecobicis - usa el API ecobici para obtener caracteristicas de las cicloestaciones como direccion, coordenadas geograficas. 
   * distancia entre cicloestaciones - Se uso QGIS para obtener las distancias, no esta automatizado, se podria obtener con cualquier aplicacion que permita realizar queries espaciales (PostGIS on Postgres por ejemplo).
  * Pipeline
   * Scraping
La data scrapeda debe ir en una carpeta /data/raw de un data lake (HDFS/S3), hay un flume ligado a esa carpeta y cuando detecta un cambio manda los datos al pipeline bash y al pipeline streaming ()
    * El scraping de la disponibilidad instantanea debe correr como deamon (e.g. cada minuto) luigi coordina?
    * El scraping de los catalogos se puede hacer una vez al mes
    * El scraping de los viajes se realiza una vez al mes
   * Cleaning
   * Transform
     * Calcular duracion del viaje y distancia 
     * Agregar viajes a nivel ciloestacion / hora - mantener proporcion de genero y de edad  
   * EDA
    * Aqui se genera la informacion para el reporteador (shiny), esto al final debe de vivir en un staging database desde donde shiny presentara la informacion (Postgres?). 
     * Numero total de viajes realizado en los ultimos 14 dias
     * Top 10 en viajes (salidas y llegadas) de las estaciones ayer y de los ultimos 14 dias (Stack Bar graph mostrando llegadas y salidas)
     * Bottom 10 en viajes (salidas y llegadas) de las estaciones ayer y de los ultimos 14 dias (Stack Bar graph mostrando llegadas y salidas)
     * Proporcion de genero en la red de los ultimos 14 dias
     * Proporcion de edades en la red de los ultimos 14 dias
     * Duracion de viaje promedio en la red, la maxima y la minima de los ultimos 14 dias
     * Precipitacion
     * Contaminacion - IMECAS
  * Modelos / Machine Learning 
   * Gibbs Sampler
    * Terminar de definir las covariables 
   * Interpolador Kriging - toma las estaciones de monitoreo atmosferico que decidamos y genera un raster / grid 
   * Generacion de regiones - calcular la matriz de pertenencia a regiones (cluster) y la matriz de contiguidad de la matriz
  * Salidas de sistema / Visualización
   * Pantalla en shiny para mostrar descriptivos 
   * Aviso de proxima saturación (cuando la probabilidad de pasar de 3 a 4 pasa de un cierto rango se dispara la alerta)



### Docs

#### Clima

![alt text](https://blog.diegovalle.net/images/posts/hoyodesmog/tufte_o3.png "Logo Title Text 1")


![alt text](https://blog.diegovalle.net/images/posts/hoyodesmog/peak_hours.png "Logo Title Text 1")




