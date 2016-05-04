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
   * Cleaning
   * Transform
    * Calcular duracion del viaje y distancia 
    * Agregar viajes a nivel ciloestacion / hora - mantener proporcion de genero y de edad  
   * EDA
   * 

### Docs

