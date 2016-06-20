# DataArchitecture_Ecobici

Proyecto para la clase de Estadística Multivariada ITAM

El repositorio tiene como objetivo desarrollar un modelo de predicción de flujos de las estaciones de Ecobici de la Cd. de México.

## Contenido del Repo

### Data

* Ecobici - Información sobre el sistema Ecobici
  * Estaciones - Información sobre cicloestaciones
    * Ubicación Ecobicis
    * Distancia entre las cicloestaciones a vuelo de pajaro
    * Capacidad máxima en bicis de cada cicloestacion
  * Viajes - Información sobre viajes mensuales 
    * A nivel transaccion(viaje), estaciones origen destino, fecha y hora de salida y llegada, identificador de bicicleta, edad y genero registrado en la tarjeta de usuario. 
 * Meteorológico
 Se puede 
   * Ozono
   * PM 10
   * Precipitacion

### Code

  * Ingesta - Aqui se guarda todo el codigo que obtiene la informacion
   * capacidad máxima - usa el API ecobici para derivar la capacidad maxima de cada cicloestacion
   * ubicacion de ecobicis - usa el API ecobici para obtener caracteristicas de las cicloestaciones como direccion, coordenadas geograficas. 
   * distancia entre cicloestaciones - Se uso QGIS para obtener las distancias, no esta automatizado, se podria obtener con cualquier aplicacion que permita realizar queries espaciales (PostGIS on Postgres por ejemplo).










