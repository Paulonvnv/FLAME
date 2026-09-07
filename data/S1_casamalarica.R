### cargar paquetes ####
#manipular tablas
library(dplyr)

#Permitir el uso de %<>% para modificar y guardar el resultado en el mismo objeto
library(magrittr)

# reorganizar datos
library(tidyr)

# manipular texto
library(stringr)

#crear gráficos
library(ggplot2)

####lectura de archivo csv ####
# Importar la base de datos de casos de malaria por persona
control_malaria_cases_by_person=
  read.csv(
    file=
      "C:/Users/brend/OneDrive/Escritorio/GitHub/FLAME/data/analysis/control_malaria_cases_by_person.csv")

### Estandarizar identificadores ####
#modifiqué el objeto y luego lo guardé en el mismo objeto ####
#mutate -> crear o modificar columnas
# nchar -> contar el número de caracteres de cada valor
# case_when -> condicional, si ocurre "A" haz "B", pero si no ocurre "A", se deja tal cual
# paste0 -> pega texto sin espacios
control_malaria_cases_by_person%<>%mutate(
  base_ind_code=case_when(
    nchar(base_ind_code)==4~as.character(paste0(0,base_ind_code)),
    nchar(base_ind_code)==5~as.character(base_ind_code)))
control_malaria_cases_by_person%<>%mutate(
  unihh_p=case_when(
    nchar(unihh_p)==4~as.character(paste0(0,unihh_p)),
    nchar(unihh_p)==5~as.character(unihh_p)))

### Revisar identificadores faltantes ####
#filter -> selecionar filas que cumplen una condicion, que es comprobar NA
#filter(!is.na(date))-> elimina episodios donde no habia fecha
#select -> seleccionar solo esa columna
#unlist -> convierte esa columna en un vector sencillo
#duplicated -> detecta valores repetidos
#sum -> contar cuantos valores duplicados existen
control_malaria_cases_by_person%>%filter(is.na(base_ind_code))%>%select(unihh_p)%>%unlist()%>%
  duplicated()%>%sum()

#### Completar base_ind_code faltantes ####
# Si base_ind_code es NA, crear un código usando "c" + unihh_p.
# Si ya existe, mantener el valor original.
control_malaria_cases_by_person %<>%
  mutate(
    base_ind_code=
      case_when(
        is.na(base_ind_code) ~ paste0("c",unihh_p),
        .default = as.character(base_ind_code)))
### Obtener village ####
# Crear la variable village a partir de base_ind_code,
# eliminando la letra "c" inicial y los últimos tres dígitos.
#^c -> busca una c al inicio del código.
#|#-> #significa o
#\\d{3}$ → busca 3 números al final del código.
#gsub -> busca un patrón de texto y reemplazalo
control_malaria_cases_by_person%<>%mutate(village=gsub("(^c|\\d{3}$)","",base_ind_code))

#### Transformar episodios de malaria a formato largo ####
# Convertir las columnas de fechas de episodios en filas.
# episode identifica el episodio y date contiene su fecha.
# Se eliminan los episodios que no tienen fecha.
malaria_cases_long<-pivot_longer(control_malaria_cases_by_person,cols = all_of(ends_with("_date")),
                                 names_to =
                                   "episode",values_to = "date")%>%filter(!is.na(date))

####Revisar formato y precisión de las coordenadas ####
# Ver el tipo de dato de la columna geometry
# Mostrar valores numéricos y colocar cuantos quiero ver en la columna
class(malaria_cases_long$geometry)
options(digits = 16)

### Extraer longitud y latitud desde geometry ####
# Crear las columnas longitude y latitude a partir de la información
# almacenada en geometry.
# gsub() elimina las partes de texto que no corresponden a cada coordenada.
# as.numeric() convierte el resultado a formato numérico.
#^ -> inicio del texto
#\\( -> busca el paréntesis
#, → busca la coma
#. → cualquier carácter
#+ → uno o más caracteres
#$ → final del texto
malaria_cases_long%<>%
  mutate(longitude=as.numeric(gsub("(^c\\(|,.+$)","",geometry)),
         latitude=as.numeric(gsub("(^.+,|\\)$)","",geometry)))

### Eliminar registros sin identificador de casa ####
# Mantener solo los registros que tienen un valor en unihh_p.
malaria_cases_long%<>%filter(!is.na(unihh_p))

### Crear listado de casas únicas ####
# Obtener todos los identificadores únicos de casas.
# Eliminar posibles valores NA del listado.
listado_de_casas=unique(malaria_cases_long$unihh_p)
listado_de_casas=listado_de_casas[!is.na(listado_de_casas)]

### Seleccionar tres casas para probar el cálculo de distancias ####
casa1=listado_de_casas[1]
casa1
casa2
casa2=listado_de_casas[2]
casa3=listado_de_casas[3]

### Obtener coordenadas de las tres casas seleccionadas ####
longitud1=malaria_cases_long%>%filter(unihh_p==casa1)%>%select(longitude)%>%unlist()%>%unique()
latitud1=malaria_cases_long%>%filter(unihh_p==casa1)%>%select(latitude)%>%unlist()%>%unique()
longitud2=malaria_cases_long%>%filter(unihh_p==casa2)%>%select(longitude)%>%unlist()%>%unique()
latitud2=malaria_cases_long%>%filter(unihh_p==casa2)%>%select(latitude)%>%unlist()%>%unique()
longitud3=malaria_cases_long%>%filter(unihh_p==casa3)%>%select(longitude)%>%unlist()%>%unique()
latitud3=malaria_cases_long%>%filter(unihh_p==casa3)%>%select(latitude)%>%unlist()%>%unique()

### Cargar paquete para calcular distancias geográficas ####
install.packages("geosphere")
library(geosphere)

### Crear puntos geográficos ####
# Combinar longitud y latitud de cada casa en un solo punto.
point1=c(longitud1,latitud1)
point2=c(longitud2,latitud2)
point3=c(longitud3,latitud3)

### Probar cálculo de distancias entre puntos ####
distm(point1,point2,point3)
distHaversine(point1,point2,point3)

### Crear todas las combinaciones posibles de pares de casas ####
# combn() genera todas las combinaciones posibles de dos casas.
# t() transpone el resultado para que cada par quede en una fila.
# as.data.frame() convierte el resultado en una tabla.
#nombrar las columnas de cada casa
comparacion_entre_casas=as.data.frame(t(combn(listado_de_casas,2)))
colnames(comparacion_entre_casas)=c("casa_i","casa_j")

# Crear columna para almacenar distancias #
comparacion_entre_casas$dist_ij=NA

### Calcular la distancia entre cada par de casas ####
#for -> toma cada posible pareja de casas y calcula qué distancia 
#hay entre ellas
# Recorrer cada fila de comparacion_entre_casas.
# En cada interación:
# identificar las dos casas,
# obtener sus coordenadas,
# crear los puntos geográficos,
# calcular la distancia Haversine,
# y guardar el resultado en dist_ij.
for (n in 1:nrow(comparacion_entre_casas)) {
  casai=comparacion_entre_casas[n,][["casa_i"]]
  casaj=comparacion_entre_casas[n,][["casa_j"]]
  longitude_i=unique(malaria_cases_long[malaria_cases_long$unihh_p==casai,][["longitude"]])
  latitude_i=unique(malaria_cases_long[malaria_cases_long$unihh_p==casai,][["latitude"]])                                              
  longitude_j=unique(malaria_cases_long[malaria_cases_long$unihh_p==casaj,][["longitude"]])
  latitude_j=unique(malaria_cases_long[malaria_cases_long$unihh_p==casaj,][["latitude"]])  
  point_i=c(longitude_i,latitude_i)
  point_j=c(longitude_j,latitude_j)
  comparacion_entre_casas[n,][["dist_ij"]]=distHaversine(point_i,point_j) 
}
casai

### Agregar village correspondiente a casa_i y casa_j ####
# left_join -> une dos tablas manteniendo todas las filas de la tabla de la derecha
comparacion_entre_casas=left_join(comparacion_entre_casas,malaria_cases_long%>%select(unihh_p,village),
                                  by=join_by("casa_i"=="unihh_p"))
comparacion_entre_casas%<>%dplyr::rename("village_i"="village")
comparacion_entre_casas=left_join(comparacion_entre_casas,malaria_cases_long%>%select(unihh_p,village),
                                  by=join_by("casa_j"=="unihh_p"))
comparacion_entre_casas%<>%dplyr::rename("village_j"="village")

#### Comparar si las filas pertenecen a una misma comunidad ####
# Completar la columna comparacion_comunidad según si village_i y village_j
# pertenecen a la misma comunidad o a comunidades diferentes
# ~ -> ctrl+alt+simbolo
#^-> ctrl+alt+simbolo 
comparacion_entre_casas$comparacion_comunidad=NA
comparacion_entre_casas %<>%
  mutate(comparacion_comunidad = case_when(
    village_i == village_j ~ "misma comunidad",
    village_i != village_j ~ "otra comunidad"
  ))

#### Comparar distancias según comunidad ####

# filter-> solo con los pares de casas que pertenecen a la MISMA comunidad
# En el eje X ->  distancia entre cada par de casas
# El histograma agrupa las distancias en intervalos
# Separamos el gráfico por village
# village_i ~ . significa: una comunidad por FILA
#bindwith -> que tan ancho es el intervalo entre barras
# scales = "free_y" permite que cada comunidad tenga su propia escala en el eje Y
# algunas comunidades pueden tener muchos más pares de casas que otras
# Mostrar aproximadamente 3 números por escala del eje Y
#formato visual más limpio
## ~ significa organiza el gráfico según village i
## :: cargar una función sin cargar todo el paquete
plot1 = comparacion_entre_casas %>%
  filter(comparacion_comunidad == "misma comunidad") %>%
  ggplot(mapping = aes(x = dist_ij, fill = village_i)) +
  geom_histogram(binwidth = 200) + 
  facet_grid(village_i ~ ., scales = "free_y")+
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 3)
  ) +
  labs(
    x = "Distancia entre casas (metros)",
    y = "Pares de casas",
    title = "Distribución de distancias entre casas dentro de cada comunidad",
    fill = "Comunidad"
  ) 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
  
plot1 + 
  theme(axis.text.x = element_text(angle = 90))

#### Crear un mapa ####
install.packages("ggplot2")
library(ggplot2)
unique(malaria_cases_long$village)
ggplot(data =malaria_cases_long,aes(x=longitude,y=latitude, color = village)) +
  geom_point(pch=20,size=2)+ scale_color_manual(values=c("grey1",
                                                         "#838B8B",
                                                         "blue4",
                                                         "brown1",
                                                         "purple4",
                                                         "cadetblue4",
                                                         "magenta4",
                                                         "hotpink",
                                                         "chartreuse",
                                                         "cyan",
                                                         "coral4",
                                                         "orangered4",
                                                         "darkgoldenrod",
                                                         "darkorchid",
                                                         "darkolivegreen"))


install.packages(c("OpenStreetMap"))
library(OpenStreetMap)
upper_left=c(max(malaria_cases_long$latitude,na.rm=TRUE)+0.1,min(malaria_cases_long$longitude,na.rm=TRUE)-0.1)
lower_right=c(min(malaria_cases_long$latitude,na.rm=TRUE)-0.1,max(malaria_cases_long$longitude,na.rm=TRUE)+0.1)
#Download the static raster map tiles
map_raster <- openmap(upper_left, lower_right, type = "osm")
map_latlon <- openproj(map_raster,projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
autoplot(map_latlon)+geom_point(data=malaria_cases_long,mapping = aes(x=longitude,y=latitude,color = village)) +
  geom_point(pch=16,size=2)+ scale_color_manual(values=c("red",
                                                         "#838B8B",
                                                         "blue4",
                                                         "brown1",
                                                         "purple4",
                                                         "cadetblue4",
                                                         "magenta4",
                                                         "hotpink",
                                                         "chartreuse",
                                                         "cyan",
                                                         "coral4",
                                                         "orangered4",
                                                         "darkgoldenrod",
                                                         "darkorchid",
                                                         "darkolivegreen"))
#### Describir el tiempo entre episodios ####
# Contar cuántas muestras/episodios tiene cada individuo.
# .by = base_ind_code hace el resumen por individuo.
malaria_cases_long%>%
  summarise(n_samples=n(),.by = base_ind_code)%>%
  # Graficar la distribución del número de episodios por individuo.
  ggplot(mapping = aes(x=n_samples)) +
  geom_histogram()
# Revisar qué clase tiene la fecha.
# Aquí primero se convierte temporalmente a Date para comprobarlo.
class(as.Date(malaria_cases_long$date))
# Ejemplo: calcular la diferencia en días entre dos fechas específicas.
# Restar dos objetos Date devuelve una diferencia de tiempo.
as.Date(malaria_cases_long$date[1])-as.Date(malaria_cases_long$date[5])
# Convertir TODA la columna date al formato Date.
# %<>% significa:
# toma malaria_cases_long, aplica mutate() y guarda el resultado nuevamente
# en malaria_cases_long.
malaria_cases_long%<>%mutate(date=as.Date(date))
# Comprobar que la columna date quedó correctamente como Date.
class(malaria_cases_long$date)
#### Probar el cálculo para un solo individuo ####

# Obtener todos los individuos únicos y seleccionar el tercero
# como ejemplo para probar el código.
ind_i=unique(malaria_cases_long$base_ind_code)[3]

# Contar cuántas filas/episodios tiene ese individuo.
nrow(malaria_cases_long%>%filter(base_ind_code==ind_i))

# Guardar únicamente los registros de ese individuo.
# Calcular la diferencia de días entre episodios consecutivos.
data_i=malaria_cases_long%>%filter(base_ind_code==ind_i)
# elimina la PRIMERA fecha:
as.numeric(data_i[["date"]][-1]-
             # elimina la ÚLTIMA fecha:
data_i[["date"]][-nrow(data_i)])
# Crear una nueva columna.
# Recorrer uno por uno todos los individuos únicos
# Solo hacer el cálculo si el individuo tiene más de un episodio.
# Si solo tiene uno, no existe un episodio anterior con el cual comparar.
malaria_cases_long$time_to_previous_episode=0
for (ind_i in unique(malaria_cases_long$base_ind_code)){
  if(nrow(malaria_cases_long%>%filter(base_ind_code==ind_i))>1){
    # Seleccionar solamente los registros del individuo actual.
    data_i=malaria_cases_long%>%filter(base_ind_code==ind_i)
    # Guardar las diferencias de tiempo en la columna
    # time_to_previous_episode.
    # c(0, ...)
    # pone 0 para el primer episodio porque no tiene episodio previo.
    malaria_cases_long$time_to_previous_episode[
      malaria_cases_long$base_ind_code==ind_i]= 
      c(0,as.numeric(data_i[["date"]][-1]-
                 data_i[["date"]][-nrow(data_i)]))
  }}
#### Graficar tiempo entre episodios ####
# Seleccionar:
# 1. Episodios que NO sean el primer episodio.
# 2. Solo individuos del village 07.
# Histograma con intervalos de 1 día.
# Mostrar únicamente valores entre 0 y 40 días.
# Crear paneles separados por village.
# En este caso solo quedará village 07 porque ya lo filtramos arriba.
malaria_cases_long%>%filter(episode!="first_date",village=="07")%>%
  ggplot(mapping = aes(x=time_to_previous_episode)) + 
  geom_histogram(binwidth=1) + scale_x_continuous(limits = c(0,40)) +
  facet_grid(village~.)

# Ordenar primero la base por fecha.
# Esto es importante porque ayuda a mantener los episodios
# en orden cronológico.
#arrange() ordena filas
malaria_cases_long%<>%arrange(date)

### Crear listado de individuos únicos ####
listado_de_individuos=unique(
  malaria_cases_long$base_ind_code)

#### Probar la creación del código de muestra con un individuo ####

# Seleccionar el tercer individuo de la lista como ejemplo.
base_ind_code= listado_de_individuos[3]
# Buscar en qué posiciones de malaria_cases_long aparece este individuo.
grep(base_ind_code,malaria_cases_long$base_ind_code)
# Contar cuántas veces aparece el individuo.
n_episodes=length(grep(base_ind_code,malaria_cases_long$base_ind_code))
# Crear una secuencia
episodes=1:n_episodes
# Convertir los números de un solo dígito a dos dígitos
episodes=ifelse(nchar(episodes)==1,paste0(0,episodes),episodes)
# Crear un código único para cada muestra/episodio
# concatenando el código del individuo con el número de episodio.
malaria_cases_long[["cod_muestra"]][malaria_cases_long$base_ind_code==base_ind_code]=
  paste0(base_ind_code,episodes)

malaria_cases_long$cod_muestra=NA
# Recorrer cada individuo único.
for(base_ind_code in listado_de_individuos){
  # Contar cuántos episodios tiene el individuo.
  n_episodes=length(grep(base_ind_code,malaria_cases_long$base_ind_code))
  # Crear una secuencia desde 1 hasta el número total de episodios.
  episodes=1:n_episodes
  # Agregar un 0 delante de los episodios de un solo dígito.
  episodes=ifelse(nchar(episodes)==1,paste0(0,episodes),episodes)
  # Crear el código único de muestra:
  malaria_cases_long[["cod_muestra"]][malaria_cases_long$base_ind_code==base_ind_code]=
    paste0(base_ind_code,episodes)
}
  
### Crear todas las combinaciones posibles de cod_muestra ####

# Crear un vector que contiene todos los códigos de muestra.
listado_de_muestras=malaria_cases_long$cod_muestra
# Crear todas las combinaciones posibles de DOS muestras.
# t() transpone el resultado para que cada par sea una fila.
# as.data.frame() transforma el resultado en una tabla.
comparacion_entre_muestras=as.data.frame(t(combn(listado_de_muestras,2)))
colnames(comparacion_entre_muestras)=c("cod_i","cod_j")
# Hacer un left_join para recuperar la información original
# correspondiente a cod_i y cod_j

comparacion_entre_muestras=left_join(comparacion_entre_muestras,malaria_cases_long%>%
                                  select(cod_muestra,base_ind_code,village,date,longitude,
                                         latitude,unihh_p),
                                  by=join_by("cod_i"=="cod_muestra"))
comparacion_entre_muestras%<>%dplyr::rename("village_i"="village","base_ind_code_i"="base_ind_code",
                                       "date_i"="date","longitude_i"="longitude",
                                       "latitude_i"="latitude","casa_i"="unihh_p")
                                
comparacion_entre_muestras=left_join(comparacion_entre_muestras,malaria_cases_long%>%
                                       select(cod_muestra,base_ind_code,village,date,longitude,
                                              latitude,unihh_p),
                                     by=join_by("cod_j"=="cod_muestra"))
comparacion_entre_muestras%<>%dplyr::rename("village_j"="village","base_ind_code_j"="base_ind_code",
                                            "date_j"="date","longitude_j"="longitude",
                                            "latitude_j"="latitude","casa_j"="unihh_p")
# Revisar qué tipo de variable es date_i.
class(comparacion_entre_muestras$date_i)

# Filtrar la tabla para conservar únicamente pares de muestras
# donde ambas muestras tengan longitud disponible

comparacion_entre_muestras%<>%filter(!is.na(longitude_i),!is.na(longitude_j))
#### Probar el cálculo de distancia espacial ####

# Tomar solamente las primeras 40 comparaciones
# para probar si el cálculo funciona antes de aplicarlo a toda la base.
#reframe() sirve para crear un nuevo resumen por grupo, y a diferencia de summarise()
#, puede devolver más de una fila por grupo.
#Para cada combinación cod_i–cod_j, calcula algo y crea una nueva tabla con el resultado
comparacion_entre_muestras %>% head(40)%>%reframe(
  distancia_espacial=distHaversine(p1=c(longitude_i,latitude_i), 
                                   p2=c(longitude_j,latitude_j)),
  .by = c(cod_i, cod_j)
  )

#as.numeric() no sirve para juntar longitud y latitud.
#El problema principal era que estabas pasando columnas enteras 
#a una función que espera un par de coordenadas por punto.

comparacion_entre_muestras %>% head(10) %>% mutate(

  distancia_espacial=
  distHaversine(
    p1=as.numeric(longitude_i,latitude_i), 
    p2=as.numeric(longitude_j,latitude_j))
  
  )
#### *Calcular distancia espacial para cada par ####
#rowwise() = calcula fila por fila
#ungroup() = quita ese comportamiento especial
#cbind

comparacion_entre_muestras %<>%
  mutate(
    
    # Calcular distancia entre la coordenada de i
    # y la coordenada de j.
    # Resultado: metros.
    distancia_espacial = distHaversine(
      p1 = cbind(longitude_i, latitude_i),
      p2 = cbind(longitude_j, latitude_j)
    ))
  

#### Revisar coordenadas faltantes ####
# Contar cuántos NA existen todavía en latitude_i.
sum(is.na(comparacion_entre_muestras$latitude_i))

#### Calcular distancia temporal entre muestras ####
# Crear una nueva columna llamada distancia_temporal.
comparacion_entre_muestras%<>%mutate(distancia_temporal=as.numeric(date_j-date_i))

#rm() elimina objetos del Environment de R, no columnas de un dataframe.
rm(list = c("longitude_i","longitude_j","latitude_i","latitude_j"))

#Crear columna para almacenar distancias 
comparacion_entre_ind$time_since_previous_episode=NA

comparacion_entre_muestras%<>%mutate(rhat=case_when(distancia_temporal>=16 & distancia_temporal<=28 &
                                                              distancia_espacial<=200~1,.default=0))
source("C:/Users/brend/OneDrive/Escritorio/GitHub/FLAME/funciones.R")

malaria_cases_long%<>%filter(!is.na(longitude))

comparacion_entre_muestrasmatriz=long2wide_relatedness(comparacion_entre_muestras,
                                                       malaria_cases_long,id = "cod_muestra",
                                                      id_i = "cod_i",id_j = "cod_j",var = "rhat")
write.csv(x=comparacion_entre_muestrasmatriz,file = "comparacion_entre_muestras_matriz.csv",quote = FALSE,
          row.names = TRUE)

comparacion_entre_muestras_matriz <- read.csv("comparacion_entre_muestras_matriz.csv")
View(comparacion_entre_muestras_matriz)

nombre_fila=comparacion_entre_muestras_matriz$X
nombre_columna=colnames(comparacion_entre_muestras_matriz)[-1]
nombre_columna=gsub("^X","",nombre_columna)
comparacion_entre_muestras_matriz=as.matrix(comparacion_entre_muestras_matriz[,-1])
dimnames(comparacion_entre_muestras_matriz)=list(nombre_fila,nombre_columna)

set.seed(500)
plot3_network=plot_ggnetwork(pairwise_relatedness_matrix = comparacion_entre_muestras_matriz,malaria_cases_long,
                             id="cod_muestra",color_by = "village",palette ="AUTO",shape_by =NULL,shape_levels =
                               NULL,
                             alpha_by = NULL,mode = "fruchtermanreingold",directed = TRUE)

plot3_network$plot_network

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("S4Vectors")
library(S4Vectors)

test1=get_network_clusters(pairwise_relatedness = comparacion_entre_muestras,variable = "rhat",threshold = 1,
                     cols = c("cod_i","cod_j"),rhat_formula = NULL,metadata = malaria_cases_long,
                      sample_id = "cod_muestra"
        )
View(test1$clusters)
