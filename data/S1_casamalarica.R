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
#¿por qué no es necesario agregar comillas?

#### Graficar distribución de distancias ####

# Crear histogramas de la distancia entre pares de casas,
# separados según si pertenecen a la misma comunidad o a comunidades diferentes
# geom_histogram() ->crea el histograma. El eje Y será el número de pares de casas 
#que caen en cada rango de distancia. 
#facet_wrap -> → divide el gráfico según la columna
ggplot(comparacion_entre_casas, aes(x = dist_ij)) +
  geom_histogram() +
  facet_wrap(~comparacion_comunidad)

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
  ) +
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
