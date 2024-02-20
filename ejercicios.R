# Métodos y Simulación Estadística
# Unidad 01
# Ejercicios


#Notas. se debe actualizar el Rtools.

install.packages("learnr")          # solo una vez
install.packages("devtools")      # solo una vez


devtools::install_github("dgonxalex80/paqueteMETODOS") #descarga paquete paqueteMETODOS
learnr::run_tutorial("Tutorial101", "paqueteMETODOS")  # carga Tutorial101
learnr::run_tutorial("Tutorial102", "paqueteMETODOS")  # carga Tutorial102
learnr::run_tutorial("Tutorial103", "paqueteMETODOS")  # carga Tutorial103

require(paqueteMETODOS)
require(dplyr)
require(stringi)
require(sf)

data("vivienda_faltantes")
View(vivienda_faltantes)

colnames(vivienda_faltantes)
colnames(vivienda_faltantes)<-c("id", 
                            "Zona",
                            "No.Pisos", 
                            "Estrato", 
                            "Precio",   
                            "Area_Construida", 
                            "No.Parqueaderos",   
                            "No.Baños",    
                            "No.Habitaciones",   
                            "Tipo.Vivienda",      
                            "Barrio",    
                            "Longitud",  
                            "Latitud" 
)

datos_Vivienda<-vivienda_faltantes
dim(datos_Vivienda)
colnames(datos_Vivienda)<-c("id", 
                            "Zona",
                            "No.Pisos", 
                            "Estrato", 
                            "Precio",   
                            "Area_Construida", 
                            "No.Parqueaderos",   
                            "No.Baños",    
                            "No.Habitaciones",   
                            "Tipo.Vivienda",      
                            "Barrio",    
                            "Longitud",  
                            "Latitud" 
                            )
View(datos_Vivienda)

class(datos_Vivienda$id)
class(datos_Vivienda$Zona)
class(datos_Vivienda$No.Pisos)
class(datos_Vivienda$Estrato)
class(datos_Vivienda$Precio)
class(datos_Vivienda$Area_Construida)
class(datos_Vivienda$No.Parqueaderos)
class(datos_Vivienda$No.Baños)
class(datos_Vivienda$No.Habitaciones)
class(datos_Vivienda$Tipo.Vivienda)
class(datos_Vivienda$Barrio)
class(datos_Vivienda$Longitud)
class(datos_Vivienda$Latitud)

levels(as.factor(vivienda_faltantes$Barrio))



datos_Vivienda$Zona<-as.factor(datos_Vivienda$Zona)
datos_Vivienda$No.Pisos<-as.factor(datos_Vivienda$No.Pisos)
datos_Vivienda$Estrato<-as.factor(datos_Vivienda$Estrato)
datos_Vivienda$No.Parqueaderos<-as.factor(datos_Vivienda$No.Parqueaderos)
datos_Vivienda$No.Baños<-as.factor(datos_Vivienda$No.Baños)
datos_Vivienda$No.Habitaciones<-as.factor(datos_Vivienda$No.Habitaciones)
datos_Vivienda$Tipo.Vivienda<-as.factor(datos_Vivienda$Tipo.Vivienda)
as.factor(datos_Vivienda$Barrio)

#Corrección de la Longitud####

for(i in 1:length(datos_Vivienda$Longitud)){
  if(is.na(datos_Vivienda$Longitud[i]) == "FALSE"){
    if(datos_Vivienda$Longitud[i]<(-100)){
      datos_Vivienda$Longitud[i] = datos_Vivienda$Longitud[i]/1000
    }
  }
}

for(i in 1:length(datos_Vivienda$Latitud)){
  if(is.na(datos_Vivienda$Latitud[i]) == "FALSE"){
    if(datos_Vivienda$Latitud[i]>10){
      datos_Vivienda$Latitud[i] = datos_Vivienda$Latitud[i]/1000
    }
  }
}
      
#Corrección de tipo de vivienda####           


datos_Vivienda$Tipo.Vivienda<-dplyr::recode_factor(datos_Vivienda$Tipo.Vivienda,
                                             "APARTAMENTO" = "Apartamento",
                                             "apto" = "Apartamento", 
                                             "casa" = "Casa",
                                             "CASA" = "Casa")

Cali_WGS84<-sf::st_read("./sig/Cali_WGS84.gpkg", layer="Barrios")


Cali_WGS84$Zonas_IDE<-dplyr::recode_factor(Cali_WGS84$comuna,
                                           "01" = "Zona Oeste",
                                           "02" = "Zona Oeste",
                                           "03" = "Zona Centro",
                                           "04" = "Zona Norte",
                                           "05" = "Zona Norte",
                                           "06" = "Zona Norte",
                                           "07" = "Zona Norte",
                                           "08" = "Zona Norte",
                                           "09" = "Zona Centro",
                                           "10" = "Zona Sur",
                                           "11" = "Zona Oriente",
                                           "12" = "Zona Oriente",
                                           "13" = "Zona Oriente",
                                           "14" = "Zona Oriente",
                                           "15" = "Zona Oriente",
                                           "16" = "Zona Oriente",
                                           "17" = "Zona Sur",
                                           "18" = "Zona Sur",
                                           "19" = "Zona Sur",
                                           "20" = "Zona Sur",
                                           "21" = "Zona Oriente",
                                           "22" = "Zona Sur")
                                           



datos_Vivienda$Barrio<-toupper(stringi::stri_trans_general(datos_Vivienda$Barrio,"Latin-ASCII"))
levels(as.factor(datos_Vivienda$Barrio))

lista_Barrios<-vector("list", length = 2)

lista_Barrios[[1]]<-as.list(levels(as.factor(datos_Vivienda$Barrio)))
lista_Barrios[[2]]<-as.list(levels(as.factor(Cali_WGS84$barrio)))
names(lista_Barrios) <- c('Originales','IDE')

intersect(lista_Barrios[[1]], lista_Barrios[[2]])
setdiff(lista_Barrios[[1]], lista_Barrios[[2]])

lista_Barrios[[3]]<-as.list(toupper(stringi::stri_trans_general(lista_Barrios[[1]],"Latin-ASCII")))
lista_Barrios[[4]]<-as.list(toupper(stringi::stri_trans_general(lista_Barrios[[2]],"Latin-ASCII")))
names(lista_Barrios) <- c('Originales','IDE', 'ORG_homo', 'IDE_homo')


intersect(lista_Barrios[[3]], lista_Barrios[[4]])
setdiff(lista_Barrios[[3]], lista_Barrios[[4]])



datos_Vivienda$Barrio_Clean<-dplyr::recode_factor(datos_Vivienda$Barrio,
                                                   "ACOPI" = "BRISAS DE LOS ALAMOS",
                                                   "AGUA BLANCA" = "AGUABLANCA", 
                                                   "ALAMOS" = "CIUDAD DE LOS ALAMOS",
                                                   "ALBORADA" = "LA ALBORADA",
                                                  "ALCAZARES" = "LOS ALCAZARES",
                                                  "ALF√(C)REZ REAL" = "ALFEREZ REAL",
                                                  "ALFONSO LOPEZ" = "ALFONSO LOPEZ P. 1A ETAPA",
                                                  "ALFONSO LOPEZ I" = "ALFONSO LOPEZ P. 1A ETAPA",
                                                  "ALTO JORDAN" = "SECTOR ALTO JORDAN",
                                                  "ALTOS DE GUADALUPE" = "SECTOR CANAVERALEJO GUADALUPE",
                                                  "ALTOS DE SANTA" = "SECTOR ALTOS DE SANTA ISABEL",
                                                  "ALAMEDA DEL RIO" = "CIUDAD DE LOS ALAMOS",
                                                  "ARBOLEDA" = "ARBOLEDAS",
                                                  "ARBOLEDA CAMPESTRE CANDELARIA" = "FUERA DE CALI",
                                                  "AUTOPISTA SUR" = "FUERA DE CALI",
                                                  "BAJO AGUACATAL" = "AGUACATAL",
                                                  "BARRANQUILLA" = "LOS PARQUES BARRANQUILLA",
                                                  "BARRIO 7DE AGOSTO" = "SIETE DE AGOSTO",
                                                  "BARRIO EL RECUERDO" = "EL RECUERDO",
                                                  "BARRIO EUCARISTICO" = "EUCARISTICO",
                                                  "BARRIO TRANQUILO Y" = "FUERA DE CALI",
                                                  "BASE A√(C)REA" = "BASE AEREA",
                                                  "BELLA SUIZA" = "CANAVERAL",
                                                  "BELLA SUIZA ALTA" = "CANAVERAL",
                                                  "BERLIN" = "SULTANA - BERLIN - SAN FRANCISCO",
                                                  "BLOQUES DEL LIMONAR" = "EL LIMONAR",
                                                  "BOCHALEMA" = "BOCHALEMA",
                                                  "CIUDAD BOCHALEMA" = "BOCHALEMA",
                                                  "BOSQUES DE ALBOLEDA" = "SANTA TERESITA",
                                                  "BOYACA" = "URBANIZACION BOYACA",
                                                  "BRISAS DE GUADALUPE" = "SECTOR CANAVERALEJO GUADALUPE",
                                                  "BRISAS DE LOS" = "BRISAS DE LOS ALAMOS",
                                                  "BRISAS DEL GUABITO" = "VILLA DEL PRADO - EL GUABITO",
                                                  "BUENO MADRID" = "UNIDAD RESIDENCIAL BUENO MADRID",
                                                  "CALI" = "FUERA DE CALI",
                                                  "CALI BELLA" = "FEPICOL",
                                                  "CALI CANTO" = "LILI",
                                                  "CALIBELLA" = "FEPICOL",
                                                  "CALICANTO" = "LILI",
                                                  "CALICANTO VIII" = "LILI",
                                                  "CALIMIO NORTE" = "URBANIZACION CALIMIO",
                                                  "CAMBULOS" = "LOS CAMBULOS",
                                                  "CAMINO REAL" = "CAMINO REAL - LOS FUNDADORES",
                                                  "CAMPESTRE" = "CLUB CAMPESTRE",
                                                  "CANEY ESPECIAL" = "CANEY",
                                                  "CANASGORDAS" = "URBANIZACION RIO LILI",
                                                  "CANAVERALEJO" = "CANAVERALEJO - SEGUROS COLPATRIA",
                                                  "CANAVERALES" = "CANAVERALES - LOS SAMANES",
                                                  "CANAVERALES LOS SAMANES" = "CANAVERALES - LOS SAMANES",
                                                  "CAPRI" = "CIUDAD CAPRI",
                                                  "CASCAJAL" = "FUERA DE CALI",
                                                  "CATAYA REAL" = "PARCELACIONES PANCE",
                                                  "CEIBAS" = "LAS CEIBAS",
                                                  "CENTELSA" = "MENGA",
                                                  "CENTRO" = "FUERA DE CALI",
                                                  "CERRO CRISTALES" = "SECTOR ALTOS DE SANTA ISABEL",
                                                  "CERROS DE GUADALUPE" = "SECTOR CANAVERALEJO GUADALUPE",
                                                  "CHIMINANGOS" = "CHIMINANGOS I",
                                                  "CHIMINANGOS 1 ETAPA" = "CHIMINANGOS I",
                                                  "CHIMINANGOS 2 ETAPA" = "CHIMINANGOS II",
                                                  "CIUDAD ANTEJARDIN" = "URBANIZACION CIUDAD JARDIN",
                                                  "CIUDAD CORDOBA RESERVADO" = "CIUDAD CORDOBA",
                                                  "CIUDAD COUNTRY" = "FUERA DE CALI",
                                                  "CIUDAD DEL CAMPO" = "FUERA DE CALI",
                                                  "CIUDAD JARDIN" = "URBANIZACION CIUDAD JARDIN",
                                                  "CIUDAD JARDIN PANCE" = "PARCELACIONES PANCE",
                                                  "CIUDAD LOS ALAMOS" = "CIUDAD DE LOS ALAMOS",
                                                  "CIUDAD MEL√(C)NDEZ" = "MELENDEZ",
                                                  "CIUDAD MELENDEZ" = "MELENDEZ",
                                                  "CIUDAD MODELO" = "LOS SAUCES",
                                                  "CIUDAD PACIFICA" = "FUERA DE CALI",
                                                  "CIUDAD REAL" = "FUERA DE CALI",
                                                  "CIUDADELA DEL RIO" = "CIUDADELA DEL RIO - CVC",
                                                  "CIUDADELA MELENDEZ" = "SECTOR MELENDEZ",
                                                  "CIUDADELA PASO ANCHO" = "CIUDADELA PASOANCHO",
                                                  "COLINAS DE MENGA" = "ALTOS DE MENGA",
                                                  "COLINAS DEL BOSQUE" = "EL BOSQUE",
                                                  "COLON" = "CRISTOBAL COLON",
                                                  "COLSEGUROS" = "COLSEGUROS ANDES",
                                                  "COMFENALCO" = "PASO DEL COMERCIO",
                                                  "CONJUNTO GIBRALTAR" = "EL TRONCAL",
                                                  "CRISTALES" = "TEJARES - CRISTALES",
                                                  "CUARTO DE LEGUA" = "CUARTO DE LEGUA - GUADALUPE",
                                                  "ED BENJAMIN HERRERA" = "SAN PEDRO",
                                                  "EL CANEY" = "CANEY",
                                                  "EL CASTILLO" = "PANAMERICANO",
                                                  "EL GUABITO" = "VILLA DEL PRADO - EL GUABITO",
                                                  "EL INGENIO 3" = "EL INGENIO",
                                                  "EL INGENIO I" = "EL INGENIO",
                                                  "EL INGENIO II" = "EL INGENIO",
                                                  "EL INGENIO III" = "EL INGENIO",
                                                  "EL TR√(C)BOL" = "EL TREBOL",
                                                  "FARRALLONES DE PANCE" = "PARCELACIONES PANCE",
                                                  "FLORA" = "LA FLORA",
                                                  "FLORALIA" = "CIUDADELA FLORALIA",
                                                  "FUENTES DE LA" = "FUERA DE CALI",
                                                  "GAITAN" = "JORGE ELIECER GAITAN",
                                                  "GRAN LIMONAR" = "EL GRAN LIMONAR",
                                                  "GUADALUPE" = "CUARTO DE LEGUA - GUADALUPE",
                                                  "GUADALUPE ALTO" = "SECTOR CANAVERALEJO GUADALUPE",
                                                  "GUADUALES" = "LOS GUADUALES",
                                                  "HACIENDA ALFEREZ REAL" = "ALFEREZ REAL",
                                                  "INGENIO" = "EL INGENIO",
                                                  "INGENIO I" = "EL INGENIO",
                                                  "INGENIO II" = "EL INGENIO",
                                                  "JAMUNDI" = "FUERA DE CALI",
                                                  "JAMUNDI ALFAGUARA" = "FUERA DE CALI",
                                                  "JOSE MANUEL MARROQUIN" = "JOSE MANUEL MARROQUIN I",
                                                  "JUANAMB√∫" = "JUANAMBU",
                                                  "LA ARBOLEDA" = "ARBOLEDAS",
                                                  "LA BUITRERA" = "FUERA DE CALI",
                                                  "LA CEIBAS" = "LAS CEIBAS",
                                                  "LA LUISA" = "SECTOR ALTO LOS CHORROS",
                                                  "LA MORADA" = "FUERA DE CALI",
                                                  "LA NUEVA BASE" = "URBANIZACION LA NUEVA BASE",
                                                  "LA PORTADA AL" = "SANTA TERESITA",
                                                  "LA PRIMAVERA" = "PRIMAVERA",
                                                  "LA REFORMA" = "FUERA DE CALI",
                                                  "LA RIVERA" = "LA RIVERA I",
                                                  "LA RIVERA II" = "LA RIVERA I",
                                                  "LA RIVERITA" = "PARCELACIONES PANCE",
                                                  "LA RIVIERA" = "LOS ANDES B - LA RIVIERA",
                                                  "LA VILLA DEL" = "FUERA DE CALI",
                                                  "LAFLORA" = "LA FLORA",
                                                  "LARES DE COMFENALCO" = "SECTOR PUENTE DEL COMERCIO",
                                                  "LAS AM√(C)RICAS" = "LAS AMERICAS",
                                                  "LAS CAMELIAS" = "CANAVERALES - LOS SAMANES",
                                                  "LAS QUINTAS DE" = "LAS QUINTAS DE DON SIMON",
                                                  "LAS VEGAS" = "MAYAPAN - LAS VEGAS",
                                                  "LAS VEGAS DE" = "EL CANEY",
                                                  "LIBERTADORES" = "LOS LIBERTADORES",
                                                  "LOS ALAMOS" = "CIUDAD LOS ALAMOS",
                                                  "LOS CRISTALES" = "TEJARES - CRISTALES",
                                                  "LOS CRISTALES CLUB" = "ALTOS DE SANTA ISABEL",
                                                  "LOS JOCKEYS" = "URBANIZACION CIUDAD JARDIN",
                                                  "MAMELLAN" = "BELLAVISTA",
                                                  "MAYAPAN LAS VEGAS" = "MAYAPAN - LAS VEGAS",
                                                  "MEL√(C)NDEZ" = "MELENDEZ",
                                                  "MIRADOL DEL AGUACATAL" = "AGUACATAL",
                                                  "MORICHAL DE COMFANDI" = "SECTOR ALTO JORDAN",
                                                  "MULTICENTRO" = "UNICENTRO CALI",
                                                  "NORMANDIA WEST POINT" = "NORMANDIA",
                                                  "NORTE" = "PRADOS DEL NORTE",
                                                  "NORTE LA FLORA" = "LA FLORA",
                                                  "NUEVA BASE" = "URBANIZACION NUEVA BASE",
                                                  "OASIS DE COMFANDI" = "LOS GUADUALES",
                                                  "OASIS DE PASOANCHO" = "COLSEGUROS ANDES",
                                                  "OCCIDENTE" = "FUERA DE CALI",
                                                  "PACARA" = "LA MERCED",
                                                  "PALMAS DEL INGENIO" = "EL INGENIO",
                                                  "PAMPALINDA" = "PAMPA LINDA",
                                                  "PANCE" = "PARCELACIONES PANCE",
                                                  "PARQUE RESIDENCIAL EL" = "EL BOSQUE",
                                                  "POBLADO CAMPESTRE" = "FUERA DE CALI",
                                                  "PONCE" = "PARCELACIONES PANCE",
                                                  "PORTADA DE COMFANDI" = "LOS ALCAZARES",
                                                  "PORTALES DE COMFANDI" = "FUERA DE CALI",
                                                  "PUENTE DEL COMERCIO" = "SECTOR PUENTE DEL COMERCIO",
                                                  "PUENTE PALMA" = "CUARTO DE LEGUA - GUADALUPE",
                                                  "PASEO DE LOS" = "LOS PARQUES BARRANQUILLA",
                                                  "QUINTAS DE DON" = "LAS QUINTAS DE DON SIMON",
                                                  "QUINTAS DE SALOMIA" = "JORGE ELIECER GAITAN",
                                                  "REFUGIO" = "EL REFUGIO",
                                                  "REP√∫BLICA DE ISRAEL" = "REPUBLICA DE ISRAEL",
                                                  "RINCON DE LA" = "UNIDAD RESIDENCIAL BUENO MADRID",
                                                  "RINCON DE SALOMIA" = "SALOMIA",
                                                  "RIVERAS DEL VALLE" = "EL CANEY",
                                                  "ROZO LA TORRE" = "FUERA DE CALI",
                                                  "SAMANES" = "CANAVERALES - LOS SAMANES",
                                                  "SAMANES DE GUADALUPE" = "CANAVERALES - LOS SAMANES",
                                                  "SAMECO" = "URBANIZACION LA FLORA",
                                                  "SAN BOSCO" = "SAN JUAN BOSCO",
                                                  "SAN FERNANDO" = "SAN FERNANDO NUEVO",
                                                  "SAN JOAQUIN" = "URBANIZACION SAN JOAQUIN",
                                                  "SAN JUDAS" = "SAN JUDAS TADEO I",
                                                  "SAN JUDAS TADEO" = "SAN JUDAS TADEO II",
                                                  "SANTA" = "SANTA TERESITA",
                                                  "SANTA ANITA" = "SANTA ANITA - LA SELVA",
                                                  "SANTA ANITA SUR" = "SANTA ANITA - LA SELVA",
                                                  "SANTA HELENA DE" = "SANTA ELENA",
                                                  "SANTA MONICA ALTA" = "SANTA MONICA",
                                                  "SANTA MONICA NORTE" = "SANTA MONICA",
                                                  "SANTA MONICA RESIDENCIAL" = "SANTA MONICA",
                                                  "SANTAFE" = "SANTA FE",
                                                  "SECTOR AGUACATAL" = "AGUACATAL",
                                                  "SEMINARIO" = "PAMPA LINDA",
                                                  "SIERRAS DE NORMANDIA" = "NORMANDIA",
                                                  "TEJARES CRISTALES" = "TEJARES - CRISTALES",
                                                  "TEJARES DE SAN" = "MIRAFLORES",
                                                  "TEMPLETE" = "EUCARISTICO",
                                                  "TEQUENDAMA" = "URBANIZACION TEQUENDAMA",
                                                  "TEQUENDEMA" = "NUEVA TEQUENDAMA",
                                                  "UNION DE VIVIENDA" = "UNION DE VIVIENDA POPULAR",
                                                  "URBANIZACION BARRANQUILLA" = "LOS PARQUES BARRANQUILLA",
                                                  "URBANIZACION EL SAMAN" = "CANAVERALES - LOS SAMANES",
                                                  "URBANIZACION GRATAMIRA" = "LA SELVA",
                                                  "URBANIZACION LA NUEVA" = "NUEVA FLORESTA",
                                                  "URBANIZACION LAS CASCADAS" = "CUARTO DE LEGUA - GUADALUPE",
                                                  "URBANIZACION LILI" = "URBANIZACION RIO LILI",
                                                  "URBANIZACION PACARA" = "CIUDAD LOS ALAMOS",
                                                  "VALLE DE LILI" = "LILI",
                                                  "VALLE DEL LILI" = "LILI",
                                                  "VILLA DE VERACRUZ" = "SALOMIA",
                                                  "VILLA DEL PARQUE" = "LOS PARQUES BARRANQUILLA",
                                                  "VILLA DEL PRADO" = "VILLA DEL PRADO - EL GUABITO",
                                                  "VILLAS DE VERACRUZ" = "SALOMIA",
                                                  "ZONA CENTRO" = "FUERA DE CALI",
                                                  "ZONA NORTE" = "FUERA DE CALI",
                                                  "ZONA NORTE LOS" = "FUERA DE CALI",
                                                  "ZONA OESTE" = "FUERA DE CALI",
                                                  "ZONA ORIENTE" = "FUERA DE CALI",
                                                  "ZONA RESIDENCIAL" = "FUERA DE CALI",
                                                  "ZONA SUR" = "FUERA DE CALI"
)



datos_Vivienda$Barrio_Clean<-as.factor(datos_Vivienda$Barrio_Clean)

datos_Vivienda<- datos_Vivienda %>% subset(Barrio_Clean != "FUERA DE CALI" | Barrio_Clean != NA)


write.csv2(datos_Vivienda, "datos_Vivienda.csv")



#####Espacial Total####

Zonas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Zonas')
Comunas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Comunas')
Barrios<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Barrios')

require(leaflet)

leaflet::leaflet()%>%
  addTiles() %>%  # Agrega tiles de OpenStreetMap
  # Agrega el objeto sf al mapa
  addPolygons(data = Zonas)


Cali_WGS84$Barrio_Clean<-toupper(stringi::stri_trans_general(Cali_WGS84$barrio,"Latin-ASCII"))

#Convertir la tabla de viviendas en un objeto espacial

datos_Vivienda_sin.na<- subset(datos_Vivienda, !is.na(Longitud) & !is.na(Latitud))
Viviendas_Espacial <- sf::st_as_sf(datos_Vivienda_sin.na, coords = c("Longitud", "Latitud"))
Viviendas_Espacial
Viviendas_Espacial$Barrio_Clean<-as.factor(Viviendas_Espacial$Barrio_Clean)
crs <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
Viviendas_Espacial <- sf::st_set_crs(Viviendas_Espacial, crs)


#Union de capas
datos_join <- dplyr::left_join(Viviendas_Espacial, as.data.frame(Cali_WGS84), by = "Barrio_Clean")
zonas_join <- dplyr::left_join(Cali_WGS84,as.data.frame(Viviendas_Espacial), by = "Barrio_Clean")





#Imputacion por datos originales
for(i in 1:length(datos_join$Zonas_IDE)){
if(is.na(datos_join$Zonas_IDE[i])== "TRUE"){
  datos_join$Zonas_IDE[i] = datos_join$Zona[i]
  }
}

summary(datos_join$Zonas_IDE)

as.data.frame(datos_join[,c(1:12, 14:19,21)])



Barrios_precio<-datos_join%>%group_by(Barrio_Clean)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

Comuna_precio<-datos_join%>%group_by(comuna)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

Zona_precio<-datos_join%>%group_by(Zonas_IDE)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

boxplot(Zona_precio$mediana)



Barrios<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Barrios')
Barrios$Barrio_Clean<-toupper(stringi::stri_trans_general(Barrios$barrio,"Latin-ASCII"))

Comunas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Comunas')
Zonas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Zonas')
Zonas$Zonas_IDE<-Zonas$Zona

Barrios_precio_join <- dplyr::left_join(Barrios,as.data.frame(Barrios_precio), by = "Barrio_Clean")
Barrios_precio_join<-Barrios_precio_join[,3:11]
comuna_precio_join <- dplyr::left_join(Comunas,as.data.frame(Comuna_precio), by = "comuna")
comuna_precio_join<-comuna_precio_join[,1:13]
zonas_precio_join <- dplyr::left_join(Zonas,as.data.frame(Zona_precio), by = "Zonas_IDE")
zonas_precio_join<-zonas_precio_join[,1:10]


sf::st_write(Barrios_precio_join, dsn ="./sig/Barrios_Precio_Total.gpkg",   layer='Barrios_Precio_Total', driver ="GPKG", append=TRUE)
sf::st_write(comuna_precio_join, dsn ="./sig/Comuna_Precio_Total.gpkg", layer='Comuna_Precio_Total',  driver ="GPKG", append=TRUE)
sf::st_write(zonas_precio_join, dsn ="./sig/Zonas_Precio_Total.gpkg", layer='Zonas_Precio_Total',  driver ="GPKG", append=TRUE)


plot(Barrios_precio_join$mediana, Barrios_precio_join$IQR)
plot(comuna_precio_join$mediana, comuna_precio_join$IQR)
plot(zonas_precio_join$mediana, zonas_precio_join$IQR)

barriosPaleta <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(Barrios_precio_join$mediana))

barriosPaletaIQR<- colorNumeric(
  palette = "PuBuGn",
  domain = na.omit(Barrios_precio_join$IQR))

BarrioslabelsMediana <- sprintf(
  "<strong>%s</strong><br/>%g millones",
  Barrios_precio_join$barrio , Barrios_precio_join$mediana
) %>% lapply(htmltools::HTML)

BarrioslabelsIQR <- sprintf(
  "<strong>%s</strong><br/>%g millones",
  Barrios_precio_join$barrio , Barrios_precio_join$IQR
) %>% lapply(htmltools::HTML)


leaflet::leaflet(height=700, width = 800)%>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
  addPolygons(data = Barrios_precio_join, 
              fillColor =  ~barriosPaletaIQR(IQR),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Barrio - RIQ",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = BarrioslabelsIQR,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%

  addPolygons(data = Barrios_precio_join, 
              fillColor =  ~barriosPaleta(mediana),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Barrio - Mediana",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = BarrioslabelsMediana,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  
  addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
  addLegend( position = "bottomleft", pal = barriosPaleta, values = na.omit(Barrios_precio_join$mediana),
             title = 'Mediana Precios',
             opacity = 1)%>%
  addLegend( position = "bottomleft", pal = barriosPaletaIQR, values = na.omit(Barrios_precio_join$IQR),
             title = 'RIQ Precios',
             opacity = 1)%>%
  addLayersControl(overlayGroups = c("Barrio - Mediana", "Barrio - RIQ"),    options = layersControlOptions(collapsed = FALSE))%>%
  addMiniMap(tiles = "CartoDB.Voyager")




ComunaPaleta <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(comuna_precio_join$mediana))

ComunaPaletaIQR<- colorNumeric(
  palette = "PuBuGn",
  domain = na.omit(comuna_precio_join$IQR))

ComunalabelsMediana <- sprintf(
  "<strong>Comuna </strong><strong>%s</strong><br/>%g millones",
  comuna_precio_join$comuna , comuna_precio_join$mediana
) %>% lapply(htmltools::HTML)

ComunalabelsIQR <- sprintf(
  "<strong>Comuna </strong><strong>%s</strong><br/>%g millones",
  comuna_precio_join$comuna , comuna_precio_join$IQR
) %>% lapply(htmltools::HTML)


leaflet::leaflet(height=700, width = 800)%>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
    addPolygons(data = comuna_precio_join, 
              fillColor =  ~ComunaPaletaIQR(IQR),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Comuna RIQ",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = ComunalabelsIQR,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
    addPolygons(data = comuna_precio_join, 
              fillColor =  ~ComunaPaleta(mediana),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Comuna Mediana",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = ComunalabelsMediana,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
 
  addLegend( position = "bottomleft", pal = ComunaPaletaIQR, values = na.omit(comuna_precio_join$IQR),
             title = 'RIQ Precios',
             opacity = 1)%>%
  
  addLegend( position = "bottomleft", pal = ComunaPaleta, values = na.omit(comuna_precio_join$mediana),
             title = 'Mediana Precios',
             opacity = 1)%>%
  addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
  addLayersControl(overlayGroups = c("Comuna Mediana", "Comuna RIQ"),    options = layersControlOptions(collapsed = FALSE))%>%
  addMiniMap(tiles = "CartoDB.Voyager")




ZonaPaleta <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(zonas_precio_join$mediana))

ZonaPaletaIQR<- colorNumeric(
  palette = "PuBuGn",
  domain = na.omit(zonas_precio_join$IQR))

ZonalabelsMediana <- sprintf(
  "<strong></strong><strong>%s</strong><br/>%g millones",
  zonas_precio_join$Zonas_IDE , zonas_precio_join$mediana
) %>% lapply(htmltools::HTML)

ZonalabelsIQR <- sprintf(
  "<strong></strong><strong>%s</strong><br/>%g millones",
  zonas_precio_join$Zonas_IDE , zonas_precio_join$IQR
) %>% lapply(htmltools::HTML)


leaflet::leaflet(height=700, width = 800)%>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
  addPolygons(data = zonas_precio_join, 
              fillColor =  ~ZonaPaletaIQR(IQR),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Zona RIQ",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = ZonalabelsIQR,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addPolygons(data = zonas_precio_join, 
              fillColor =  ~ZonaPaleta(mediana),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Zona Mediana",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = ZonalabelsMediana,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  
  addLegend( position = "bottomleft", pal = ComunaPaletaIQR, values = na.omit(zonas_precio_join$IQR),
             title = 'RIQ Precios',
             opacity = 1)%>%
  
  addLegend( position = "bottomleft", pal = ComunaPaleta, values = na.omit(zonas_precio_join$mediana),
             title = 'Mediana Precios',
             opacity = 1)%>%
  addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
  addLayersControl(overlayGroups = c("Zona Mediana", "Zona RIQ"),    options = layersControlOptions(collapsed = FALSE))%>%
  addMiniMap(tiles = "CartoDB.Voyager")


#Análisis de datos faltantes####



require(mice)
png("Datos_limpios_MdPattern.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
md.pattern(datos_Vivienda[,c(1:13)], rotate.names = TRUE, plot=TRUE) 
dev.off()

require(VIM)
VIM::aggr(datos_Vivienda[,c(1:13)], numbers=TRUE, prop=FALSE)

png("Datos_Originales_MdPattern.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
md.pattern(vivienda_faltantes, rotate.names = TRUE, plot=TRUE) 
dev.off()

datos_Casa<-datos_Vivienda%>%subset(Tipo.Vivienda == "Casa")



png("datos_Casa_MdPattern.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
md.pattern(datos_Casa[,c(1:13)], rotate.names = TRUE, plot=TRUE) 
dev.off()



#Casas vs Estrato
Casa_Estrato03<-datos_Casa%>%subset(Estrato == "3")
Casa_Estrato04<-datos_Casa%>%subset(Estrato == "4")
Casa_Estrato05<-datos_Casa%>%subset(Estrato == "5")
Casa_Estrato06<-datos_Casa%>%subset(Estrato == "6")

Casa_Estrato03$No.Pisos<-as.factor(Casa_Estrato03$No.Pisos)
Casa_Estrato04$No.Pisos<-as.factor(Casa_Estrato04$No.Pisos)
Casa_Estrato05$No.Pisos<-as.factor(Casa_Estrato05$No.Pisos)
Casa_Estrato06$No.Pisos<-as.factor(Casa_Estrato06$No.Pisos)

barplot(summary(Casa_Estrato03$No.Pisos), main = "No.Pisos casas de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")

plot(Casa_Estrato03$No.Pisos, Casa_Estrato03$Area_Construida, col="white", border = '#367BB5')
plot(Casa_Estrato03$No.Parqueaderos, Casa_Estrato03$Area_Construida)

png("Casa_Estrato_Pisos.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Casa_Estrato03$No.Pisos), main = "No.Pisos casas de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato04$No.Pisos), main = "No.Pisos casas de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato05$No.Pisos), main = "No.Pisos casas de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato06$No.Pisos), main = "No.Pisos casas de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")

dev.off()


#Casas vs Parqueaderos

Casa_Estrato03$No.Parqueaderos<-as.factor(Casa_Estrato03$No.Parqueaderos)
Casa_Estrato04$No.Parqueaderos<-as.factor(Casa_Estrato04$No.Parqueaderos)
Casa_Estrato05$No.Parqueaderos<-as.factor(Casa_Estrato05$No.Parqueaderos)
Casa_Estrato06$No.Parqueaderos<-as.factor(Casa_Estrato06$No.Parqueaderos)


png("Casa_Estrato_Parqueaderos.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Casa_Estrato03$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato04$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato05$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato06$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")

dev.off()


datos_Apartamento<-datos_Vivienda%>%subset(Tipo.Vivienda == "Apartamento")

png("datos_Apartamento_MdPattern.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
md.pattern(datos_Apartamento, rotate.names = TRUE, plot=TRUE) 
dev.off()


#Apartamentos vs Estrato
Apartamento_Estrato03<-datos_Apartamento%>%subset(Estrato == "3")
Apartamento_Estrato04<-datos_Apartamento%>%subset(Estrato == "4")
Apartamento_Estrato05<-datos_Apartamento%>%subset(Estrato == "5")
Apartamento_Estrato06<-datos_Apartamento%>%subset(Estrato == "6")

Apartamento_Estrato03$No.Pisos<-as.factor(Apartamento_Estrato03$No.Pisos)
Apartamento_Estrato04$No.Pisos<-as.factor(Apartamento_Estrato04$No.Pisos)
Apartamento_Estrato05$No.Pisos<-as.factor(Apartamento_Estrato05$No.Pisos)
Apartamento_Estrato06$No.Pisos<-as.factor(Apartamento_Estrato06$No.Pisos)


png("Apartamento_Estrato_Pisos.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Apartamento_Estrato03$No.Pisos), main = "No.Pisos Apartamentos de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato04$No.Pisos), main = "No.Pisos Apartamentos de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato05$No.Pisos), main = "No.Pisos Apartamentos de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato06$No.Pisos), main = "No.Pisos Apartamentos de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")

dev.off()


#Apartamentos vs Parqueaderos

Apartamento_Estrato03$No.Parqueaderos<-as.factor(Apartamento_Estrato03$No.Parqueaderos)
Apartamento_Estrato04$No.Parqueaderos<-as.factor(Apartamento_Estrato04$No.Parqueaderos)
Apartamento_Estrato05$No.Parqueaderos<-as.factor(Apartamento_Estrato05$No.Parqueaderos)
Apartamento_Estrato06$No.Parqueaderos<-as.factor(Apartamento_Estrato06$No.Parqueaderos)


png("Apartamento_Estrato_Parqueaderos.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Apartamento_Estrato03$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato04$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato05$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato06$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")

dev.off()


#Imputacion de datos####

# Casas

Casa_Estrato03_Imp<-Casa_Estrato03
Casa_Estrato04_Imp<-Casa_Estrato04
Casa_Estrato05_Imp<-Casa_Estrato05
Casa_Estrato06_Imp<-Casa_Estrato06

Casa_Estrato03_Imp$No.Parqueaderos[is.na(Casa_Estrato03_Imp$No.Parqueaderos)]<-1
Casa_Estrato04_Imp$No.Parqueaderos[is.na(Casa_Estrato04_Imp$No.Parqueaderos)]<-1
Casa_Estrato05_Imp$No.Parqueaderos[is.na(Casa_Estrato05_Imp$No.Parqueaderos)]<-2
Casa_Estrato06_Imp$No.Parqueaderos[is.na(Casa_Estrato06_Imp$No.Parqueaderos)]<-2

Casa_Estrato03_Imp$No.Pisos[is.na(Casa_Estrato03_Imp$No.Pisos)]<-2
Casa_Estrato04_Imp$No.Pisos[is.na(Casa_Estrato04_Imp$No.Pisos)]<-2
Casa_Estrato05_Imp$No.Pisos[is.na(Casa_Estrato05_Imp$No.Pisos)]<-2
Casa_Estrato06_Imp$No.Pisos[is.na(Casa_Estrato06_Imp$No.Pisos)]<-2

Casas_Imputadas<-rbind(Casa_Estrato03_Imp, Casa_Estrato04_Imp, Casa_Estrato05_Imp, Casa_Estrato06_Imp)

png("datos_Casa_Imputadas_MdPattern.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
md.pattern(Casas_Imputadas, rotate.names = TRUE, plot=TRUE) 
dev.off()

# Apartamentos

Apartamento_Estrato03_Imp<-Apartamento_Estrato03
Apartamento_Estrato04_Imp<-Apartamento_Estrato04
Apartamento_Estrato05_Imp<-Apartamento_Estrato05
Apartamento_Estrato06_Imp<-Apartamento_Estrato06

Apartamento_Estrato03_Imp$No.Parqueaderos[is.na(Apartamento_Estrato03_Imp$No.Parqueaderos)]<-1
Apartamento_Estrato04_Imp$No.Parqueaderos[is.na(Apartamento_Estrato04_Imp$No.Parqueaderos)]<-1
Apartamento_Estrato05_Imp$No.Parqueaderos[is.na(Apartamento_Estrato05_Imp$No.Parqueaderos)]<-2
Apartamento_Estrato06_Imp$No.Parqueaderos[is.na(Apartamento_Estrato06_Imp$No.Parqueaderos)]<-2

Apartamento_Estrato03_Imp$No.Pisos[is.na(Apartamento_Estrato03_Imp$No.Pisos)]<-5
Apartamento_Estrato04_Imp$No.Pisos[is.na(Apartamento_Estrato04_Imp$No.Pisos)]<-5
Apartamento_Estrato05_Imp$No.Pisos[is.na(Apartamento_Estrato05_Imp$No.Pisos)]<-3
Apartamento_Estrato06_Imp$No.Pisos[is.na(Apartamento_Estrato06_Imp$No.Pisos)]<-3

Apartamento_Imputadas<-rbind(Apartamento_Estrato03_Imp, Apartamento_Estrato04_Imp, Apartamento_Estrato05_Imp, Apartamento_Estrato06_Imp)

png("datos_Apartamento_Imputadas_MdPattern.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
md.pattern(Apartamento_Imputadas, rotate.names = TRUE, plot=TRUE) 
dev.off()


#ANALISIS ESTADISTICO CON DATOS IMPUTADOS

#Casas
png("Casa_Estrato_Pisos_Imputadas.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Casa_Estrato03_Imp$No.Pisos), main = "No.Pisos casas de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato04_Imp$No.Pisos), main = "No.Pisos casas de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato05_Imp$No.Pisos), main = "No.Pisos casas de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato06_Imp$No.Pisos), main = "No.Pisos casas de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
dev.off()

png("Casa_Estrato_Parqueaderos_Imputadas.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Casa_Estrato03_Imp$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato04_Imp$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato05_Imp$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Casa_Estrato06_Imp$No.Parqueaderos), main = "No.Parqueaderos casas de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
dev.off()

#Apartamentos
png("Apartamento_Estrato_Pisos_Imputadas.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Apartamento_Estrato03_Imp$No.Pisos), main = "No.Pisos Apartamentos de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato04_Imp$No.Pisos), main = "No.Pisos Apartamentos de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato05_Imp$No.Pisos), main = "No.Pisos Apartamentos de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato06_Imp$No.Pisos), main = "No.Pisos Apartamentos de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
dev.off()

png("Apartamento_Estrato_Parqueaderos_Imputadas.png", width = 1250, height = 750, units = "px", pointsize = 12, bg = "#FFFFFF", res = 100 )
par(mfrow=c(2,2))
barplot(summary(Apartamento_Estrato03_Imp$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 3", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato04_Imp$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 4", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato05_Imp$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 5", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
barplot(summary(Apartamento_Estrato06_Imp$No.Parqueaderos), main = "No.Parqueaderos Apartamentos de estrato 6", xlab = "Pisos", ylab="Frecuencia", col="#367BB5", border = 'white', bg="#FFFFFF")
dev.off()




datos_imputados_Totales<-rbind(Casas_Imputadas, Apartamento_Imputadas)


datos_imputados_Totales <- dplyr::left_join(datos_imputados_Totales, as.data.frame(Cali_WGS84), by = "Barrio_Clean")

for(i in 1:length(datos_imputados_Totales$Zonas_IDE)){
  if(is.na(datos_imputados_Totales$Zonas_IDE[i])== "TRUE"){
    datos_imputados_Totales$Zonas_IDE[i] = datos_imputados_Totales$Zona[i]
  }
}


datos_imputados_Totales<-as.data.frame(datos_imputados_Totales)
datos_imputados_Totales<-datos_imputados_Totales[,c(1:20,22)]
write.csv(datos_imputados_Totales, "datos_imputados_Totales.csv", row.names = FALSE)

#####Espacial####

#Casas####

Zonas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Zonas')
Comunas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Comunas')
Barrios<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Barrios')

require(leaflet)

leaflet::leaflet()%>%
  addTiles() %>%  # Agrega tiles de OpenStreetMap
  # Agrega el objeto sf al mapa
  addPolygons(data = Zonas)


Cali_WGS84$Barrio_Clean<-toupper(stringi::stri_trans_general(Cali_WGS84$barrio,"Latin-ASCII"))

#Convertir la tabla de viviendas en un objeto espacial


Casas_Imputadas_Espacial <- sf::st_as_sf(Casas_Imputadas, coords = c("Longitud", "Latitud"))
Casas_Imputadas_Espacial
Casas_Imputadas_Espacial$Barrio_Clean<-as.factor(Casas_Imputadas_Espacial$Barrio_Clean)
crs <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
Casas_Imputadas_Espacial <- sf::st_set_crs(Casas_Imputadas_Espacial, crs)


#Union de capas
Casas_Imputadas_join <- dplyr::left_join(Casas_Imputadas_Espacial, as.data.frame(Cali_WGS84), by = "Barrio_Clean")
Casas_Impitadas_zonas_join <- dplyr::left_join(Cali_WGS84,as.data.frame(Casas_Imputadas_Espacial), by = "Barrio_Clean")




summary(Casas_Imputadas_join$Zonas_IDE)

#Imputacion por datos originales
for(i in 1:length(Casas_Imputadas_join$Zonas_IDE)){
  if(is.na(Casas_Imputadas_join$Zonas_IDE[i])== "TRUE"){
    Casas_Imputadas_join$Zonas_IDE[i] = Casas_Imputadas_join$Zona[i]
  }
}



write.table(Casas_Imputadas_join, "Casas_Imputadas_join.csv", sep=",", col.names = TRUE)

Casas_Barrios_precio<-Casas_Imputadas_join%>%group_by(Barrio_Clean)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

Casas_Comuna_precio<-Casas_Imputadas_join%>%group_by(comuna)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

Casas_Zona_precio<-Casas_Imputadas_join%>%group_by(Zonas_IDE)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

boxplot(Casas_Zona_precio$mediana)



Barrios<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Barrios')
Barrios$Barrio_Clean<-toupper(stringi::stri_trans_general(Barrios$barrio,"Latin-ASCII"))

Comunas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Comunas')
Zonas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Zonas')
Zonas$Zonas_IDE<-Zonas$Zona

Casas_Barrios_precio_join <- dplyr::left_join(Barrios,as.data.frame(Casas_Barrios_precio), by = "Barrio_Clean")
Casas_Barrios_precio_join<-Casas_Barrios_precio_join[,1:15]
Casas_comuna_precio_join <- dplyr::left_join(Comunas,as.data.frame(Casas_Comuna_precio), by = "comuna")
Casas_comuna_precio_join<-Casas_comuna_precio_join[,1:13]
Casas_zonas_precio_join <- dplyr::left_join(Zonas,as.data.frame(Casas_Zona_precio), by = "Zonas_IDE")
Casas_zonas_precio_join<-Casas_zonas_precio_join[,1:10]


sf::st_write(Casas_Barrios_precio_join, dsn ="./sig/Barrios_Precio_Total.gpkg",   layer='Casas_Barrios_precio', driver ="GPKG", append=TRUE)
sf::st_write(Casas_comuna_precio_join, dsn ="./sig/Comuna_Precio_Total.gpkg", layer='Casas_comuna_precio',  driver ="GPKG", append=TRUE)
sf::st_write(Casas_zonas_precio_join, dsn ="./sig/Zonas_Precio_Total.gpkg", layer='Casas_zonas_precio',  driver ="GPKG", append=TRUE)


plot(Casas_Barrios_precio_join$mediana, Casas_Barrios_precio_join$IQR)
plot(Casas_comuna_precio_join$mediana, Casas_comuna_precio_join$IQR)
plot(Casas_zonas_precio_join$mediana, Casas_zonas_precio_join$IQR)

Casas_barriosPaleta <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(Casas_Barrios_precio_join$mediana))

Casas_barriosPaletaIQR<- colorNumeric(
  palette = "PuBuGn",
  domain = na.omit(Casas_Barrios_precio_join$IQR))

Casas_labelsMediana <- sprintf(
  "<strong>%s</strong><br/>%g millones",
  Casas_Barrios_precio_join$barrio , Casas_Barrios_precio_join$mediana
) %>% lapply(htmltools::HTML)

Casas_labelsIQR <- sprintf(
  "<strong>%s</strong><br/>%g millones",
  Casas_Barrios_precio_join$barrio , Casas_Barrios_precio_join$IQR
) %>% lapply(htmltools::HTML)


leaflet::leaflet(height=700, width = 800)%>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
  addPolygons(data = Casas_Barrios_precio_join, 
              fillColor =  ~Casas_barriosPaletaIQR(IQR),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "IQR",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = Casas_labelsIQR,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addLegend( position = "bottomleft", pal = barriosPaletaIQR, values = na.omit(Barrios_precio_join$IQR),
             title = 'IQR Precios',
             opacity = 1)%>%
  addPolygons(data = Casas_Barrios_precio_join, 
              fillColor =  ~Casas_barriosPaleta(mediana),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Mediana",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = Casas_labelsMediana,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  
  addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
  addLegend( position = "bottomleft", pal = Casas_barriosPaleta, values = na.omit(Casas_Barrios_precio_join$mediana),
             title = 'Mediana Precios',
             opacity = 1)%>%
  
  addLayersControl(overlayGroups = c("Mediana", "IQR"),    options = layersControlOptions(collapsed = FALSE))%>%
  addMiniMap(tiles = "CartoDB.Voyager")


#Apartamentos####

Zonas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Zonas')
Comunas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Comunas')
Barrios<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Barrios')



Apartamentos_Imputadas_Espacial <- sf::st_as_sf(Apartamento_Imputadas, coords = c("Longitud", "Latitud"))
Apartamentos_Imputadas_Espacial
Apartamentos_Imputadas_Espacial$Barrio_Clean<-as.factor(Apartamentos_Imputadas_Espacial$Barrio_Clean)
crs <- sf::st_crs("+proj=longlat +datum=WGS84 +no_defs")
Apartamentos_Imputadas_Espacial <- sf::st_set_crs(Apartamentos_Imputadas_Espacial, crs)


#Union de capas
Apartamentos_Imputadas_join <- dplyr::left_join(Apartamentos_Imputadas_Espacial, as.data.frame(Cali_WGS84), by = "Barrio_Clean")
Apartamentos_Impitadas_zonas_join <- dplyr::left_join(Cali_WGS84,as.data.frame(Apartamentos_Imputadas_Espacial), by = "Barrio_Clean")




summary(Apartamentos_Imputadas_join$Zonas_IDE)

#Imputacion por datos originales

for(i in 1:length(Apartamentos_Imputadas_join$Zonas_IDE)){
  if(is.na(Apartamentos_Imputadas_join$Zonas_IDE[i])== "TRUE"){
    Apartamentos_Imputadas_join$Zonas_IDE[i] = Apartamentos_Imputadas_join$Zona[i]
  }
}
Apartamentos_Imputadas_join$Zonas_IDE<-as.factor(Apartamentos_Imputadas_join$Zonas_IDE)
summary(Apartamentos_Imputadas_join$Zonas_IDE)

write.table(Apartamentos_Imputadas_join, "Apartamentos_Imputadas_join.csv", sep=",", col.names = TRUE)

Apartamentos_Barrios_precio<-Apartamentos_Imputadas_join%>%group_by(Barrio_Clean)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

Apartamentos_Comuna_precio<-Apartamentos_Imputadas_join%>%group_by(comuna)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

Apartamentos_Zona_precio<-Apartamentos_Imputadas_join%>%group_by(Zonas_IDE)%>%summarise(
  media = mean(Precio),
  mediana = median(Precio ),
  IQR = IQR(Precio ),
  Q1 =quantile(Precio, 0.25),
  Q3 =quantile(Precio, 0.75),
  VarCoef = sd(Precio) / mean(Precio),
  sd = sd(Precio)
)

boxplot(Apartamentos_Zona_precio$mediana)



Barrios<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Barrios')
Barrios$Barrio_Clean<-toupper(stringi::stri_trans_general(Barrios$barrio,"Latin-ASCII"))

Comunas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Comunas')
Zonas<-sf::read_sf("./sig/Cali_WGS84.gpkg", layer='Zonas')
Zonas$Zonas_IDE<-Zonas$Zona

Apartamentos_Barrios_precio_join <- dplyr::left_join(Barrios,as.data.frame(Apartamentos_Barrios_precio), by = "Barrio_Clean")
Apartamentos_Barrios_precio_join<-Apartamentos_Barrios_precio_join[,1:15]
Apartamentos_comuna_precio_join <- dplyr::left_join(Comunas,as.data.frame(Apartamentos_Comuna_precio), by = "comuna")
Apartamentos_comuna_precio_join<-Apartamentos_comuna_precio_join[,1:13]
Apartamentos_zonas_precio_join <- dplyr::left_join(Zonas,as.data.frame(Apartamentos_Zona_precio), by = "Zonas_IDE")
Apartamentos_zonas_precio_join<-Apartamentos_zonas_precio_join[,1:10]


sf::st_write(Apartamentos_Barrios_precio_join, dsn ="./sig/Barrios_Precio_Total.gpkg",   layer='Apartamentos_Barrios_precio', driver ="GPKG", append=TRUE)
sf::st_write(Apartamentos_comuna_precio_join, dsn ="./sig/Comuna_Precio_Total.gpkg", layer='Apartamentos_comuna_precio',  driver ="GPKG", append=TRUE)
sf::st_write(Apartamentos_zonas_precio_join, dsn ="./sig/Zonas_Precio_Total.gpkg", layer='Apartamentos_zonas_precio',  driver ="GPKG", append=TRUE)


plot(Apartamentos_Barrios_precio_join$mediana, Apartamentos_Barrios_precio_join$IQR)
plot(Apartamentos_comuna_precio_join$mediana, Apartamentos_comuna_precio_join$IQR)
plot(Apartamentos_zonas_precio_join$mediana, Apartamentos_zonas_precio_join$IQR)

Apartamentos_barriosPaleta <- colorNumeric(
  palette = "YlOrRd",
  domain = na.omit(Apartamentos_Barrios_precio_join$mediana))

Apartamentos_barriosPaletaIQR<- colorNumeric(
  palette = "PuBuGn",
  domain = na.omit(Apartamentos_Barrios_precio_join$IQR))

Apartamentos_labelsMediana <- sprintf(
  "<strong>%s</strong><br/>%g millones",
  Apartamentos_Barrios_precio_join$barrio , Apartamentos_Barrios_precio_join$mediana
) %>% lapply(htmltools::HTML)

Apartamentos_labelsIQR <- sprintf(
  "<strong>%s</strong><br/>%g millones",
  Apartamentos_Barrios_precio_join$barrio , Apartamentos_Barrios_precio_join$IQR
) %>% lapply(htmltools::HTML)


leaflet::leaflet(height=700, width = 800)%>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
  addPolygons(data = Apartamentos_Barrios_precio_join, 
              fillColor =  ~Apartamentos_barriosPaletaIQR(IQR),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "IQR",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = Apartamentos_labelsIQR,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addLegend( position = "bottomleft", pal = barriosPaletaIQR, values = na.omit(Barrios_precio_join$IQR),
             title = 'IQR Precios',
             opacity = 1)%>%
  addPolygons(data = Apartamentos_Barrios_precio_join, 
              fillColor =  ~Apartamentos_barriosPaleta(mediana),  weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              group = "Mediana",
              fillOpacity = 1,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                bringToFront = TRUE),
              label = Apartamentos_labelsMediana,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  
  addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
  addLegend( position = "bottomleft", pal = Apartamentos_barriosPaleta, values = na.omit(Apartamentos_Barrios_precio_join$mediana),
             title = 'Mediana Precios',
             opacity = 1)%>%
  
  addLayersControl(overlayGroups = c("Mediana", "IQR"),    options = layersControlOptions(collapsed = FALSE))%>%
  addMiniMap(tiles = "CartoDB.Voyager")
