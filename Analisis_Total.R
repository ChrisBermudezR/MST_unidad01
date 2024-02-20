#Analisis

if(!require(e1071)) install.packages("e1071")
if(!require(dplyr)) install.packages("dplyr")

datos_imputados_Totales<-read.table("datos_imputados_Totales.csv", sep=",", header = TRUE)

e1071::skewness(datos_imputados_Totales$Precio)
base::summary(datos_imputados_Totales$Precio)

mxLo<-matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE)
nf<-layout(mat=mxLo, widths = c(3,3), heights = c(1,3), respect=TRUE)
par(mar=c(0,3,1,1))
boxplot(datos_join$Precio,horizontal=TRUE, axes=FALSE, frame.plot=FALSE, col='white', border = '#367BB5', main=NULL)
par(mar=c(4,4,2,2))
hist(datos_join$Precio, breaks = 30, col = '#367BB5', border = 'white', ylab="Frecuencia",xlab="Precios (millones)", main=NULL)



subset_zones <- c("Zona Centro", "Zona Norte", "Zona Oriente", "Zona Oeste", "Zona Sur")

for (i in 1:length(subset_zones)) {
  print(subset_zones[i])
  print(e1071::skewness(datos_imputados_Totales %>% filter(Zonas_IDE %in% subset_zones[i]) %>% pull(Precio))) 
}


for (i in 1:length(subset_zones)) {
  print(subset_zones[i])
  print(summary(datos_imputados_Totales %>% filter(Zonas_IDE %in% subset_zones[i]) %>% pull(Precio))) 
}

zonas_precio_join<-sf::read_sf("./sig/Cali_WGS84.gpkg",   layer='Zonas_Precio_Total')
plot(zonas_precio_join$mediana, zonas_precio_join$IQR, 
     main = "Relación entre el precio y la variabilidad de precios", 
     xlab = "Mediana de precios por zonas", 
     ylab = "RIQ de precio por zonas", 
     pch = 19, 
     col = "#367BB5", # Color de los puntos
     bg = "#FFFFFF")


cor(zonas_precio_join$mediana, zonas_precio_join$IQR,  method = "spearman")




comuna_precio_join<-sf::read_sf("./sig/Cali_WGS84.gpkg",   layer='Comuna_Precio_Total')
plot(comuna_precio_join$mediana, comuna_precio_join$IQR, 
     main = "Relación entre el precio y la variabilidad de precios", 
     xlab = "Mediana de precios por zonas", 
     ylab = "RIQ de precio por zonas", 
     pch = 19, 
     col = "#367BB5", # Color de los puntos
     bg = "#FFFFFF")


cor(comuna_precio_join$mediana, comuna_precio_join$IQR,  method = "spearman")



Barrios_precio_join<-sf::read_sf("./sig/Cali_WGS84.gpkg",   layer='Barrios_Precio_Total')

plot(Barrios_precio_join$mediana, Barrios_precio_join$IQR, 
     main = "Relación entre el precio y la variabilidad de precios", 
     xlab = "Mediana de precios por comunas", 
     ylab = "RIQ de precio por comunas", 
     pch = 19, 
     col = "#367BB5", # Color de los puntos
     bg = "#FFFFFF")


cor(na.omit(Barrios_precio_join$mediana), na.omit(Barrios_precio_join$IQR),  method = "spearman")



#Tipos de viviendas

tipo <- c("Apartamento", "Casa")

for (i in 1:length(tipo)) {
  print(tipo[i])
  print(e1071::skewness(datos_imputados_Totales %>% filter(Tipo.Vivienda %in% tipo[i]) %>% pull(Precio))) 
}


for (i in 1:length(tipo)) {
  print(tipo[i])
  print(summary(datos_imputados_Totales %>% filter(Tipo.Vivienda %in% tipo[i]) %>% pull(Precio))) 
}


boxplot(Precio ~ Tipo.Vivienda, data = datos_imputados_Totales,
        main = "Distribución de precios por tipos de vivienda",
        xlab = "Zona",
        ylab = "Número de datos",
        col = "#367BB5", # Color de los puntos
        bg = "#FFFFFF")

Casas<- datos_imputados_Totales %>% subset(Tipo.Vivienda == "Casa")

boxplot(Precio ~ Zonas_IDE, data = Casas,
        main = "Distribución de los precios de las casas por zonas",
        xlab = "Zona",
        ylab = "Número de datos",
        col = "#367BB5", # Color de los puntos
        bg = "#FFFFFF")


Apartamentos<- datos_imputados_Totales %>% subset(Tipo.Vivienda == "Apartamento")
boxplot(Precio ~ Zonas_IDE, data = Apartamentos,
        main = "Distribución de los precios de las apartamentos por zonas",
        xlab = "Zona",
        ylab = "Número de datos",
        col = "#367BB5", # Color de los puntos
        bg = "#FFFFFF")


colnames(datos_imputados_Totales)



## Caracteristicas de Piso

require(ggplot2)

datos_imputados_Totales$No.Pisos<-as.factor(datos_imputados_Totales$No.Pisos)

ggplot(datos_imputados_Totales, aes(x = No.Pisos, y = Precio, fill = Tipo.Vivienda)) +
  geom_boxplot() +
  facet_wrap(~ Estrato, scales = "free") +
  labs(title = "Boxplot de Precio vs. Número de Pisos",
       x = "Número de Pisos",
       y = "Precio") +
  scale_fill_manual(values = c("#FF5733", "#335EFF"))+
  theme_bw()


### plot precio area

colors <- c("#367BB5", 
            "#ece7f2") 

plot( datos_imputados_Totales$Area_Construida,datos_imputados_Totales$Precio,
       pch = 19,
     col = colors[factor(datos_imputados_Totales$Tipo.Vivienda)],
main = "Relación Precio con Área Construida por Tipo de Vivienda",
xlab = "Área Construida",
ylab = "Precio")

legend("topleft",
       legend = c("Apartamento", "Casa"),
       pch = 19,
       col = colors)


#Correlaciones tipo de viviendas

cor(Apartamentos$Precio, Apartamentos$Area_Construida,  method = "spearman")
cor(Casas$Precio, Casas$Area_Construida,  method = "spearman")
