#############################################
## Title:  "ANÁLISIS EXPLORATORO DE DATOS" ##
## Author: "DILAN SUÁREZ AGÜERO"           ##
## Date:   "05/05/2022"                    ##
#############################################

##------------------------------------------------------------------------------------

#### INSTALACIÓN Y ACTIVACIÓN DE PAQUETES

#
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")

#
library(dplyr)
library(corrplot)
library(ggplot2)

#### IMPORTANDO LA BASE DE DATOS DE R

#Para este ejercicio utilizaremos la base de datos CO2 "Carbon Dioxide Uptake in Grass Plants", correspondiente a un experimento sobre la tolerancia al frío de la especie de pasto Echinochloa crus-galli.
  
data("CO2") #Importaci??n de dataset
CO2         #Visualizaci??n parcial en consola
View(CO2)   #Visualizaci??n completa en una nueva ventana

#### UN VISTAZO A LOS DATOS

#Para la obtención de algunos detalles sobre cómo está estructurada la base de datos, utilizaremos la función glimpse() contenida dentro del paquete dplyr.
  
#Esta función nos brinda información sobre:
  
# -El número de filas (observaciones) y columnas (variables) que presenta la base de datos.

# -La naturaleza de los elementos de cada variable (columna): <ord> , <fct>, <dbl>.

# -Contenido de cada variable (columna) en forma de lista.

#Es importante tambi??n identificar la presencia de valores perdidos o "NA values" para que no generen conflictos al momento de realizar an??lisis estad??sticos posteriores, para ello usaremos la funci??n is.na().

glimpse(CO2)
is.na(CO2)

#### ANÁLISIS DE VARIABLES CATEGÓRICAS

#Para las variables categóricas podemos calcular tablas de frecuencia, es decir, ver el número de ocurrencias de cada categoría de la variable. Esto lo hacemos con la función table().

table(CO2$Plant)
table(CO2$Type)
table(CO2$Treatment)

#### TABLAS DE CONTINGENCIA

#Para poder evaluar la frecuencia de dos variables categ??ricas podemos utilizar tablas de contingencia. Esto lo hacemos con la función prop.table()
#Tablas de contingencia para cada combinaci??n de variables categ??ricas. Valores expresados en porcentajes.

Plant_vs_Type <- prop.table(table(CO2$Plant, CO2$Type),2)*100
Plant_vs_Type
Type_vs_Treatment <- prop.table(table(CO2$Type, CO2$Treatment),2)*100
Type_vs_Treatment
Plant_vs_Treatment <- prop.table(table(CO2$Plant, CO2$Treatment),2)*100
Plant_vs_Treatment

### ANÁLISIS DE VARIABLES NUMÉRICAS

#Para calcular los estadísticos descriptivos de las variables numéricas utilizaremos la función summary(). Esta función de puede aplicar a toda la base de datos o solo a variables específicas dentro de mi base de datos. Los valores que nos brinda son:
  
# -Valor mínimo

# -Valor del 1er cuartil

# -Mediana

# -Promedio

# -Valor del 3er cuartil

# -Valor máximo

summary(CO2$conc)
summary(CO2$uptake)

#### VISUALIZACIÓN DE DATOS NUMÉRICOS

#Una forma útil de explorar la distribución de los datos en variables numéricas es con histogramas. Estos podemos hacerlos con el comando hist(), donde ponemos como argumento el nombre de la tabla seguido de $ y el nombre de la variable que deseamos graficar.

hist(CO2$conc, col = "pink", border = "red", xlab ="Concentration (ml/L)", main = "Histogram")
hist(CO2$uptake, col = "gray", border ="black", xlab ="Uptake (umol/m^2 sec)", main = "Histogram")

#### CORRELACIONES

#Una correlaci??n mide la relaci??n lineal entre dos variables. De esta manera podemos establecer si una variable está influenciada por otra de forma positiva o negativa.

#El resultado de este análisis es un coeficiente que va de -1 a 1

# -R = 1, la relación es positiva perfecta

# -0 \< R \< 1 la relación es positiva

# -R = 0 no hay relación lineal

# - -1 \< R \< 0 la relación es negativa

# -R = -1 la relación es negativa perfecta

#Cálculo del coeficiente de correlación para las dos variables numéricas

cor(CO2$conc, CO2$uptake)

#Creación de una tabla que solo contenga las dos variables num??ricas de la base de datos CO2

cor <- data.frame(Concentration = CO2$conc, Uptake = CO2$uptake)
cor

#Creación de gráfica con ggplot2

ggplot(cor, mapping = aes(x = Concentration, y = Uptake)) +
  geom_point(color="blue", size = 1.5) +
  labs(x = "Concentration" , y = "Uptake") +
  geom_smooth(method = "lm", se = TRUE, color = "red", lwd = 0.5) +
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 
