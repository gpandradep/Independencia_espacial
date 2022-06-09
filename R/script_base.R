##%######################################################%##
#                                                          #
####   Explorando la independencia espacial en datos    ####
####       de Fototrampeo # Gabriel Andrade Ponce       ####
#                                                          #
##%######################################################%##


# Librerías ---------------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(sp) # Classes and Methods for Spatial Data
library(sf) # Simple Features for R
library(gghighlight) # Highlight Lines and Points in 'ggplot2'
library(showtext) # Using Fonts More Easily in R Graphs
library(ggtext) # Improved Text Rendering Support for 'ggplot2'
library(ggspatial) # Spatial Data Framework for ggplot2
library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
library(DHARMa) # Residual Diagnostics for Hierarchical (Multi-Level / Mixed) Regression Models
library(MASS) # Support Functions and Datasets for Venables and Ripley's MASS

# Paquetes para correlogramas
library(pgirmess) # Spatial Analysis and Data Mining for Field Ecologists
library(ncf) # Spatial Covariance Functions



font_add_google("Fira Sans", "firasans") # Familia de tipo de letra
showtext_auto()


# Cargar datos ------------------------------------------------------------

CTtable <- read.csv("Data/CTtable.csv") %>%  # Base de coordenadas de cámaras
 dplyr::select(Station, utm_x, utm_y ) # Seleccionar columnas que usaremos
  
events_by_species <- read_csv("Data/surveyReport/events_by_species.csv") %>% 
  type.convert(n_events= col_double(), # Convertir tipo de columna
               n_stations= col_double()) %>% # Convertir tipo de columna
  mutate(Sp_n= paste("*",species,"*", " (n= ", n_events, " ) ", sep = "") ) # Columna para los nombres del eje

freq_reg <- read.csv("Data/surveyReport/events_by_station2.csv") %>% # Datos de registros por estación
  filter(Species == "Odocoileus virginianus") %>% # Filtrar especie
  left_join(CTtable, by= "Station") # Unir con la base da cámaras



# Gráficas de registros ---------------------------------------------------

# Gráfica de número de registros

(ndetectionplot <- ggplot(events_by_species, # Datos
                          aes(x= reorder(Sp_n,n_events), #Ordenar sp 
                              y= n_events))+ # No. eventos
    geom_bar(stat= "identity")+  # Geometria
    gghighlight(species %in% c("Odocoileus virginianus"))+ # Señalar venados
    coord_flip()+ # Girar ejes
    labs(title = "Tabla de frecuencia de registro de especies", # Título
      y= "Número de registros independientes", # Eje y
      x= NULL)+ # Sin eje x
    theme_minimal()+ # Tema
    theme(text = element_text(family = 'firasans', size = 16), # tipo de letra
      plot.title.position = 'plot', # Marco de posición
      plot.title = element_text(face = 'bold', # Negrillas 
        margin = margin(t = 2, r = 0, b = 7, l = 0, unit = "mm"), # Margenes
        hjust = 0.5), #posición central
      axis.text.y= element_markdown())) # Itálica nombre de especies
        
      
# Histograma de registros
  
(hist <- ggplot(freq_reg, aes(x= n_events))+ # Datos
    geom_histogram(binwidth = 3, color= "white")+ # Geometria
    labs(title = " Histograma número de detecciones",
      x= "Número de detecciones",
      y= "Frecuencia") +
    theme_bw()+ # Tema
    theme(text = element_text(family = 'firasans', size = 16), # tipo de letra
          plot.title.position = 'plot', # Marco de posición
          plot.title = element_text(face = 'bold', # Negrillas 
                                    margin = margin(t = 2, r = 0, b = 7, l = 0, unit = "mm"), # Margenes
                                    hjust = 0.5)))

UMA <- st_read("Shape/UMA.shp") # Leer el shape de la UMA
# Es importante verificar que este en la misma proyección (CRS)

# Mapa de registros

(map <- ggplot(UMA)+ # shape de uma
  geom_sf()+ # Graficar objeto espacial
  geom_point(data= freq_reg, aes(x=utm_x, y=utm_y, # Agregar puntos
                 size= n_events, color= n_events), # Tamaño y color
             alpha=0.9)+ # Transparencia
  scale_size(range = c(1,20))+ # Escala del radio para los puntos
  labs(title= "Mapa de número detecciones venado cola blanca",
       y= "Lat",
       x= "Lon",
       size= "Número de \ndetecciones",
       color=NULL)+
    annotation_scale(location = "bl", width_hint = 0.5, # Barra de escala
                     line_width=2, tick_height=10) +
    annotation_north_arrow(location = "tr", # Flecha de norte
                           height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                           pad_x = unit(1, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering)+
  theme_bw()+
    theme(text=element_text(size=15, family = "special"))+
  theme(text = element_text(family = 'firasans', size = 16), # tipo de letra
        plot.title.position = 'plot', # Marco de posición
        plot.title = element_text(face = 'bold', # Negrillas 
                                  margin = margin(t = 2, r = 0, b = 7, l = 0, unit = "mm"), # Margenes
                                  hjust = 0.5)))


# Mapa interactivo --------------------------------------------------------------------

# Generar objeto espacial para el mapa
CT_points <- SpatialPoints(cbind(CTtable$utm_x, 
                                 CTtable$utm_y),
                           proj4string = CRS('+proj=utm +datum=WGS84 +zone=14 +towgs84=0,0,0')) 

# Proyectar a WGS solo para este paso
CT_points <- spTransform(CT_points, "+proj=longlat +datum=WGS84")

UMA_proj <- st_transform(UMA, "+proj=longlat +datum=WGS84")

# Generar el mapa
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  # Add satellite data
  addProviderTiles(providers$Esri.WorldTopoMap, group="Base") %>% 
  addCircleMarkers(lng= sp::coordinates(CT_points)[,1], lat=sp::coordinates(CT_points)[,2], 
                   popup= paste(CTtable$Station)) %>%
  addPolygons(lng= st_coordinates(UMA_proj)[,1], lat = st_coordinates(UMA_proj)[,2],
              fillOpacity= 0) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("Satellite", "Base"),
    options = layersControlOptions(collapsed = FALSE)
  )
m


# Modelando la frecuencia de captura --------------------------------------

# Vamos a modelar la frecuencia de registros del venado por medio de un GLM poisson. Uno de los supuestos de los GLMs es que las unidades espaciales son independientes, de manera que vamos a verificar que en nuestro caso es así. Adicionalmente, vamos a probar algunas covariables que nos ayuden a explicar la distribución espacial de los registros. La no inclusión de una variable importante generaría autocorrelación espacial de los residuales.

covs.data<-read.csv("Data/covars.csv", header=TRUE) %>% # covariables
  dplyr::select(-X) # eliminar columna X

covs.data$Cluster<- as.character(covs.data$Cluster) # cluster es categórica

### Separar las variables númericas y categóricas
cov.num <- covs.data %>% 
  dplyr::select(where(is.numeric)) %>% # Seleccionar columnas numéricas
  scale() %>%  # estandarizar
  as.data.frame()

cov.fac <- covs.data %>% 
  dplyr::select(where(is.character)) # Seleccionar variables de caracter

### Unir las bases
sp_glmdata <- data.frame(cov.fac, cov.num) %>% # Unir covariables
  right_join(freq_reg, by= "Station")# Unir con la base de rgistros
  

# Modelos lineales generalizados simples

## Modelos lineales generalizados 

# sin variables
m0 <- glm(n_events~ 1, # Formula
          data = sp_glmdata, # datos
          family = "poisson") # tipo de distribución

# la frecuencia de registro afectada por la distancia a cultivo
m1 <- glm(n_events~ Dcrops, 
          data = sp_glmdata, family = "poisson")

# la frecuencia de registro afectada por el verdor de la vegetación
m2 <- glm(n_events~ MSAVI, 
                    data = sp_glmdata, family = "poisson")

# la frecuencia de registro afectada por la pendiente
m3 <- glm(n_events~ Slope, 
                    data = sp_glmdata, family = "poisson") 
          #family = "poisson")

# la frecuencia de registro afectada por la distancia a poblados
m4 <- glm(n_events~ Dpop_G, 
                    data = sp_glmdata, family = "poisson")


# la frecuencia de registro afectada por el tipo de habitat
m5 <- glm(n_events~ Habitat, 
             data = sp_glmdata, family = "poisson" )

lista_mods <- list(m0, m1, m2, m3, m4, m5)
mod_names <- c("freq~ 1",
               "freq~ D_cultivos",
               "freq~ MSAVI",
               "freq~ Slope",
               "freq~ D_poblado",
               "freq~ Habitat"
               )

AIC <- aictab(lista_mods, # Lista de modelos
              modnames = mod_names,  # nombres
              second.ord = F, # Seleccionar AIC
              sort = T) # Ordenar por menor valor

View(AIC)

# para inspeccionar el modelo usar la función summary
summary(m5)

# Dado al carácter relativo del AIC es necesario verificar que el mejor modelo es un buen modelo. Un mal ajuste puede ser causado por la existencia de autocorrelación en los residuales.

# Residuales del mejor modelo ---------------------------------------------

# Debido a que es un GLM de familia poisson los residuales no están definidos como en un modelo lineal simple. La función simulateResidual permite obtener una aproximación de residuales escalados para este tipo de modelos

# Test de sobredispersión
testDispersion(m5)

# Verificamos visualmente que el modelo cumpla los requisitos de la distribución
residuales <- simulateResiduals(fittedModel = m5, plot =F)

plotQQunif(residuales)


## Hacer el mapa de residuales

# Creamos el data.frame pcon las coordenadas y residuales
data_resm5 <- data.frame(res=residuals(residuales), # Residuales que creamos
                         x= sp_glmdata$utm_x,# coordenadas en x
                         y= sp_glmdata$utm_y) # coordenadas en y

# Mapa de los residuales
(map_res <- ggplot(UMA)+ # shape de uma
    geom_sf()+ # Graficar objeto espacial
    geom_point(data= data_resm5, aes(x=x, y=y, # Agregar puntos
                                   size= res, color= res), # Tamaño y color
               alpha=0.9)+ # Transparencia
    scale_size(range = c(1,15))+ # Escala del radio para los puntos
    labs(title= "Mapa de residuales modelo m5",
         y= "Lat",
         x= "Lon",
         size= "Número de \ndetecciones",
         color=NULL)+
    annotation_scale(location = "bl", width_hint = 0.5, # Barra de escala
                     line_width=2, tick_height=10) +
    annotation_north_arrow(location = "tr", # Flecha de norte
                           height = unit(2.5, "cm"), width = unit(2.5, "cm"),
                           pad_x = unit(1, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering)+
    theme_bw()+
    theme(text=element_text(size=15, family = "special"))+
    theme(text = element_text(family = 'firasans', size = 16), # tipo de letra
          plot.title.position = 'plot', # Marco de posición
          plot.title = element_text(face = 'bold', # Negrillas 
                                    margin = margin(t = 2, r = 0, b = 7, l = 0, unit = "mm"), # Margenes
                                    hjust = 0.5)))



# Verificando autocorrelación espacial ---------------------------------------------

# El test de Moran I básicamente es la extensión de una prueba de correlación de Pearson para las diferentes distancias entre unidades de muestreo. El objetivo es estimar el grado de correlación espacial de una misma variable. Cuando ploteamos el valor de I para las diferentes distancias tenemos un correlograma espacial

# Hay diferentes paquetes que permiten generar los correlogramas, cada una tiene sus virtudes y puede ser más conveniente o no, dependiendo de la capácidad de programación del usuario y los datos

# Primero vamos a generar la matriz de distancia (algunos paquetes lo hacen automáticamente)

Wdist <- data.frame(x= sp_glmdata$utm_x,
                    y= sp_glmdata$utm_y) %>% 
  dist() %>% 
  as.matrix()

# Para asegurar que cada categoría de distancia tenga suficientes pares se recomienda trabajar con 2/3 de la máxima distancia. En este caso la máxima distancia es ~ 8km

maxd <- 2/3* max(Wdist)


## pgirmess
# pgirmess llama las funciones de spdep con una sintaxis mas amigable. Esto también hace que sea menos flexible. Esta aproximación asume que la variable de respuesta se distribuye de manera normal para calcular los p valores


pgirmess_correg <- pgirmess::correlog(coords = cbind(data_resm5$x, # coordenada x
                                                     data_resm5$y), # coordenada y
                                      z= data_resm5$res, # residuales
                                      method = "Moran", # Tipo demétodo
                                      nbclass = NULL) # Automáticamente el número de pares

pgirmess_correg

## ncf
# La ventaja del paquete ncf es que permite generar correlogramas usando un test de significancia no paramétrico, por medio simulaciones monte Carlo


ncf_correg <- ncf::correlog(x=data_resm5$x, # coordenadas en x
                            y=data_resm5$y, # coordenadas en y
                            z=data_resm5$res, # variable de interés,
                            latlon =  FALSE,
                            na.rm=T, # en caso de NAs
                            increment = 700, # Distancia mínima de unidades
                            resamp=500)

# Modificamos data frame para la gráfica
corbase <- data.frame(coef=ncf_correg$correlation, 
                     dist.class= ncf_correg$mean.of.class, 
                     p.value= ncf_correg$p,
                     n= ncf_correg$n )%>% 
  bind_rows(as.data.frame(pgirmess_correg)) %>% 
  mutate(package= c(rep("ncf", 12), rep("pgirmess",12)),
         p_valor= if_else(p.value<0.025, "significativo", "no-significativo"))
  
  
# Graficamos con ggplot

(ggplot(corbase ,aes(x=dist.class, y=coef, group= package))+
    geom_hline(yintercept = 0, linetype= "dashed")+
    geom_line(size=0.9, colour="black")+
    geom_point(aes(fill= package, shape=p_valor), 
               shape= 21, size=5, stroke = 1.5)+
    ylim(-1,1)+
    scale_x_continuous(breaks = seq(0,8000, by=500),limits = c(500,6000))+
    labs(x= "Unidades de distancia (m)", y= " Moran I", 
         title = " Correlograma de residuales",
         colour= "Librería")+
    theme_classic()+
    theme(text = element_text(family = 'firasans', size = 20), # tipo de letra
          plot.title.position = 'plot', # Marco de posición
          plot.title = element_text(face = 'bold', # Negrillas 
                                    margin = margin(t = 2, r = 0, 
                                                    b = 7, l = 0, 
                                                    unit = "mm"), # Margenes
                                    hjust = 0.5),
          legend.position = c(0.8, 0.8))) # posición de la legenda


# Sp line -----------------------------------------------------------------

# ncf también permite ajustar splines cubicas por medio de bootstrap, en este caso no es necesario definir cortes de pares de distancias y permite obtener intervalos de confianza

spline <- ncf::spline.correlog(x=data_resm5$x, # coordenadas en x
                               y=data_resm5$y, # coordenadas en y
                               z=data_resm5$res,
                               xmax = maxd, # Máxima distancia
                               type = "boot",
                               resamp = 500)# Remuestreos
plot(spline)
summary(spline)

spl_dist <- as.data.frame(spline[["boot"]][["boot.summary"]][["predicted"]][["x"]]) %>% 
  t()

sp_base <- spline[["boot"]][["boot.summary"]][["predicted"]][["y"]][c(2,6,10),] %>% t() %>% 
  as_tibble() %>% 
  mutate(dist= spl_dist[,1])
  
(splineplot <- ggplot(sp_base, aes(x= dist, y= `0.5`))+
  geom_ribbon(aes(ymin= `0.025`, ymax= `0.975`),
              fill= "lightgray")+
  geom_line(size=1)+
  ylim(-1,1)+
  scale_x_continuous(breaks = seq(0,8000, by=500),
                     limits = c(min(sp_base$dist), max(sp_base$dist)),
                     expand = c(0,0))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = 500, linetype=2, colour= "blue", size=0.8)+
  annotate(geom = "curve", x = 900, y = 0.7, xend = 530, yend = 0.5, 
      curvature = .3, arrow = arrow(length = unit(4, "mm")), size=0.8) +
  annotate(geom = "text", x = 930, y = 0.7, size= 5, 
           label = "Distancia mínima", hjust = "left")+
  labs(x= "Unidades de distancia (m)", y= " Moran I", 
       title = " Spatial line de residuales")+
  theme_classic()+
  theme(text = element_text(family = 'firasans', size = 20), # tipo de letra
        plot.title.position = 'plot', # Marco de posición
        plot.title = element_text(face = 'bold', # Negrillas 
                                  margin = margin(t = 2, r = 0, 
                                                  b = 7, l = 0, 
                                                  unit = "mm"), # Margenes
                                  hjust = 0.5),
        legend.position = c(0.8, 0.8))) # posición de la legenda


# Lo que podemos concluir con todas las aproximaciones vistas es que parece ser que no hay una autocorrelación espacial fuerte de los residuales de nuestro modelo m5. Esto quiere decir que las desviaciones de la normalidad se deben a otro factor.

## Podemos intentar otra especificación del modelo
#Ajustamos otro modelo que asumen una distribución de error *binomial negativa*. Con ello nos damos cuenta que el problema era el tipo de distribución.

m5bn <- glm.nb(n_events~ Habitat, data = sp_glmdata)

testDispersion(m5bn)

residuales.bn <- simulateResiduals(fittedModel = m5bn, plot =F)

plotQQunif(residuales.bn)

# Sorpresa

# Recuerden ver las lecturas al final de la página web

# Fin script ---------------------------------------------------------------------


