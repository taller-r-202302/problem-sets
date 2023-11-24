
## packages
rm(list=ls())
require(pacman)
p_load(tidyverse,rio, ## base
       coefplot,fixest,lmtest,modelsummary,margins, ## regresiones
       sf,osmdata,tmaptools,ggmap,ggsn, ## spatial 
       )

##=== punto 1 ===##

### Punto 1.1

## load data
data <- mtcars

## variable dependiente vs
modelo_1 <- lm(vs ~ mpg , data =data)
modelo_2 <- glm(vs ~ mpg*hp , data=data , family=binomial(link="logit")) 
modelo_3 <- lm(vs ~ mpg*hp , data =data)

### Punto 1.2

## tabla
tabla <- full_join(tidy(modelo_1),tidy(modelo_2),"term") %>% full_join(tidy(modelo_3),"term")
tabla_2 <- modelsummary(list(modelo_1,modelo_2,modelo_3) , output = "data.frame")

## grafico


### Punto 1.3 
export(tabla_2,"resultados.xlsx")

##=== punto 2 ===##

## polygono bogota
bog <- opq(bbox = getbb("Bogota Colombia")) %>%
       add_osm_feature(key="boundary", value="administrative") %>% 
       osmdata_sf()
bog <- bog$osm_multipolygons %>% subset(admin_level==9)

casa <- geocode_OSM("Casa de Narino, Bogota" , as.sf = T)

centro <- geocode_OSM("Centro Internacional, Bogota" , as.sf = T)


## add osm layer
osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(bog)), 
                           maptype="toner", source="osm", zoom=13) 

map <- ggmap(osm_layer) + 
       #geom_sf(data=bog , alpha=0.3 , inherit.aes=F) +
       geom_sf(data=casa, col="red" , inherit.aes=F) +
       geom_sf(data=centro, col="blue" , inherit.aes=F) +
       scale_color_manual(labels=c("A"="Casa de Nariño", ))
       geom_sf(data=centro, col="blue" , inherit.aes=F) + theme_linedraw() + labs(x="" , y="")
map

##=== punto 3 ===##

## polygono bogota
col <- st_read("MGN2021_DPTO_POLITICO/MGN_DPTO_POLITICO.shp")
index <- tibble(DPTO_CCDGO = col$DPTO_CCDGO , index_product=rnorm(33))
col <- left_join(col,index,"DPTO_CCDGO")

## plot
ggplot() + geom_sf(data=col , aes(fill=index_product)) +
viridis::scale_fill_viridis(option = "D" , name = "Índice de productividad" , direction=1)


