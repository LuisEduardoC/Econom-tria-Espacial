folderData=file.path("~","Users","ADMIN","Desktop")

library(rgdal)
library(raster)
library(ggplot2)
library(spatstat)
library(plotrix)
library(fields)
library(spdep)
library(leaflet)
library(plotGoogleMaps)
library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)
library(sp)
library(spatstat.data)
library(spatstat.utils)
library(Moran.I)
library("ape")
library(tensor)
library(abind)
library(polyclip)
library(goftest)
library(spatstat)
library(car)
library(pgirmess)
install.packages("pgirmess")

library(installr)
updateR()

vignette(getstart)


BF_malaria_data$log_odds <- logit(BF_malaria_data$CASOS)
hist(BF_malaria_data$CASOS)




BF_malaria_data <- read.csv("C:/Users/ADMIN/Desktop/charo/cov1.csv")
head(BF_malaria_data)

BF_Adm_1 <- raster::getData("GADM", country="PER", level=1)
BF_Adm_1 
proj4string(BF_Adm_1) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

pal
pal = colorNumeric("Oranges", BF_malaria_data$cum_conf)
leaflet(BF_malaria_data) %>% addTiles() %>% addCircleMarkers(~x, ~y, fillOpacity=1,
                                                             fillColor= ~pal(cum_conf), stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~cum_conf)

BF.dists <- as.matrix(dist(cbind(BF_malaria_data$y, BF_malaria_data$x)))
dim(BF.dists)
BF.dists.inv <- 1/BF.dists
diag(BF.dists.inv) <- 0
BF_malaria_data$log_odds <- logit(BF_malaria_data$cum_conf)
hist(BF_malaria_data$log_odds)

Moran.I(BF_malaria_data$log_odds, BF.dists.inv,na.rm = TRUE)

#para casos del peru observaciones por departamebtos

BF_malaria_data <- read.csv("C:/Users/ADMIN/Desktop/charo/covi2.csv")
head(BF_malaria_data)

BF_Adm_1 <- raster::getData("GADM", country="PER", level=1)
BF_Adm_1 
proj4string(BF_Adm_1) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

pal
pal = colorNumeric("Oranges", BF_malaria_data$P1)
leaflet(BF_malaria_data) %>% addTiles() %>% addCircleMarkers(~y, ~x, fillOpacity=2,
                                                             fillColor= ~pal(P1), stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~P1)

BF.dists <- as.matrix(dist(cbind(BF_malaria_data$y, BF_malaria_data$x)))
dim(BF.dists)
BF.dists.inv <- 1/BF.dists
diag(BF.dists.inv) <- 0
BF_malaria_data$log_odds <- logit(BF_malaria_data$P1)
hist(BF_malaria_data$log_odds)

Moran.I(BF_malaria_data$P1, BF.dists.inv,na.rm = TRUE)




























maxDist<-max(dist(cbind(BF_malaria_data$longitude, BF_malaria_data$latitude)))
maxDist

xy=cbind(BF_malaria_data$X, BF_malaria_data$Y)
pgi.cor <- correlog(coords=xy, z=BF_malaria_data$log_odds, method="Geary", nbclass=10)



maxDist<-max(dist(cbind(BF_malaria_data$X, BF_malaria_data$latitude)))
maxDist
xy=cbind(BF_malaria_data$X, BF_malaria_data$Y)
pgi.cor <- correlog(coords=xy, z=BF_malaria_data$log_odds, method="Moran", nbclass=10)

plot(xy, pch=16)
plot(Neigh_kd1, coords, col="green",add=T)
  hist(BF_malaria_data$CASOS)

