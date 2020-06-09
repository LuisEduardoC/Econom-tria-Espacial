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
pal = colorNumeric("Oranges", BF_malaria_data$Casos)
leaflet(BF_malaria_data) %>% addTiles() %>% addCircleMarkers(~y, ~x, fillOpacity=2,
                                                             fillColor= ~pal(Casos), stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~Casos)

BF.dists <- as.matrix(dist(cbind(BF_malaria_data$y, BF_malaria_data$x)))
dim(BF.dists)
BF.dists.inv <- 1/BF.dists
diag(BF.dists.inv) <- 0
BF_malaria_data$log_odds <- logit(BF_malaria_data$Casos)
hist(BF_malaria_data$log_odds)

Moran.I(BF_malaria_data$Casos, BF.dists.inv,na.rm = TRUE)

#Aprox 2

maxDist<-max(dist(cbind(BF_malaria_data$Casos, BF_malaria_data$x)))
maxDist

xy=cbind(BF_malaria_data$x, BF_malaria_data$y)
pgi.cor <- correlog(coords=xy, z=BF_malaria_data$Casos, method="Moran", nbclass=10)
plot(pgi.cor)

coords<-coordinates(xy) # set spatial coordinates to create a spatial object
IDs<-row.names(as.data.frame(coords))
Neigh_nb<-knn2nb(knearneigh(coords, k=1, longlat = TRUE), row.names=IDs)
dsts<-unlist(nbdists(Neigh_nb,coords)) # returns the distance between nearest neighbors for each point
summary(dsts)
max_1nn<-max(dsts)

# We create different neighbor structures based upon distance
Neigh_kd1<-dnearneigh(coords,d1=0, d2=max_1nn, row.names=IDs)   # neighbors within maximum distance
Neigh_kd2<-dnearneigh(coords,d1=0, d2=2*max_1nn, row.names=IDs) # neighbors within 2X maximum distance

nb_1<-list(d1=Neigh_kd1, d2=Neigh_kd2) # list of neighbor structures
sapply(nb_1, function(x) is.symmetric.nb(x, verbose=F, force=T))

sapply(nb_1, function(x) n.comp.nb(x)$nc)

par(mfrow=c(2,1), mar= c(1, 0, 1, 0))
plot(xy, pch=16)
plot(Neigh_kd1, coords, col="blue",add=T)
plot(xy, pch=16)
plot(Neigh_kd2, coords,col="red", add=T)

weights<-nb2listw(Neigh_kd1, style="W")   # row standardized binary weights, using minimum distance for one neighbor
weights

I <-localmoran(BF_malaria_data$log_odds, weights, na.action=na.exclude)  

Coef<-printCoefmat(data.frame(I[IDs,], row.names=row.names(coords),
                              check.names=FALSE))
}
nci<-moran.plot(BF_malaria_data$log_odds, listw=weights, 
                xlab="Log prevalence", ylab="Spatially lagged log prev", labels=BF_malaria_data$id, pch=16, col="grey")
text(c(3,3, -5,-5),c(0.9, -1.9,0.9,-1.9), c("High-High", "High-Low", "Low-High", "Low-Low"), cex=0.8)

#otro tipo de mapa
mapa <- readOGR("C:/Users/ADMIN/Desktop/papaer UAC covid/db.shp")
plot(mapa)
names(mapa)
head(mapa)
names (mapa)[4] = "id"
BF_malaria_data <- read.csv("C:/Users/ADMIN/Desktop/charo/covi2.csv")
OA.Census <- merge(mapa, BF_malaria_data,by="id")
names(OA.Census)
proj4string(OA.Census) <- CRS("+init=EPSG:27700")
library(tmap)
library(leaflet)
qtm(OA.Census,fill ="Casos",fill.breaks=c(0,100,200,300,400,500,600,10000)) + tm_legend(legend.position = c("left", "bottom"))
tm_shape(OA.Census)+tm_polygons("Casos")
str(OA.Census)
sum(is.na(OA.Census))



qtm(mapa, fill = c("Casos"),
    col = "Median_income", palette = "BuGn", scale = 0.7, fill.title="Casos", title.font=1,
    fill.style="fixed", title.fontface=2,
    fill.breaks=round(c(seq(0, 10000, length.out=7),Inf)),0)


















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

