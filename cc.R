BF_malaria_data <- read.csv("C:/Users/ADMIN/Desktop/charo/cov.csv")
BF_Adm_1 <- raster::getData("GADM", country="", level=1)
proj4string(BF_Adm_1) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')
BF_malaria_data$prevalence <- BF_malaria_data$positives / BF_malaria_data$examined

pal = colorNumeric("Oranges", BF_malaria_data$prevalence)
leaflet(BF_malaria_data) %>% addTiles() %>% addCircleMarkers(~longitude, ~latitude, fillOpacity=1,
                                                             fillColor= ~pal(prevalence), radius=~prevalence*10, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~prevalence)

hist(BF_malaria_data$prevalence)

BF_malaria_data$log_odds <- logit(BF_malaria_data$prevalence)
hist(BF_malaria_data$log_odds)
BF.dists <- as.matrix(dist(cbind(BF_malaria_data$longitude, BF_malaria_data$latitude)))
dim(BF.dists)

BF.dists.inv <- 1/BF.dists
diag(BF.dists.inv) <- 0

Moran.I(BF_malaria_data$log_odds, BF.dists.inv)   




BF_malaria_data <- read.csv("C:/Users/ADMIN/Desktop/charo/cov.csv")
head(BF_malaria_data)
BF.dists <- as.matrix(dist(cbind(BF_malaria_data$Longitud, BF_malaria_data$Latitud)))
dim(BF.dists)
BF.dists.inv <- 1/BF.dists
diag(BF.dists.inv) <- 0 
Moran.I(BF_malaria_data$CASOS, BF.dists.inv) 
data(meuse)
coordinates(BF_malaria_data) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")
w <- 1/as.matrix(dist(coordinates(BF_malaria_data)))

diag(w) <- 0
moran.test(BF_malaria_data$CASOS,mat2listw(w),zero.policy = NULL)

Moran.I(BF_malaria_data$CASOS, BF.dists.inv,
        alternative = "two.sided")




