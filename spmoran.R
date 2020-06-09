require(spdep)
library(spdep)
library(rgdal)
library(sp)
library(spmoran)
data(boston)
y	<- completa[, "flow" ]
x	<- completa[,c("distance_hav")]

completa=read.csv("C:/Users/ADMIN/Desktop/charo/completa.csv")

coords  <- completa[,c("x_o","y_o")]
head(coords)

meig 	<- meigen(coords=coords)
esfD	<- esf(y=y,x=x,meig=meig, vif=5)
esfD$vif
esfD$b
esfD$s
esfD$e
