#Usar base de datos de ENAHO
library(tidyverse)
library(haven)
library(survey)
library(flows)
library(stplanr)
library(dplyr)
OD=read_sav("C:/Users/ADMIN/Desktop/charo/Enaho01-2017-200.sav")
OD1<- select(OD, UBIGEO, P208A2, P203B, AÑO)
OD2<-filter(OD1, P203B==1)
OD3<- OD2 %>% mutate(AÑO = replace(AÑO, AÑO == 2017, 1))
head(OD3)
OD_FINAL<-select(OD3, i= UBIGEO, j=P208A2, fij=AÑO)
OD_FINAL
OD_FINAL1 <- as.numeric(OD_FINAL$i,OD_FINAL$j,OD_FINAL$fij)  
OD_FINAL1=na.omit(OD_FINAL)

myflows <- prepflows(mat = OD_FINAL1, i = "i", j = "j", fij = "fij")

write.csv(OD_FINAL1, file="C:/Users/ADMIN/Desktop/charo/OD.csv") #SE GUARDA PARA CAMBIAR NOMBRE I Y FIJ

datos=read.csv("C:/Users/ADMIN/Desktop/charo/OD.csv")
datos1=na.omit(datos)

myflows <- prepflows(mat = datos1, i = "i", j = "j", fij = "fij")
head(myflows)
myflows[1:10,1:10]

baselarga<-odmatrix_to_od(myflows)
sincerosBL=filter(baselarga, baselarga$flow>0)
write.csv(sincerosBL, file="C:/Users/ADMIN/Desktop/charo/sincerosBL.csv")
diag(myflows) <- 0
myflows
a<-colSums(myflows) 

namecol<-names(a)
m2<-matrix(a)
rownames(m2)<-namecol
m2
cnames <- c("j")


colnames(m2)
colnames(m2) <- cnames
m2
head(m2)



j<-rownames(m2)
b1<-as.data.frame(j)
head(b1)
c<-mutate(b1,m2)

d<-mutate(c,m2)


e<-merge(datos,c,by="j")
f<-e[!duplicated(e[,1]),]

f