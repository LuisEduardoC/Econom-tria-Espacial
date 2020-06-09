library(tidyverse)
library(haven)
install.packages("survey")
SALUD <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/CSALUD01.SAV", encoding = 'UTF-8')
 <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/CSALUD01.SAV", encoding = 'UTF-8')
SALUD <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/CSALUD01.SAV", encoding = 'UTF-8')
MUJER_OBS <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/RE223132.SAV", encoding = 'UTF-8') 
MUJER_LAC <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/REC42.SAV", encoding = 'UTF-8')
MUJER_ANT <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/RECH5.SAV", encoding = 'UTF-8')
PERSONA   <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/RECH1.SAV", encoding = 'UTF-8') 
VIVIENDA  <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/RECH23.SAV", encoding = 'UTF-8')
HOGAR     <- read_sav("C:/Users/ADMIN/Desktop/paperUAC/RECH0.SAV", encoding = 'UTF-8')

MUJER_OBS$HHID      <- str_sub(MUJER_OBS$CASEID,1,(str_length(MUJER_OBS$CASEID)-3)) 
MUJER_OBS$QSNUMERO  <- str_sub(MUJER_OBS$CASEID,-2,-1)
MUJER_OBS$QSNUMERO  <- as.numeric(MUJER_OBS$QSNUMERO)
MUJER_LAC$HHID      <- str_sub(MUJER_LAC$CASEID,1,(str_length(MUJER_LAC$CASEID)-3)) 
MUJER_LAC$QSNUMERO  <- str_sub(MUJER_LAC$CASEID,-2,-1)
MUJER_LAC$QSNUMERO  <- as.numeric(MUJER_LAC$QSNUMERO)
PERSONA$QSNUMERO    <- PERSONA$HVIDX 
MUJER_ANT$QSNUMERO  <- MUJER_ANT$HA0

BASE1 <- left_join(PERSONA, SALUD, by = c("HHID","QSNUMERO"))
BASE2 <- left_join(MUJER_OBS, MUJER_LAC, by = c("HHID", "QSNUMERO")) 
BASE2 <- left_join(BASE2, MUJER_ANT, by = c("HHID", "QSNUMERO"))
BASE3 <- left_join(HOGAR, VIVIENDA, by = "HHID")
BASE4 <- left_join(BASE1,BASE2,by = c("HHID","QSNUMERO")) 
BASE5 <- left_join(BASE3,BASE4,by = 'HHID')

BASE_FINAL <- filter(BASE5,V213!= 1 | is.na(V213)) # Descartamos a las gestantes
BASE_FINAL <- filter(BASE_FINAL,QSRESINF == 1) 

rm(SALUD,BASE1,BASE2,BASE3,BASE4,BASE5,PERSONA,HOGAR,VIVIENDA,MUJER_LAC,
   MUJER_OBS, MUJER_ANT)

BASE_FINAL$PAMS <-(BASE_FINAL$QS905S+BASE_FINAL$QS903S)/2 # Obtenemos la PAM sistólica de dos mediciones 
BASE_FINAL$PAMD <-(BASE_FINAL$QS905D+BASE_FINAL$QS903D)/2 # Obtenemos la PAM diastólica de dos mediciones
BASE_FINAL$HIPERTENSION  <- (BASE_FINAL$PAMS>=140) | (BASE_FINAL$PAMD>=90)

BASE_FINAL <- BASE_FINAL %>%  
  mutate(PESO = ifelse(QS902 == 1 & (QS900 >= 1 & QS900 < 999), QS900,
                       ifelse(QS902 == 4, HA2/10, NA)),
         TALLA = ifelse(QS902 == 1 & (QS901 >= 1 & QS901 < 999), QS901, 
                        ifelse(QS902 == 4, HA3/10, NA))) %>% 
  mutate(IMC = (PESO/(TALLA^2))*10000) %>% 
  mutate(OBESIDAD = IMC >= 30)

BASE_FINAL <- BASE_FINAL %>%
  mutate(fruta_dxs = ifelse(QS213U == 1, QS213C, NA), fruta_uxd = QS214C, 
         fruta_semana = fruta_dxs * fruta_uxd)


BASE_FINAL <- BASE_FINAL %>%
  mutate(fruta_dxs = ifelse(QS213U == 1, QS213C, NA), fruta_uxd = QS214C, 
         fruta_semana = fruta_dxs * fruta_uxd)

BASE_FINAL <- BASE_FINAL %>% 
  mutate(jugof_dxs = ifelse(QS215U == 1, QS215C, NA), jugof_vxd = QS216C,
         jugof_semana = jugof_dxs * jugof_vxd * 2)

BASE_FINAL <- BASE_FINAL %>% 
  mutate(ensaladaf_dxs = ifelse(QS217U == 1, QS217C, NA), ensaladaf_pxd = QS218C,
         ensaladaf_semana = ensaladaf_dxs * ensaladaf_pxd * 2)

BASE_FINAL <- BASE_FINAL %>%
  mutate(ensaladav_dxs = ifelse(QS219U == 1, QS219C, NA), ensaladav_u = QS220U, 
         ensaladav_uxd = coalesce(as.numeric(QS220CV), as.numeric(QS220CC)),
         ensaladav_porcion = ifelse(ensaladav_u == 2, ensaladav_uxd / 4,
                                    ifelse(ensaladav_u == 1, ensaladav_uxd, NA)), 
         ensaladav_semana = ensaladav_dxs * ensaladav_porcion)


BASE_FINAL <- BASE_FINAL %>% 
  mutate(frutaensalada_pxs = rowSums(cbind(ensaladav_semana,ensaladaf_semana,
                                           jugof_semana, fruta_semana),  
                                     na.rm = TRUE)) %>% 
  mutate(DIETA = ifelse(frutaensalada_pxs == 0, NA, 
                        as.numeric(frutaensalada_pxs >= 31.5))) 


ENDES <- select(BASE_FINAL, HHID, QSNUMERO, CONGLOMERADO = HV001, ESTRATO = HV022, 
                PONDERACION = PESO15_AMAS, SEXO = HV104, AREA_RESIDENCIA = HV025, 
                QUINTIL_BIENESTAR = HV270, REGION_NATURAL = SHREGION,HIPERTENSION, PESO, 
                TALLA, IMC, OBESIDAD,DIETA)

diseño <- svydesign(id =~ CONGLOMERADO, strata =~ ESTRATO, weights=~ PONDERACION, data=ENDES) 

svyby(~HIPERTENSION,by=~AREA_RESIDENCIA,design = diseño,FUN=svyciprop,vartype=c('se','ci')) 
svyby(~HIPERTENSION,by=~REGION_NATURAL,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
svyby(~HIPERTENSION,by=~SEXO,design = diseño,FUN=svyciprop,vartype=c('se','ci'))
Hipertensión <- svyby(~HIPERTENSION,by=~SEXO+QUINTIL_BIENESTAR,design = diseño,FUN=svyciprop,vartype=c('se','ci'))


svyby(~OBESIDAD,by=~AREA_RESIDENCIA,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
svyby(~OBESIDAD,by=~REGION_NATURAL,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T)
svyby(~OBESIDAD,by=~SEXO,design = diseño,FUN=svyciprop,vartype=c('se','ci'),na.rm=T) 
Obseidad <- svyby(~OBESIDAD,by=~SEXO+QUINTIL_BIENESTAR,design = diseño,FUN=svyciprop,
                  vartype=c('se','ci'),na.rm=T)

Hipertensión <- set_names(Hipertensión, c('SEXO','QUINTIL_BIENESTAR','y','se','ci_l','ci_u')) 
Obesidad <- set_names(Obseidad, c('SEXO','QUINTIL_BIENESTAR','y','se','ci_l','ci_u'))
QUINTIL_BIENESTAR <- combine(Hipertensión,Obesidad)
QUINTIL_BIENESTAR <- mutate(QUINTIL_BIENESTAR, SEXO = ifelse(SEXO==1,'Masculino','Femenino'))


ggplot(QUINTIL_BIENESTAR, aes(x=QUINTIL_BIENESTAR, y=y, colour = as.factor(SEXO))) +
  geom_point(size = 2) + facet_grid(source ~ as.factor(SEXO)) +
  geom_errorbar(width=.1, aes(ymin=ci_l, ymax= ci_u)) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.35)) +
  labs(x = 'Quintil de bienestar', y = 'Prevalencia')  +
  theme(legend.position="none")













































