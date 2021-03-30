##cargamos el data set 


library(readxl)

ruta_totalP <- "C:\\Users\\bodega\\Desktop\\datos de defunciones para limpieza\\data_set_ECP.xlsx"
data_set_ECP <- read_excel(ruta_totalP, sheet = "MUETRA1")

library(tidyverse)
library(gifski)
library(gganimate)
###### hacemos la limpieza de datos de nuestro data set
data_set_ECP$sexo <- factor(data_set_ECP$sexo,
                            levels = c(1, 2),
                            labels = c("hombre", "mujer"))

data_set_ECP <- data_set_ECP %>% 
  mutate(esc_fac = factor(escolarida,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 88, 99),
                          labels = c("sin estudios",
                                     "prescolar",
                                     "primaria trunca",
                                     "primaria",
                                     "secundaria trunca", 
                                     "secundaria", 
                                     "preparatoria trunca", 
                                     "preparatoria",
                                     "profesional", 
                                     "posgrado", 
                                     "no aplica",
                                     "no especificado")))

#### grafica de la distribucion por sexo y edad 
data_set_ECP %>% filter(escolarida<80) %>% 
  ggplot(data_set_ECP, 
         mapping = aes(x=escolarida, fill = esc_fac))+
  geom_bar() + facet_wrap(~sexo) + ylab("Conteo") + 
  xlab("Escolaridad") + theme_minimal() + 
  ggtitle("Nivel de escolaridad de fallecidos por sexo") 

### programamos una tabla dinamica 

library(reshape2)

data_set_ECP <- data_set_ECP %>% 
  mutate(sex2 = as.integer(factor(sexo, 
                                 levels = c("hombre", "mujer"),
                                 labels = c(1, 2))))

tapply(data_set_ECP$sex2, data_set_ECP$esc_fac, sum)

dmin <- dcast(data_set_ECP, ex2~escolarida, sum, value.var = "escolarida",) 

##analisis del segundo objetivo 
data_set_ECP$derechohab <- mutate(data_set_ECP, 
                                  derechohab = as_factor(derechohab))

data_set_ECP <- mutate(data_set_ECP,derechoabientes = factor(as.integer(derechohab),labels =  
                                                             c(1, 2, 3, 5, 7, 8, 9, 99),
                                                             levels = c("ninguna", "IMSS", "ISSTE",
                                                                        "Sedena",  "seguro Popular",
                                                                        "Otra", "Imms Oportunidades", 
                                                                        "no especificada")))
data_set_ECP$derechoabientes <- factor(data_set_ECP$derechoabientes,
                                       labels = c(1, 2, 3, 5, 7, 8, 9, 99),
                                       levels = c("ninguna", "IMSS", "ISSTE",
                                                  "Sedena",  "seguro Popular",
                                                  "Otra", "Imms Oportunidades", 
                                                  "no especificada"), 
                                       ordered = F)


ggplot(data_set_ECP, 
       mapping = aes(x = derechohab, fill=derechohab)) +
  geom_bar() 


## tercer revisar si existe una relacion entre la ocupacion y la escolaridad de los fallecidos

#1-medimos la correlacion 

cor(data_set_ECP$ocupacion, data_set_ECP$escolarida)

##0.09675672

#install.packages("GGally")

library(GGally)

##preparamos nuestro data_set nuevo

core_new <- select(data_set_ECP, esc_fac,ocupacion, escolarida) %>% 
  filter(escolarida<90 & ocupacion<90)

##medimos la correlacion y le damos formato

ggpairs(core_new, columns = 2:3, ggplot2::aes(colour=esc_fac))


###sacamos la tendencia de la calidad del aire 

APC_rut <- "C:\\Users\\bodega\\Desktop\\datos de defunciones para limpieza\\data_set_APC.xlsx"

APC <- read_excel(APC_rut)

ggplot(APC, mapping = aes(x = tiempo, y = MAXIMO)) + 
  geom_line(color="blue") +
  geom_abline(intercept = 85.7806, slope =  0.0138, col="black")+
  labs(title = 'maximo de contaminantes en Toluca el dia{frame_along}')+ 
  transition_reveal(FECHA)

#creamos la variable del tiempo y la unimos a APC
#para medir la tendencia 
  
tiempo <- c(1:1826)

APC <- cbind(APC, tiempo)

regLin <- lm(MAXIMO~tiempo, APC)

summary(regLin)



