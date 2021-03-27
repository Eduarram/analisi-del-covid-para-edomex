
arch <- c(list.files())

## carga de las librerias

library(tidyverse)
library(lubridate)
########c19_csosNuevos son los casos nuevos detectados
c19_csosNuevos <- read.csv(arch[2])
c19_Decesos <- read.csv(arch[5])
c19_Hospitalizados <- read.csv(arch[3])
head(c19_Hospitalizados, 10)

c19_csosNuevos <- mutate(c19_csosNuevos, Fecha = as_date(Fecha)) 

#### a nivel nacional##
ggplot(c19_csosNuevos ,aes(x = Fecha, y = Nacional)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_bw()

###los dias del 01-02-2021 al 25 del del 03 del 2021

c19_csosNuevos %>% filter(Fecha > "2021-01-15") %>% 
  ggplot(aes(x = Fecha, y = Nacional)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_minimal() + 
  geom_vline(xintercept =  2021-03-08)
##mutamos la variable a fecha otra vez para que la detecte
c19_Hospitalizados <- mutate(c19_Hospitalizados, Fecha = as_date(Fecha))
###hospitalizados 
ggplot(c19_Hospitalizados ,aes(x = Fecha, y = Nacional)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_bw()

###hospitalizados filtrados a nivel nacional 

c19_Hospitalizados %>% filter(Fecha > "2021-01-15") %>% 
  ggplot(aes(x = Fecha, y = Nacional)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_minimal() + 
  geom_vline(xintercept =  2021-03-08)

###decesos a nivel nacional
c19_Decesos <- mutate(c19_Decesos, Fecha = as_date(Fecha)) 

##grafica de toda la pandemia
ggplot(c19_Decesos ,aes(x = Fecha, y = Nacional)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_bw()

###grafica de la pandemia filtrada desde febrero
c19_Decesos %>% filter(Fecha > "2021-01-15") %>% 
  ggplot(aes(x = Fecha, y = Nacional)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_minimal() + 
  geom_vline(xintercept =  2021-03-08)


