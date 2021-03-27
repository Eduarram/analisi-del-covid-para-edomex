###se recomienda correr primero el script a nivel nacional

ggplot(c19_csosNuevos ,aes(x = Fecha, y = MÃ.xico)) +
  geom_line(color="blue") +
  geom_smooth(color="yellow", method = "lm") + theme_bw() +
  labs(x = "Fecha", y = "Estado de México") + 
  ggtitle("casos nuevos en el estado de México desde el inicio") 

###los dias del 01-02-2021 al 25 del del 03 del 2021

c19_csosNuevos %>% filter(Fecha > "2021-01-15") %>% 
  ggplot(aes(x = Fecha, y = MÃ.xico)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_minimal() + 
  geom_vline(xintercept =  2021-03-08) + 
  labs(x = "Fecha", y = "Estado de México") +
  ggtitle("casos nuevos en el estado de México de febrero a la fecha") 

##mutamos la variable a fecha otra vez para que la detecte
##en caso de no haberlo hecho antes lo hacemos 
##c19_Hospitalizados <- mutate(c19_Hospitalizados, Fecha = as_date(Fecha))
###hospitalizados 
ggplot(c19_Hospitalizados ,aes(x = Fecha, y = MÃ.xico)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_bw() +
  labs(x = "Fecha", y = "Estado de México")

###hospitalizados filtrados a nivel nacional 

c19_Hospitalizados %>% filter(Fecha > "2021-01-15") %>% 
  ggplot(aes(x = Fecha, y = MÃ.xico)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_minimal() + 
  geom_vline(xintercept =  2021-03-08) + 
  labs(x = "Fecha", y = "Estado de México")

###decesos a nivel nacional
##c19_Decesos <- mutate(c19_Decesos, Fecha = as_date(Fecha)) 
##grafica de toda la pandemia

ggplot(c19_Decesos ,aes(x = Fecha, y = MÃ.xico)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_bw() +
  labs(x = "Fecha", y = "Estado de México")

###grafica de la pandemia filtrada desde febrero

c19_Decesos %>% filter(Fecha > "2021-01-15") %>% 
  ggplot(aes(x = Fecha, y = MÃ.xico)) +
  geom_line(color="red") +
  geom_smooth(color="green") + theme_minimal() + 
  geom_vline(xintercept =  2021-03-08) + 
  labs(x = "Fecha", y = "Estado de México")

### histograma y densidad poblacional 
ggplot(c19_csosNuevos, mapping = aes(x = MÃ.xico, fill = "green")) + 
  geom_histogram(color = "pink") 

##eliminamos los 0ros

pop <- c19_csosNuevos %>% filter(MÃ.xico>0)
##sacamos los quantiles de la muestra
quantile(c19_csosNuevos$MÃ.xico)
##boxplot para outliers
boxplot(c19_csosNuevos$MÃ.xico)
## sacamos los rangos intercuantiles 
IQR(c19_csosNuevos$MÃ.xico)
##rango maximo y el rango minimo IQR = 386.5
max_Rc19 <- 633-1.5*386.5
min_Rc19 <- 633 + 1.5*386.5
##calculamos el rango de edades con 
range(c19_csosNuevos$MÃ.xico)
##no se encuentran en el rango del maximo(1212,75) y el minimo(53.25)
##por lo que deducimos que existen muchos datos atipicos.




