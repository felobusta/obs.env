#Animación proyección poblacional 1992-2050 Chile

library(dplyr)      #uso de filter, select, mutate, mutate_at, group_by, vars, all_of
library(tidyr)      #cargar paquetes adicionales
library(readxl)     #cargar excel
library(tidyverse)  #cargar paquetes adicionales
library(ggplot2)    #realizar gráficos de barra
library(gganimate)  #animar

proyeccion_poblacion_comas <- read_excel("~CENSO/proyeccion_poblacion_comas.xlsx") #cargar y renombrar base

proy <- proyeccion_poblacion_comas 

proy<-proy %>%
  tidyr::pivot_longer(-EDAD, names_to = "ano", values_to = "poblacion") #pasar años de multiples columnas a columna de año y valores a columna poblacion


proy$EDAD<-as.numeric(recode(proy$EDAD,"100+"="100")) #variable "EDAD" character, etiqueta "100+" debe ser recodificada o pasará a NA

animate_1<-proy%>%
  group_by(ano, EDAD)%>%
  summarize(total_edad = sum(poblacion))%>%
  arrange(desc(ano))%>%
  mutate(EDAD3 = EDAD)%>%
  mutate_at(vars(all_of("EDAD3")),
            function(x) case_when(
              x>=0 & x<=14 ~ '0 a 14',
              x>=15 & x<=30 ~ '15 a 30',
              x>=31 & x<=45 ~ '31 a 45',
              x>=46 & x<=59 ~ '46 a 59',
              x>=60 ~ '60 o mÃ¡s',
            )
  )

animate_2<-animate_1%>%
  group_by(EDAD3, ano)%>%
  summarize(total_poblacion = sum(total_edad))

animate_2%>%
  group_by(ano)%>%
  arrange(ano,-total_poblacion)%>%
  mutate(rank = 1:n())->animate_by_year


my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))+
  theme(legend.position = "none")

millones_total<-animate_by_year%>%
  summarize(millones = sum(total_poblacion))
#%>%
#  mutate(porcentaje = total_poblacion/millones)


left_join(animate_by_year, millones_total, by = "ano")%>%
  mutate(porcentaje = round(total_poblacion/millones*100,1))%>%
  ggplot()+
  aes(xmin = 0 ,  
      xmax = porcentaje)+
  aes(ymin = rank - .45,
      ymax = rank + .45,
      y = rank)+
  facet_wrap(~ ano)+
  geom_rect(alpha = .7)+
  aes(fill = EDAD3)+
  scale_fill_viridis_d(option = "magma",
                       direction = -1)+
  scale_x_continuous(
    limits = c(-4, 35),
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35))+
  geom_text(col = "gray13",
            hjust = "right",
            aes(label = EDAD3),
            x = -0.1)+
  geom_text(aes(x=porcentaje, label = as.character(porcentaje)), 
            position = position_dodge(1),
            hjust = 0,
            size = 5)+
  scale_y_reverse()+
  labs(fill = NULL)+
  labs(x = 'Porcentaje')+
  labs(y = "",
       title = "Cambio poblacional segÃºn tramos de edad 1992-2050",
       caption = 'Fuente: proyecciÃ³n poblacional INE 1992-2050')+
  theme(legend.position = "none")->my_plot2


p2<-my_plot2 +  
  facet_null()+
  scale_x_continuous(
    limits = c(-4, 35),
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35))+
  geom_text(x = 30 , y = -5,
            family = "Times",
            aes(label = as.character(ano)),
            size = 15, col = "grey18")+
  aes(group = EDAD3) +  
  gganimate::transition_time(as.numeric(ano))

animate(p2, fps = 3)

anim_save("p2.gif")
anim_save("p2.avi")
