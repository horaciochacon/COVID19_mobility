library(tidyverse)
library(lubridate)
library(ggsci)
library(ggrepel)

# Lectura de base de datos + Rename de tipos de movimientos
peru <- read_csv("data/Global_Mobility_Report.csv") %>% 
  filter(country_region == "Peru") %>% 
  gather(key = tipo, value = change, -(country_region_code:date)) %>% 
  mutate(tipo = case_when(str_starts(tipo,"groce") ~ "Farmacia y supermercado",
                          str_starts(tipo,"parks") ~ "Parques",
                          str_starts(tipo,"residential") ~ "Domicilio",
                          str_starts(tipo,"retail") ~ "Restaurantes y recreación",
                          str_starts(tipo,"transit") ~ "Transporte",
                          str_starts(tipo,"workplace") ~ "Trabajo"
  ))

# Agrupar según departamento y resumir cambio para todo el movimiento posterior
# a la cuarentena y que no sea de categoría Domicilio
departamentos <- peru %>% 
  filter(!is.na(sub_region_1), 
         tipo != "Domicilio", 
         date >= ymd("2020-03-16")) %>% 
  group_by(sub_region_1) %>% 
  summarise(promedio = mean(change,na.rm = TRUE)) %>% 
  arrange(desc(promedio)) %>% 
  mutate(sub_region_1 = factor(sub_region_1, levels = .$sub_region_1))

# Reordenar factor según porcentaje de cumplimiento
peru <- peru %>% 
  mutate(sub_region_1 = factor(sub_region_1, 
                               levels = departamentos$sub_region_1))

peru %>% 
  filter(!is.na(sub_region_1)) %>% 
  ggplot(aes(x = date, y = change, colour = tipo)) +
  geom_line(size = 0.6) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-16")), linetype="dashed", 
             color = "black", size=1) +
  facet_wrap(. ~ sub_region_1) +
  scale_color_jco() +
  geom_label_repel(data = departamentos, aes(label= paste0(round(promedio,1),"%")), 
             x = Inf, y = -Inf, hjust= 3.5, vjust=-1,
             inherit.aes = FALSE, segment.size = 0) +
  xlab("Fecha") + ylab("% de cambio respecto a movimiento habitual") +
  theme_bw()


