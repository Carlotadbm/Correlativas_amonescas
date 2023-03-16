###Carlota de Benito Moreno
###18/8/2022
#Análisis diacrónico de las correlativas amonescas ----
###Apartado 5

#cargar librerías
library(tidyverse)
library(wesanderson)

#leer la tabla
amon_total <- read_delim("correlativas_amon_total_definitivo.csv", delim = "\t")

## Comparativa entre las amonescas y las estándar ----
#Limpiar la tabla de las no usadas para el análisis
#En este caso no limpiamos las citas (en el apartado 4 sí)
amon_total_limpia <- amon_total %>% 
  filter(!str_detect(ID_sentence, "suelto")) %>% #quitar los sueltos, ya que no sirven para comparar frecuencias, al no ser sistemáticos
  filter(cor_manual != "interesante") %>% #excluir los ejemplos que no son de correlativas
  filter(Interrumpida == "no") %>% 
  #quitamos las que no tienen verbo en ninguna de las dos oraciones, puesto que no es un contexto variable
  filter(!(SV_prótasis == "sin verbo" & SV_apódosis == "sin verbo"))

amon_total_diacronia <- amon_total_limpia %>% 
  mutate(ID_sentence = str_c(ID_sentence, source)) %>% 
  select(ID_sentence, date, cor_manual, Cita_traducción) %>% 
  mutate(cor_manual = ifelse(cor_manual == "amonesca", "amonesca", "estándar")) %>% 
  mutate(Cita_traducción_clean = ifelse(Cita_traducción == "cita de memoria", "no", 
                                  ifelse(str_detect(Cita_traducción, "fr|ing|ita"), "influencia FR/EN/IT", Cita_traducción))) %>% 
  mutate(Cita_traducción_clean = str_replace(Cita_traducción_clean, "no", "propia")) %>% 
  mutate(Ejey = "Ejey") 

min(amon_total_diacronia$date)
max(amon_total_diacronia$date)
amon_total_diacronia %>% 
  filter(cor_manual == "amonesca") %>% 
  slice_min(date, n = 10) %>% 
  View()


amon_total_diacronia %>% 
  ggplot(aes(x = date, y = Ejey, colour = cor_manual, shape = Cita_traducción_clean)) + 
  geom_point(position = position_jitter(height = 0.3), size = 5, alpha = 0.5) + 
  geom_vline(xintercept=as.Date("2005-04-02"), linetype=4) +
  geom_vline(xintercept=as.Date("1998-09-01"), linetype=4) +
  geom_text(aes(label = "04/2005"), x = as.Date("2006-05-02"), y = 1.4, colour = "black") +
  geom_text(aes(label = "09/1998"), x = as.Date("1999-11-01"), y = 1.45, colour = "black") +
  labs(title = "Comparativas proporcionales de Rubén Amón (5/6/1990-5/8/2022)", 
       y = "", x = "Fecha", colour = "", shape = "") +
  scale_color_manual(values=wes_palette(n = 2, "Darjeeling1")) +
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("correlativas_todas_diacronia.png", width = 10, height = 4.37)

## Diacronía de las amonescas por tipos  ----
amonescas_diacronia <- amon_total %>% 
  filter(cor_manual == "amonesca") %>% 
  filter(!str_detect(ID_sentence, "suelto")) %>% 
  mutate(ID_sentence = str_c(ID_sentence, source)) %>% 
  mutate(Tipo_amonesca = ifelse(Comparativos == "tanto_tanto", "igualdad", 
                                ifelse(str_detect(Comparativos, "tanto"), "mixta", "desigualdad"))) %>% 
  select(ID_sentence, date, Tipo_amonesca, Cita_traducción) %>% 
  mutate(Cita_traducción_clean = ifelse(Cita_traducción == "cita de memoria", "no", 
                                        ifelse(str_detect(Cita_traducción, "fr|ing|ita"), "influencia FR/EN/IT", Cita_traducción))) %>% 
  mutate(Cita_traducción_clean = str_replace(Cita_traducción_clean, "no", "propia")) %>% 
  mutate(Ejey = "Ejey") 

min(amonescas_diacronia$date)
max(amonescas_diacronia$date)

amonescas_diacronia %>% 
  filter(Tipo_amonesca != "desigualdad") %>% 
  slice_min(date, n = 5)

amonescas_diacronia %>% 
  ggplot(aes(x = date, y = Ejey, colour = Tipo_amonesca, shape = Cita_traducción_clean)) + 
  geom_point(position = position_jitter(height = 0.3), size = 5, alpha = 0.5) + 
  labs(title = "Comparativas proporcionales amonescas (19/4/2005-5/8/2022)", 
       y = "", x = "Fecha", colour = "", shape = "") +
  scale_color_manual(values=wes_palette(n = 3, "Darjeeling1", type = "continuous")) +
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("amonescas_diacronia.png", width = 10, height = 4.37)
