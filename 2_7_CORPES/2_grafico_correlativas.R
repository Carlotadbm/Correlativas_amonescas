###Carlota de Benito Moreno
###21/8/2022
#Comparativas proprocionales en CORPES XXI ----
###Apartado 2

#cargar librerías
library(tidyverse)
library(wesanderson)

# leer tablas
corpes_amon <- read_delim("CORPES_correlativas_amonescas.csv", delim = ";")
corpes_normal <- read_delim("CORPES_correlativas_normales.csv", delim = ";")


## fechas del corpus ----
corpes_amon_validas <- corpes_amon %>% 
  filter(Valida == "si") %>% 
  mutate(corpus = "CORPES XXI",
         ID = seq_along(BIBLIOGRAFIA), 
         tipo = "Paratáctica") %>% 
  select(ID, tipo, corpus, PAIS)

## países  ----
corpes_amon_validas %>% 
  count(PAIS)

## crear gráfico  ----
corpes_normal_validas <- corpes_normal %>% 
  filter(Valida == "si") %>% 
  mutate(corpus = "CORPES XXI",
         ID = seq_along(BIBLIOGRAFIA)) %>% 
  select(ID, cuantificador, corpus, PAIS) %>% 
  rename(tipo = cuantificador)
  
correlativas_orden <- c("Paratáctica", "entre", "mientras", "cuanto")

todas <- corpes_amon_validas %>% 
  add_row(corpes_normal_validas) 

todas$tipo <- factor(todas$tipo, correlativas_orden)

todas %>% 
  count(tipo) %>% 
  ggplot(aes(x = tipo, y = n, fill = tipo)) +
  geom_col(show.legend = F, position = "dodge") + 
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = 0.5) +
  scale_fill_manual(values = wes_palette("Royal1")[c(2, 1, 3, 4)]) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Frecuencias de las distintas comparativas correlativas en español (CORPES XXI)", 
       subtitle = "(Encabezadas por 'más' y con un verbo en el primer término.)", x = "Tipo de comparativa correlativa",
         y = "Número de ejemplos") +
  theme_bw() 
ggsave("CORPES_comparativa.png", width = 10, height = 4.37)

# ver cifras
todas %>% 
  count(tipo) %>%
  mutate(total = sum(n), 
         perc = round(n/total*100, 1))
