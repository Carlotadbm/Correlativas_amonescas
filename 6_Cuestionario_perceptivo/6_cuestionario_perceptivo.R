###Carlota de Benito Moreno
###20/8/2022
#Análisis cuestionario perceptivo de las correlativas amonescas ----
###Apartado 6

#cargar librerías
library(tidyverse)
library(wesanderson)

## Preparar los datos  ----
###Leer tablas ----
#(hay dos, porque difundí dos cuestionarios distintos)
#las tablas solo contienen los participantes que contestaron todo
cuestionario_1 <- read_delim("cuestionario_marburgo_respuestas.csv", delim = "\t")
cuestionario_2 <- read_delim("cuestionario2_marburgo_respuestas.csv", delim = "\t")
cuestionario <- cuestionario_1 %>% 
  add_row(cuestionario_2)

#leer tablas de participantes
participantes_1 <- read_delim("cuestionario_marburgo_participantes.csv", delim = "\t")
participantes_2 <- read_delim("cuestionario2_marburgo_participantes.csv", delim = "\t")

### Juntarlas y limpiarlas  ----
participantes <- participantes_1 %>% 
  add_row(participantes_2) %>%
  #limpiar países (nos vamos a quedar solo con los españoles)
  mutate(country = trimws(country)) %>% 
  mutate(country = tolower(country)) %>% 
  mutate(country = ifelse(str_detect(country, "(^es|^bar)"), "españa", country)) %>% 
  #nos quedamos solo con los españoles
  filter(country == "españa") %>% 
  mutate(lengua = trimws(lengua)) %>% 
  mutate(lengua = tolower(lengua)) %>%
  #nueva columna con los que saben inglés/francés o italiano y los demás
  mutate(lengua_amonesca = ifelse(str_detect(lengua, "(ingl[eé]s|\\ben\\b|franc[ée]s|italiano)"), "en., fr., it.", 
                                  ifelse(str_detect(lengua, "\\d"), NA, "otras / ninguna"))) %>% 
  filter(!birthyear > 2002) 

### Cargar los inputs  ----
inputs_amon_1 <- read_lines("Input_amon.txt")
inputs_amon_2 <- read_lines("Input_amon2.txt")

### Limpiarlos  ----
# ponerlos en formato tabla y seleccionar solo las líneas con los ejemplos (los contextos no son relevantes)
inputs_amon_tb_1 <- inputs_amon_1 %>% 
  as_tibble_col("value") %>% 
  separate(value, into = c("ID_o", "content"), sep = ">") %>% 
  mutate(ID_o = str_c(ID_o, ">")) %>% 
  filter(str_detect(ID_o, "id:o")) %>% 
  mutate(Variante = c("Amonesca", "Estándar_VN", "Amonesca", "Estándar", "Estándar", "Amonesca", "Estándar_SV", "Amonesca"),
         Cuantificadores = rep(c("más_menos", "tanto_tanto", "más_más", "peor_mejor"), 2))

inputs_amon_tb_2 <- inputs_amon_2 %>% 
  as_tibble_col("value") %>% 
  separate(value, into = c("ID_o", "content"), sep = ">") %>% 
  mutate(ID_o = str_c(ID_o, ">")) %>% 
  filter(str_detect(ID_o, "id:o")) %>% 
  mutate(Variante = c("Estándar_NV", "Estándar_VS"),
         Cuantificadores = c("tanto_tanto", "más_más"))

### Juntarlos  ----
inputs_amon_tb <- inputs_amon_tb_1 %>% 
  add_row(inputs_amon_tb_2)

### Normalizar valores ----
#Normalizamos los valores de la columna natural, que es la escala de Lickert
#Para eso vamos a usar todos los asignados por el usuario (no solo los de las correlativas proporcionales)
cuestionario_limpio <- cuestionario %>% 
  #agrupamos por usuario
  group_by(id) %>% 
  #generamos z-scores
  mutate(natural_normalizado = scale(natural)) %>% 
  ungroup() %>% 
  #ahora ya seleccionamos solo los inputs de las de Amón
  right_join(inputs_amon_tb) %>% 
  #y nos quedamos con los participantes españoles
  right_join(participantes) %>% 
  #quitamos a un participante que respondió siempre que le sonaba natural y por eso es NaN
  filter(!is.na(natural_normalizado))


## Alternativas a tanto…cuanto ----
cuestionario_limpio %>% 
  filter(Variante == "Estándar_VN") %>% 
  filter(!is.na(alternativa)) %>% 
  select(alternativa) %>% 
  View()
  
## Análisis de la muestra de participantes seleccionada  ----
### Edad ----
summary(participantes$birthyear)

### Estudios ----
participantes %>% 
  mutate(estudios = ifelse(estudios == 1, "Educación Primaria", 
                           ifelse(estudios == 2, "Educación Secundaria",
                                  ifelse(estudios == 3, "Formación profesional",
                                         ifelse(estudios == 4, "Título Universitario",
                                                "Posgrado Universitario"))))) %>% 
  count(estudios) %>% 
  mutate(total = sum(n), 
         perc = round(n/total*100, 0))

### Lengua ----
participantes %>% 
  count(lengua_amonesca) %>% 
  mutate(total = sum(n), 
         perc = round(n/total*100, 0))

### Region ----
participantes %>% 
  mutate(region = tolower(region)) %>% 
  mutate(region = str_remove(region, "\\(.+\\)|, barc.+|soria, | me")) %>% 
  mutate(region = trimws(region)) %>% 
  mutate(region = str_replace_all(region, "ó", "o")) %>% 
  mutate(region = str_replace_all(region, "ny", "ñ")) %>% 
  mutate(region = ifelse(str_detect(region, "vasco|bizk|guip|eusk"), "país vasco", region)) %>% 
  mutate(region = ifelse(str_detect(region, "leon|salamanca"), "castilla y leon", region)) %>% 
  mutate(region = ifelse(str_detect(region, "sevilla|jaén|granada"), "andalucía", region)) %>% 
  mutate(region = ifelse(str_detect(region, "zaragoza"), "aragon", region)) %>% 
  mutate(region = ifelse(str_detect(region, "cuenca"), "castilla la mancha", region)) %>% 
  mutate(region = ifelse(str_detect(region, "ibiza"), "baleares", region)) %>% 
  count(region, sort = T) %>% 
  mutate(total = sum(n), 
         perc = round(n/total*100, 0))

## Análisis de los resultados ----
### Parámetro de naturalidad ----
cuestionario_limpio %>% 
  group_by(ID_c) %>% 
  select(-c("id", "time", "familiar", "alternativa")) %>% 
  ggplot() + 
  geom_boxplot(aes(y = natural_normalizado, x = Variante)) +
  labs(title = "¿Cómo de natural le suena la oración anterior?", 
       subtitle = "(1 = nada natural; 7 = completamente natural)",
       y = "") +
  theme_bw() +
  facet_wrap(~ Cuantificadores, scales = "free_x")

ggsave("naturalidad_cuestionario_amon.png")

#### ¿Hay diferencias según la lengua que hablan? ----
#No parece (especialmente teniendo en cuenta que los que no hablan inglés, francés o italiano 
#son bastantes menos). Solo en el caso de más-menos, me parece casualidad
cuestionario_limpio %>% 
  select(-c("id", "time", "familiar", "alternativa")) %>% 
  filter(!is.na(lengua_amonesca)) %>% 
  #count(lengua_amonesca)
  ggplot() + 
  geom_boxplot(aes(y = natural_normalizado, x = Variante)) +
  labs(title = "¿Cómo de natural le suena la oración anterior?", 
       subtitle = "(1 = nada natural; 7 = completamente natural)",
       y = "") +
  theme_bw() +
  facet_wrap(~ Cuantificadores + lengua_amonesca, scales = "free_x")

### Parámetro de familiaridad ----
#¿Usaría usted una expresión así?
#"1" Sí, podría decirlo así. 
#"2" Yo no lo diría así, pero conozco gente que la usa.
#"3" No, nunca he oído o leído algo así, pero la entiendo bien.
#"4" No, y me cuesta entender el significado. 
cuestionario_limpio %>% 
  select(-c("id", "time", "natural", "alternativa")) %>% 
  mutate(familiar = ifelse(familiar == 1, "Sí, podría decirlo así", 
                           ifelse(familiar == 2, "Yo no lo diría así, \npero conozco gente que la usa",
                                  ifelse(familiar == 3, "No, nunca he oído o leído algo así, \npero la entiendo bien",
                                         "No, y me cuesta entender \nel significado")))) %>% 
  mutate(familiar = ordered(familiar, levels = c("Sí, podría decirlo así", "Yo no lo diría así, \npero conozco gente que la usa", "No, nunca he oído o leído algo así, \npero la entiendo bien", "No, y me cuesta entender \nel significado"))) %>% 
  #mutate(Variante = ifelse(Variante == "Si", "Variante", "Amonesca")) %>% 
  group_by(Cuantificadores, Variante) %>% 
  count(familiar) %>% 
  ungroup() %>% 
  ggplot(aes(x = Variante, y = n, fill = familiar)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(title="¿Usaría usted una expresión así?", x="Variante", 
       y="Número de respuestas", fill="Respuestas") +
  scale_fill_manual(values=wes_palette(name="Royal2")[c(5, 1, 4, 3)]) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  facet_wrap(~ Cuantificadores, scales = "free_x") 

ggsave("familiaridad_cuestionario_amon.png")
