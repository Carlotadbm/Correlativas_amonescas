###Carlota de Benito Moreno
###6/1/2023
#Análisis sincrónico de las correlativas amonescas ----
###Apartado 4 

#cargar librerías
library(tidyverse)
library(wesanderson)

#leer la tabla
amon_total <- read_delim("correlativas_amon_total_definitivo.csv", delim = "\t")

## Limpiar la tabla de las no usadas para el análisis ----
amon_total_limpia <- amon_total %>% 
  filter(!str_detect(ID_sentence, "suelto")) %>% #quitar los sueltos, ya que no sirven para comparar frecuencias, al no ser sistemáticos
  filter(cor_manual != "interesante") %>% #excluir los ejemplos que no son de correlativas
  filter(Cita_traducción == "no") %>% #excluir los ejemplos que son citas
  filter(Interrumpida == "no") #excluir las que llamo interrumpidas (falta comparativo en una de ellas)

## Proporción amonescas vs. estándar (dos tipos)  ----
amon_total_limpia %>% 
  count(cor_manual) %>% 
  mutate(cor_manual = fct_relevel(cor_manual, "inversa", after = 2)) %>% 
  ggplot(aes(x = cor_manual, fill = cor_manual, y = n)) +
  geom_col(show.legend = F) + 
  geom_text(aes(label = n), position = position_stack(vjust = .5)) +
  labs(title="Tipos de comparativas correlativas en los textos de Rubén Amón", 
       x="", y="Número de ocurrencias") +
  theme_bw() +
  scale_fill_manual(values=wes_palette(n = 3, "Darjeeling2"))

ggsave("correlativas_amon_tipos.png", width = 10, height = 4.37)

## contar cuántas hay sin verbo en los dos miembros  ----
amon_total_limpia %>% 
  filter(SV_prótasis == "sin verbo" & SV_apódosis == "sin verbo") %>% 
  count(cor_manual)

amon_total_limpia %>% 
  count(Introductor)


## Analizar las amonescas (incluimos los sueltos)  ----
amonescas_total <- amon_total %>% 
  filter(cor_manual == "amonesca") %>%
  filter(Cita_traducción == "no" | is.na(Cita_traducción))  #excluir los ejemplos que son citas o traducciones

### Comparativos  ----
amonescas_total %>%
  #limpiar las respuestas
  mutate(Comparativos = str_remove(Comparativos, "_y")) %>% #para las coordinaciones: quitamos el _y y se quedan dos segmentos iguales (siempre lo son)
  mutate(Comparativos = str_replace(Comparativos, "(_\\w+)\\1", "\\1")) %>% #los encontramos y remplazamos por solo uno de los segmentos: aquí si están en la segunda
  mutate(Comparativos = str_replace(Comparativos, "(\\w+_)\\1", "\\1")) %>% #los encontramos y remplazamos por solo uno de los segmentos: aquí si están en la primera
  mutate(Comparativos = str_replace(Comparativos, "_ ", "_")) %>% 
  separate(Comparativos, into = c("Comparativo_1", "Comparativo_2"), sep = "_") %>% 
  pivot_longer(cols = c("Comparativo_1", "Comparativo_2"), names_to = "Posicion", values_to = "Comparativos") %>% 
  distinct(Comparativos)

### Comparativos de desigualdad vs. tanto  ----
amonescas_total_2 <- amonescas_total %>%
  #limpiar las respuestas
  mutate(Comparativos = str_remove(Comparativos, "_y")) %>% #para las coordinaciones: quitamos el _y y se quedan dos segmentos iguales (siempre lo son)
  mutate(Comparativos = str_replace(Comparativos, "(_\\w+)\\1", "\\1")) %>% #los encontramos y remplazamos por solo uno de los segmentos: aquí si están en la segunda
  mutate(Comparativos = str_replace(Comparativos, "(\\w+_)\\1", "\\1")) %>% #los encontramos y remplazamos por solo uno de los segmentos: aquí si están en la primera
  mutate(Comparativos = str_replace(Comparativos, "_ ", "_")) %>% 
  mutate(Tipo_amonesca = ifelse(Comparativos == "tanto_tanto", "igualdad", 
                                ifelse(str_detect(Comparativos, "tanto"), "mixta", "desigualdad"))) 

amonescas_total_2 %>% 
  count(Tipo_amonesca) %>% 
  mutate(total = sum(n), 
         perc = round(n/sum(n)*100, 1))

### tipos de mixtas  ----
amonescas_total_2 %>% 
  filter(Tipo_amonesca == "mixta") %>% 
  count(Comparativos)

  
### ¿Proporcionalidad directa o inversa?  ----
amonescas_total_2 %>% 
  mutate(Proporcionalidad = ifelse(str_detect(Comparativos, "(tanto|más|mejor|mayor|antes)_(tanto|más|mejor|mayor|antes)"), "directa", 
                                   ifelse(str_detect(Comparativos, "(menos|peor)_(menos|peor)"), "directa", "inversa"))) %>% 
  count(Proporcionalidad) %>% 
  mutate(total = sum(n),
         perc = round(n/sum(n)*100, 1))

### Interpretación de tanto… tanto  ----
amonescas_total_2 %>% 
  count(Interpretación_tanto_tanto) %>% 
  
  filter(!is.na(Interpretación_tanto_tanto)) %>% 
  mutate(total = sum(n), 
         perc = round(n/sum(n)*100, 0))

### Modificados  ----
modificados <- amonescas_total_2 %>% 
  mutate(Modificados = str_remove(Modificados, "_y")) %>% 
  #distinct(Modificados) #%>% 
  separate(Modificados, into = c("Modificado1", "Modificado2", "Modificado3"), sep = "_") %>% 
  pivot_longer(cols = c("Modificado1", "Modificado2", "Modificado3"), names_to = "Posicion", values_to = "Modificados") %>% 
  filter(!is.na(Modificados)) %>% #quitamos los casos de NA (todos en Modificado3, la mayoría no tienen coordinadas)
  filter(Tipo_amonesca != "mixta") %>% #quitamos las mixtas, que son muy pocas
  filter(Modificados != "NP") %>% 
  group_by(Tipo_amonesca) %>% 
  count(Modificados) %>% 
  mutate(perc = round(n/sum(n)*100, 1)) 

modificados %>% 
  mutate(Tipo_amonesca = str_replace(Tipo_amonesca, "^d", "D")) %>% 
  mutate(Tipo_amonesca = str_replace(Tipo_amonesca, "^i", "I")) %>% 
  ggplot(aes(x = reorder(Modificados, n), fill = Modificados, y = perc)) +
  geom_col(show.legend = F) + 
  geom_text(aes(label = n), position = position_stack(vjust = .5)) +
  labs(title="Categoría gramatical de los elementos modificados por el \ncomparativo en las comparativas correlativas amonescas", 
       fill = "Elemento modificado", x="Elemento modificado", y="Porcentaje") +
  facet_wrap(~Tipo_amonesca, scales = "free_x") +
  theme_bw() +
  scale_fill_manual(values=wes_palette(n = 8, "Darjeeling1", type = "continuous"))

ggsave("modificados_prop.png", width = 10, height = 7)

### Orden SV  ----
#Cada fila será una de las dos oraciones correlativas
#Tener en cuenta las coordinadas

amonescas_total_orden <- amonescas_total_2 %>% 
  #los nombres los pongo para hacer un pivot_longer con dos sets de columnas (https://stackoverflow.com/questions/59253987/parallel-pivot-longer-of-two-sets-of-columns)
  separate(SV_prótasis, into = c("prótasis1_SV", "prótasis2_SV"), sep = "/") %>% 
  separate(SV_apódosis, into = c("apódosis1_SV", "apódosis2_SV"), sep = "/") %>% 
  separate(Función_Scomp_prótasis, into = c("prótasis1_FunciónScomp", "prótasis2_FunciónScomp"), sep = "_y_") %>% 
  separate(Función_Scomp_apódosis, into = c("apódosis1_FunciónScomp", "apódosis2_FunciónScomp"), sep = "(_y_|_)") %>% #hay yuxtapuestas, no solo coordinadas
  #queremos tener los valores de función y orden de la prótasis y la apódosis (=Oración 1 y 2) en la misma columna
  pivot_longer(13:20, names_to = c("Oración", ".value"), names_sep = "_") %>% 
  mutate(FunciónScomp = ifelse(FunciónScomp == "NA", NA, FunciónScomp)) %>% 
  filter(!is.na(FunciónScomp)) %>% #después de comprobarlas, podemos quitar las que tienen NA aquí
  #son las que sobran de los separate y el caso sin verbo
  mutate(Oración = ifelse(str_detect(Oración, "prótasis"), "Oración 1", "Oración 2"))%>% 
  #si la función es sujeto, el orden siempre es SV: lo quitamos
  filter(!str_detect(FunciónScomp, "S")) %>% 
  #si solo hay verbo, tiene poco interés: lo quitamos, igual que si es impersonal
  filter(!SV %in% c("V", "impersonal")) 


### Gráfico por tipo oracional y los dos miembros oracionales  ----
amonescas_total_orden %>% 
  group_by(Tipo_amonesca, Oración) %>% 
  count(SV) %>% 
  #quito las del comparativo separado
  filter(!str_detect(SV, "Comp")) %>%
  #quito las mixtas
  filter(Tipo_amonesca != "mixta") %>% 
  mutate(perc = round(n/sum(n)*100, 1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Oración, fill = SV, y = perc)) +
  geom_col(position = "dodge") + 
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = 0.5) +
  labs(title="Orden de sujeto y verbo en las comparativas correlativas amonescas", 
       fill = "Orden de palabras", x="", y="Porcentaje") +
  theme_bw() +
  #scale_fill_manual(values=wes_palette(n = 6, "Rushmore1", type = "continuous")) +
  scale_fill_manual(values = wes_palette("Royal2")[c(3, 1, 5, 2, 4)]) + 
  facet_wrap(~Tipo_amonesca)
  
ggsave("orden SV_prop.png", width = 10, height = 4.37)



# Comparar estándar y amonescas, pero usando todas (incluyendo los sueltos) para ver las posibilidades
#de las amonescas
### Modos ----
amon_total_limpia %>% 
  group_by(cor_manual, Modo_verbal_prótasis) %>%
  count(Modo_verbal_apódosis) 

amonescas_total %>% 
  #como hay una oración que tenía coordinada un caso sin verbo y otro con, tenemos que quitarlo
  #filter(str_detect(Modo_verbal_apódosis, "NA")) %>% View()
  mutate(Modo_verbal_apódosis = str_remove(Modo_verbal_apódosis, "NA_y_")) %>% 
  group_by(cor_manual, Modo_verbal_prótasis) %>%
  count(Modo_verbal_apódosis) 

### Tiempos: ¿son idénticos? ----
#todas
amon_total_limpia %>% 
  mutate(Tiempo_verbal_prótasis = str_c(Tiempo_verbal_prótasis, "_", Modo_verbal_prótasis),
         Tiempo_verbal_apódosis = str_c(Tiempo_verbal_apódosis, "_", Modo_verbal_apódosis),) %>% 
  mutate(Tiempo_verbal_idéntico = Tiempo_verbal_prótasis == Tiempo_verbal_apódosis) %>% 
  filter(!is.na(Tiempo_verbal_idéntico)) %>% 
  group_by(cor_manual) %>% 
  count(Tiempo_verbal_idéntico)

#amonescas
amonescas_total %>% 
  #como hay una oración que tenía coordinada un caso sin verbo y otro con, tenemos que quitarlo
  #filter(str_detect(Tiempo_verbal_apódosis, "NA")) %>% View()
  mutate(Tiempo_verbal_apódosis = str_remove(Tiempo_verbal_apódosis, "NA_y_")) %>% 
  mutate(Modo_verbal_apódosis = str_remove(Modo_verbal_apódosis, "NA_y_")) %>% 
  mutate(Tiempo_verbal_prótasis = str_c(Tiempo_verbal_prótasis, "_", Modo_verbal_prótasis),
         Tiempo_verbal_apódosis = str_c(Tiempo_verbal_apódosis, "_", Modo_verbal_apódosis),) %>% 
  mutate(Tiempo_verbal_idéntico = Tiempo_verbal_prótasis == Tiempo_verbal_apódosis) %>% 
  filter(!is.na(Tiempo_verbal_idéntico)) %>% 
  group_by(cor_manual) %>% 
  count(Tiempo_verbal_idéntico) %>% 
  mutate(Perc = round(n/sum(n)*100, 1),
         total = sum(n))


### Tiempos: ¿cuáles son? ----
#todas
amon_total_limpia %>% 
  mutate(Tiempo_verbal_prótasis = str_c(Tiempo_verbal_prótasis, "_", Modo_verbal_prótasis),
         Tiempo_verbal_apódosis = str_c(Tiempo_verbal_apódosis, "_", Modo_verbal_apódosis),) %>% 
  group_by(cor_manual, Tiempo_verbal_prótasis) %>%
  filter(!is.na(Tiempo_verbal_prótasis)) %>% 
  count(Tiempo_verbal_apódosis)
#amonescas
amonescas_total %>% 
  #como hay una oración que tenía coordinada un caso sin verbo y otro con, tenemos que quitarlo
  mutate(Tiempo_verbal_apódosis = str_remove(Tiempo_verbal_apódosis, "NA_y_")) %>% 
  mutate(Modo_verbal_apódosis = str_remove(Modo_verbal_apódosis, "NA_y_")) %>% 
  mutate(Tiempo_verbal_prótasis = str_c(Tiempo_verbal_prótasis, "_", Modo_verbal_prótasis),
         Tiempo_verbal_apódosis = str_c(Tiempo_verbal_apódosis, "_", Modo_verbal_apódosis),) %>% 
  group_by(cor_manual, Tiempo_verbal_prótasis) %>%
  filter(!is.na(Tiempo_verbal_prótasis)) %>% 
  count(Tiempo_verbal_apódosis)


### Marco sintáctico ----
amon_total_limpia %>% 
  group_by(cor_manual) %>% 
  count(Marco_sintáctico_oración)

amonescas_total %>% 
  count(Marco_sintáctico_oración)

# Relación con el código oral/escrito
amonescas_total %>%
  filter(Marco_sintáctico_oración != "principal") %>% 
  group_by(Marco_sintáctico_oración) %>% 
  count(source)

amonescas_total %>%
  filter(Marco_sintáctico_oración != "principal") %>% 
  filter(!str_detect(source, "T[ée]rtul")) %>%  
  View()
