###Carlota de Benito Moreno
# Análisis datos exhaustivos de CORPES ----
### Apartado 7
### 8 de enero de 2023

#librerías
library(tidyverse)

# cargar tabla
corpes_xxi <- read_delim("correlativas_CORPESXX1_20220808.csv", delim = "\t", guess_max = 10000)

## Cuántas hay ----
#nos interesan aquellas que tengan por lo menos un verbo
corpes_xxi %>% 
  count(Valido, Con_verbo)

## Análisis amonescas ----
corpes_xxi_amonescas <- corpes_xxi %>% 
  filter(Valido %in% c("amonesca", "dudosa")) %>% 
  filter(Con_verbo %in% c("con verbo", "parcialmente")) %>% 
  #filter(!str_detect(Cuantificadores, "tanto_tanto")) %>% 
  mutate(Tipo_amonesca = ifelse(str_detect(Cuantificadores, "tanto_tanto"), "igualdad", "desigualdad")) 

## igualdad vs. desigualdad ----
corpes_xxi_amonescas %>% 
  group_by(Valido) %>% 
  count(Tipo_amonesca) %>% 
  mutate(perc_por_validez = round(n/sum(n)*100, 0)) %>% 
  ungroup() 

## Con comparativos de desigualdad ----
### de dónde son ----
corpes_xxi_amonescas %>% 
  filter(Tipo_amonesca == "desigualdad") %>% 
  filter(Valido == "amonesca") %>% 
  count(PAÍS, sort = T) %>% 
  mutate(perc = round(n/sum(n)*100, 0), 
         total = sum(n))
  
### Coordinadas vs. yuxtapuestas ----
corpes_xxi_amonescas %>% 
  filter(Tipo_amonesca == "desigualdad") %>% 
  filter(Valido == "amonesca") %>% 
  count(Coordinadas) %>% 
  mutate(perc = round(n/sum(n)*100, 0), 
         total = sum(n))
### Posición VS, exploración ----
corpes_xxi_amonescas %>% 
  filter(Tipo_amonesca == "desigualdad") %>% 
  filter(Valido == "amonesca") %>% 
  count(SV_apódosis) %>% 
  mutate(perc = round(n/sum(n)*100, 0), 
         total = sum(n))

## Con tanto ----
### De dónde son ----
corpes_xxi_amonescas %>% 
  filter(Tipo_amonesca == "igualdad") %>% 
  filter(Valido == "amonesca") %>% 
  count(PAÍS, sort = T) %>% 
  mutate(perc = round(n/sum(n)*100, 0), 
         total = sum(n))

### Coordinadas vs. yuxtapuestas ----
corpes_xxi_amonescas %>% 
  filter(Tipo_amonesca == "igualdad") %>% 
  filter(Valido == "amonesca") %>% 
  count(Coordinadas) %>% 
  mutate(perc = round(n/sum(n)*100, 0), 
         total = sum(n))

### Efectos contextuales ----
corpes_xxi_amonescas %>% 
filter(Tipo_amonesca == "igualdad") %>% 
  filter(Valido == "amonesca") %>% 
  count(Efectos_contextuales) %>% 
  mutate(perc = round(n/sum(n)*100, 0), 
         total = sum(n))
  

## Sin verbo (solo las no dudosas) ----
corpes_xxi_amonescas_sinverbo <- corpes_xxi %>% 
  filter(Valido == "amonesca") %>% 
  filter(Con_verbo == "sin verbo") 

### Cuantificadores ----
corpes_xxi_amonescas_sinverbo %>% 
  count(Cuantificadores)

### País ----
corpes_xxi_amonescas_sinverbo %>% 
  count(PAÍS)

### Zona ----
corpes_xxi_amonescas_sinverbo %>% 
  count(ZONA)

  
