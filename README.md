---
title: "Microplásticos en peces y sedimentos de la Reserva Natural de Usos Múltiples Monterrico"
output:
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: scroll
resource_files:
- nuevo.html
---

Microplásticos en peces
```{r setup, include = FALSE}
library(readxl)
library(flexdashboard)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(knitr)
library(scales)
library(urltools)
library(purrr)
library(readr)
library(devtools)
library(usethis)
library(Hmisc)
library(ggpubr)
```

```{r}
setwd("c://Users/Carlos Mazariegos/Documents/Microplásticos/DIGI 2020/Archivos informe (fotos y descripción)/base_datos/")


mps<- read_excel("regpeces_digi2021.xlsx", sheet = "BD")

sed<- read_excel("matriz_sed.xlsx", sheet = "Hoja1")

tamaño<- read_excel("matriz_sed.xlsx", sheet = "Tamaño")

forma<- read_excel("sedi_colfom.xlsx")
```


Dashboard
================================


Row
-------------------------------


### 

```{r}
valueBox(caption = "Peces con microplásticos",
         value = length(mps$MPS),
         icon = "fa-fish",
         color = "skyblue")
```


### 


```{r}
valueBox(caption = "Total de microplásticos registrados",
         value = sum(mps$MPS),
         icon = "fa-recycle",
         color = "red")
```


###

```{r}
valueBox(caption = "Frecuencia de ocurrencia general",
         value = round(((length(mps$N)/624)*100), 0),
         icon = "fa-percent",
         color = "green")
```

###
```{r}
esp<- mps %>%
  group_by(mps$Nombre_científico)

valueBox(caption = "Total de especies analizadas",
         value = 16,
         icon = "fa-fish",
         color = "green")
```

###
```{r}
esp<- mps %>%
  group_by(mps$Nombre_científico)

valueBox(caption = "Total de especies con microplásticos",
         value = 15,
         icon = "fa-fish",
         color = "brown")
```


Row
----------------------------------------

### Figura 1. Relación del índice de condición y la cantidad de microplásticos en Gobionellus microdon


```{r}
gobio<- mps %>%
  filter(mps$Nombre_científico =="Gobionellus microdon")

f1<- ggplot(gobio, aes(x = gobio$MPS, y = gobio$IC)) + geom_jitter() +
  geom_smooth(method = "lm", fill = "skyblue", alpha = 0.6) + theme_light() +
  xlab('Cantidad de microplásticos') + ylab('Índice de condición (g/cm^3)')

ggplotly(f1, height = 400, width = 650)
```

### Figura 2. Total de microplásticos por especie


```{r}
cien<- mps %>%
    group_by(Nombre_científico, Hábitat) %>%
    summarise(tot = sum(MPS),
              media = mean(MPS),
              ds = sd(MPS))

f2<- ggplot(cien, aes(y = reorder(cien$Nombre_científico, + cien$tot), x = cien$tot, fill = cien$Hábitat)) + geom_col() + theme(axis.text.y = element_text(face = "italic")) +   
  labs(y = "Nombre científico", x = "Promedio de microplásticos", fill = "Hábitat")
 
ggplotly(f2, height = 400, width = 650)
```


Row
-------------------------------------------------


### Figura 3. Microplásticos en relación al hábitat de los peces


```{r}
hab<- mps %>%
    group_by(Hábitat) %>%
    summarise(ha = sum(MPS),
              media_ha = mean(MPS),
              ds_ha = sd(MPS))

f3<- ggplot(mps, aes(x = Hábitat, y = MPS)) + 
  geom_jitter() + geom_violin(aes(fill = Hábitat), alpha = 0.8) + theme(legend.position = "below", axis.text = element_blank()) +
  scale_fill_discrete(guide = FALSE) + theme_light() + labs(y = "Cantidad de microplásticos")  

 
ggplotly(f3, height = 400, width = 650)
```


### Figura 4. Microplásticos en relación al hábito alimenticio de los peces

```{r}
feed<- mps %>%
    group_by(Hábito_alimenticio) %>%
    summarise(fh = sum(MPS),
              media_fh = mean(MPS),
              ds_fh = sd(MPS))

f4<- ggplot(mps, aes(x = Hábito_alimenticio, y = MPS)) + 
  geom_jitter() + geom_violin(aes(fill = Hábito_alimenticio), alpha = 0.8) + theme(legend.position = "below", axis.text = element_blank()) +
  scale_fill_discrete(guide = FALSE) + theme_light() + labs(y = "Cantidad de microplásticos")
 
ggplotly(f4, height = 400, width = 650)
```


Row
----------------------------------------------------------


###

```{r}
valueBox(caption = "Microplásticos en sedimentos",
         value = sum(sed$MPS),
         icon = "fa-calculator",
         color = "skyblue")
```


###

```{r}
valueBox(caption = "Seis sitios de muestreo dentro de la RNUMM",
         value = 6,
         icon = "fa-globe",
         color = "blue")
```

###

```{r}
valueBox(caption = "Microplásticos menores a 1 mm de tamaño",
         value = sum(sed$`<_1_mm`),
         icon = "fa-calculator",
         color = "red")
```

###


```{r}
valueBox(caption = "Microplásticos mayores a 1 mm de tamaño",
         value = sum(sed$`>_1mm`),
         icon = "fa-calculator",
         color = "green")
```


Row
--------------------------------------------

### Figura 5. Promedio de microplásticos por lugar de colecta.

```{r}
base <- theme(panel.background = element_blank(), axis.text.x = element_text(size = 12, angle = 45), axis.line = element_line(size = 1),
              axis.title = element_text(size = 12))

f5<- ggplot(sed, aes(x= PC, y = MPS)) + stat_summary(fun = mean, geom = "bar", fill = "skyblue", position = "identity") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 0.1), geom = "errorbar", width = 0.1, size = 1 ) +
  xlab("Punto de colecta") + ylab("Promedio de microplásticos") + base

ggplotly(f5, height = 400, width = 650)
```


### Figura 6. Composición de microplásticos en relación al tamaño por punto de colecta.

```{r}


f6<- ggplot(tamaño, aes(x = PC, y= MPS, fill = Tamaño)) + geom_bar(stat = "identity") + base + xlab("Punto de colecta") +
  ylab("Total de microplásticos") + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12)) + theme(axis.text.x = element_text(size = 12, angle = 45)) + scale_fill_manual(values = c("skyblue", "red"))

ggplotly(f6, height = 400, width = 650)
```


Row
--------------------------------------------


### Figura 7. Proporción de microplásticos por color. 

```{r}


f7<- ggplot(forma, aes(x = PM, y = Total, fill = Color)) + geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("yellow", "blue", "white", "brown", "grey", "purple", "black", "red", "skyblue", "darkgreen"), guide = guide_legend(title = "Color", size = 12)) +
  scale_y_continuous(labels = scales:: percent_format(accuracy = 1)) + coord_flip() + xlab("Punto de colecta") + ylab("Proporción de microplásticos") +
  theme(legend.text =element_text(size = 10), legend.title = element_text(size = 10), axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggplotly(f7, height = 400, width = 650)

```


### Figura 8. Proporción de microplásticos por forma.

```{r}
f8<- ggplot(forma, aes(x = PM, y = Total, fill = Tipo)) + geom_col(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("wheat3", "tomato3", "royalblue4", "olivedrab1"), guide = guide_legend(title = "Color", size = 12)) +
  scale_y_continuous(labels = scales:: percent_format(accuracy = 1)) + coord_flip() + ylab("Proporción de microplásticos") +
  theme(legend.text =element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.text.x = element_text(size = 12), axis.title = element_text(size = 12)) 

ggplotly(f8, height = 400, width = 650)

```
