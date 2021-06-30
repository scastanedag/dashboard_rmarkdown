---
title: "Tablero Seguimiento Países"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
runtime: shiny

---

```{r setup, include=FALSE}
library(flexdashboard)
library(rio)
library(tidyverse)
```

```{r}
data<-import("https://covid.ourworldindata.org/data/owid-covid-data.csv", format ="csv")

data$date<-as.Date(data$date)
data$week<-as.numeric(strftime(data$date, format = "%U")) 
data$year<-strftime(data$date, format = "%y")

tasa.muertes<-function(A){
ggplot(filter(data,location==A)%>%
         group_by(week,year) %>%
         summarise(Tasa=(sum(new_deaths)/(sum(population)/n()))*1000000,n=n()),
       aes(x=week,y=Tasa))+
  geom_point(alpha=0.9,color="black")+
  geom_smooth(col="purple")+
  labs(title=A, y="tasa", x="semana")+
  facet_wrap(~year)
}

```

Tablero de Control  con el número de muertes por Covid-19 por millón de habitantes por semana

Column {data-width=200}
-----------------------------------------------------------------------

### Localización 

Selecciona localización 

```{r}

selectInput("lugar", "localización", unique(data$locat))
```

Column {data-width=800}
-----------------------------------------------------------------------

### Gráfico por localización

```{r}
renderPlot({tasa.muertes(input$lugar)})

```

