install.packages("tidyverse",dependencies = TRUE)
library(tidyverse)
install.packages("nycflights13",dependencies = TRUE)
library(nycflights13)
filter(flights,month==10)
filter(flights,(month==10)&(day==10))

filter(flights,(year==2013)&(dep_delay<0))
filter(iris,(Species=="setosa")|(Species=="versicolor"))
iris[iris$Species==c("setosa","versicolor"),]

iris %>% 
  filter((Species=="setosa")|(Species=="versicolor")) %>%
  tibble()

flights %>% 
  select(year,month,day,dep_delay) %>% 
  filter(month==10) %>% 
  arrange(desc(dep_delay)) %>% 
  mutate(dep_delay_2=dep_delay*2)
dia<-30
mes<-2
año<-2020
paste0("hola mundo",nombre)
sprintf("%s-%s-%s",dia,mes,año)

flights %>% 
  select(year,month,day,dep_delay) %>% 
  filter(month==10) %>% 
  arrange(desc(dep_delay)) %>% 
  mutate(fecha=as.Date(sprintf("%s-%s-%s",year,month,day)))
