library(tidyverse)
plot(iris$Sepal.Length,iris$Species[setosas])
iris %>% filter(Species=="setosas")


df<- tibble(
  fecha = rep(seq(
    as.Date("1981-01-01"),
    as.Date("2016-12-01"),
    by="1 month"
  ),2),
  estacion = c(rep("jefferson",432),rep("victor",432)),
  valor = rnorm(432*2, mean=20, 10)
)



df
plot(df)

ggplot(data = df, aes(x = fecha, y = valor, col=estacion)) +
  geom_line()

