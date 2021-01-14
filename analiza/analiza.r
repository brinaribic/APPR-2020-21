# napoved plač 

model.plac <- lm(Placa ~ Leto, data=place)
p <- data.frame(Leto=c(2019, 2020, 2021))
napoved <- mutate(p, Placa=predict(model.plac,p))

graf.napoved1 <- ggplot(place, aes(x=Leto, y=Placa)) +
  geom_point() + 
  geom_smooth(method=lm, fullrange=TRUE) +
  geom_point(data=napoved, aes(x=Leto, y=Placa), color='red') +
  scale_x_continuous(breaks=2008:2021) +
  xlab("Place (v EUR)") +
  labs(title = "Napoved nadaljnih bruto plač v Sloveniji")

