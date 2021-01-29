source("lib/libraries.r", encoding="UTF-8")

# metoda voditeljev
placa.bdppc <- left_join(place_Evropa,bdp_pc_Evropa) %>% drop_na()

podatki <- placa.bdppc %>%
  select(Placa, BDP_na_prebivalca) %>%
  as.matrix() %>%
  scale()

modelK <- kmeans(podatki, 3)

skupine <- placa.bdppc %>%
  mutate(skupina=modelK$cluster) %>%
  ggplot(aes(x=Placa/1000,y=BDP_na_prebivalca/1000, col=Leto, shape=factor(skupina))) + 
  geom_point(size=2) + 
  ylab("BDP na prebivalca * 1000 (v EUR)") + 
  xlab("Plača * 1000") + 
  labs(title = "Primerjava BDP na prebivalca in plač v evropskih državah",shape="Skupina") +
  theme_bw()
