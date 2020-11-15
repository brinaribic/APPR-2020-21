library(readr)
library(dplyr)
library(tidyr)

stolpci <- c("Leto", "Regije", "Skupaj", "Moski", "Zenske")

uvoz1 <- read.csv2("podatki/SLO_place.csv", skip=2, col.names=stolpci)

place_skupaj <- uvoz1 %>% select(Leto, Regije, Skupaj) %>% 
  pivot_wider(names_from=Leto, values_from=Skupaj) %>% 
  rename("2019"=13)

place_moski <- uvoz1 %>% select(Leto, Regije, Moski) %>% 
  pivot_wider(names_from=Leto, values_from=Moski) %>% 
  rename("2019"=13)

place_zenske <- uvoz1 %>% select(Leto, Regije, Zenske) %>% 
  pivot_wider(names_from=Leto, values_from=Zenske) %>% 
  rename("2019"=13)


View(place_skupaj)
