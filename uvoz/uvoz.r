library(readr)
library(dplyr)
library(tidyr)

uvoz1 <- read.csv2("podatki/SLO_place.csv", skip=2, na=c("z", "Z", "-")) 

moski <- uvoz1 %>% select(c(1, 2, 4, 7, 10, 13, 16, 19, 22)) %>% 
  rename("STAROST_SKUPAJ"=3, "15-64"=4, "15-24"=5,"25-34"=6, "35-44"=7, "45-54"=8, "55-64"=9) %>%
  mutate(SPOL="moški")

zenske <- uvoz1 %>% select(c(1, 2, 5, 8, 11, 14, 17, 20, 23)) %>% 
  rename("STAROST_SKUPAJ"=3, "15-64"=4, "15-24"=5,"25-34"=6, "35-44"=7, "45-54"=8, "55-64"=9) %>%
  mutate(SPOL="ženske")

skupaj <- uvoz1 %>% select(c(1, 2, 3, 6, 9, 12, 15, 18, 21)) %>% 
  rename("STAROST_SKUPAJ"=3, "15-64"=4, "15-24"=5,"25-34"=6, "35-44"=7, "45-54"=8, "55-64"=9) %>%
  mutate(SPOL="skupaj")

zdruzitev1 <- full_join(zenske, moski)
zdruzitev1 <- full_join(zdruzitev1, skupaj)
zdruzitev1 <- zdruzitev1[c(1,2,10,3,4,5,6,7,8,9)]
zdruzitev1 <- zdruzitev1 %>% rename("STATISTICNA_REGIJA"="STATISTIČNA.REGIJA") %>%
  mutate(LETO=parse_number(LETO)) %>% arrange(LETO, STATISTICNA_REGIJA)

View(zdruzitev1)
