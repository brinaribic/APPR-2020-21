library(readr)
library(dplyr)
library(tidyr)

uvoz1 <- read.csv2("podatki/SLO_place.csv", skip=2, na=c("z", "Z", "-")) %>%
  mutate(LETO=parse_number(LETO))

spol <- function(stolpci, ime) {
  uvoz1 %>% select(stolpci) %>% 
    rename("Skupaj"=3, "15-64"=4, "15-24"=5,"25-34"=6, "35-44"=7, "45-54"=8, "55-64"=9) %>% 
    mutate(SPOL=ime) %>%
    pivot_longer(c(-1,-2,-10), names_to="STAROST", values_to="PLACE") %>%
    pivot_wider(names_from = LETO, values_from = PLACE)
}

zenske <- spol(c(1, 2, 5, 8, 11, 14, 17, 20, 23), "ženske")
moski <- spol(c(1, 2, 4, 7, 10, 13, 16, 19, 22),"moški")
skupaj <- spol(c(1, 2, 3, 6, 9, 12, 15, 18, 21),"Skupaj")

place <- full_join(moski, zenske)
place <- full_join(place, skupaj) %>% rename("STATISTICNA_REGIJA"="STATISTIČNA.REGIJA")
place <- place%>% arrange(STATISTICNA_REGIJA)

View(place)
