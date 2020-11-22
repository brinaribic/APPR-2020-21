library(readr)
library(dplyr)
library(tidyr)

# tabela s plačami po regijah glede na spol in starost

uvoz1 <- read.csv2("podatki/SLO_place.csv", skip=2, na=c("z", "Z", "-"),
                   encoding = "cp1250") %>%
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
place <- place %>% arrange(STATISTICNA_REGIJA) %>%
  mutate(STATISTICNA_REGIJA=parse_factor(STATISTICNA_REGIJA)) %>%
  mutate(SPOL=parse_factor(SPOL)) %>%
  mutate(STAROST=parse_factor(STAROST))


# tabela BDP za Slovenijo

uvoz2 <- read.csv2("podatki/SLO_BDP_regije.csv", skip=2, encoding = "cp1250")
uvoz2 <- uvoz2[-c(12:22),]
  
#MERITVE <- parse_character(uvoz2$MERITVE, locale=locale(encoding="cp1250"))
#MERITVE <- gsub("Mio EUR \\(fiksni tečaj 2007\\)", "BDP \\(mio EUR\\)", MERITVE)
#MERITVE <- gsub("Na prebivalca, EUR \\(tekoči tečaj\\)", "BDP na prebivalca \\(EUR\\)", MERITVE)

regije <- uvoz2 %>% pivot_longer(c(-1,-2), names_to = "REGIJE", values_to = "BDP")

MERITVE <- parse_character(regije$MERITVE, locale=locale(encoding="cp1250"))
MERITVE <- gsub("Mio EUR \\(fiksni tečaj 2007\\)", "BDP \\(mio EUR\\)", MERITVE)
MERITVE <- gsub("Na prebivalca, EUR \\(tekoči tečaj\\)", "BDP na prebivalca \\(EUR\\)", MERITVE)

bdp <- parse_number(regije$BDP)

REGIJE <- regije$REGIJE 
REGIJE <- gsub("\\.", "\\-", REGIJE)
REGIJE <- gsub("Jugovzhodna\\-Slovenija", "Jugovzhodna\\ Slovenija", REGIJE)

LETO <- regije$LETO

bdp_regije <- data.frame(MERITVE, LETO, REGIJE, bdp)
bdp_regije <- bdp_regije %>% 
  pivot_wider(names_from = LETO, values_from = bdp) %>%
  arrange(REGIJE)

View(bdp_regije)


# odstranila bi vse vrstice, v katerih se pojavlja "Struktura (Slovenija = 100%)"
# na prebivalca v EUR, drugače v MIO

  
