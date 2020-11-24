library(readr)
library(dplyr)
library(tidyr)

# tabela s plačami po regijah glede na spol in starost

uvoz1 <- read.csv2("podatki/SLO_place.csv", skip=2, na=c("z", "Z", "-"),
                   encoding="cp1250") %>%
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
place <- place %>%
  mutate(STATISTICNA_REGIJA=parse_factor(STATISTICNA_REGIJA, locale=locale(encoding="cp1250"))) %>%
  mutate(SPOL=parse_factor(SPOL,locale=locale(encoding="cp1250"))) %>%
  mutate(STAROST=parse_factor(STAROST,locale=locale(encoding="cp1250"))) %>%
  arrange(STATISTICNA_REGIJA)


# tabela BDP za Slovenijo

uvoz2 <- read.csv2("podatki/SLO_BDP_regije.csv", skip=2, encoding = "cp1250")
uvoz2 <- uvoz2[-c(12:22),]

regije <- uvoz2 %>% pivot_longer(c(-1,-2), names_to = "REGIJE", values_to = "BDP")

BDP <- parse_character(regije$MERITVE, locale=locale(encoding="cp1250"))
BDP <- gsub("Mio EUR \\(fiksni tečaj 2007\\)", "BDP \\(mio EUR\\)", BDP)
BDP <- gsub("Na prebivalca, EUR \\(tekoči tečaj\\)", "BDP na prebivalca \\(EUR\\)", BDP)
BDP <- parse_factor(BDP)

bdp <- parse_number(regije$BDP)

REGIJE <- parse_character(regije$REGIJE, locale=locale(encoding="cp1250"))
REGIJE <- gsub("\\.", "\\-", REGIJE)
REGIJE <- gsub("Jugovzhodna\\-Slovenija", "Jugovzhodna\\ Slovenija", REGIJE)
REGIJE <- parse_factor(REGIJE)

LETO <- regije$LETO

bdp_regije <- data.frame(BDP, LETO, REGIJE, bdp)
bdp_regije <- bdp_regije %>% 
  pivot_wider(names_from = LETO, values_from = bdp) %>%
  arrange(REGIJE) %>% rename("STATISTICNA_REGIJA"="REGIJE")

# tabela povprečnih plač v Evropi v EUR

library(rvest)

link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_average_wage"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% 
  html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[2]] %>% 
  html_table(fill=TRUE)

place_Evropa <- tabela[c(1, 12:22)]
place_Evropa <- place_Evropa[-c(1,2),]

colnames(place_Evropa) <- c("DRZAVA", 2008:2018)

place_Evropa <- place_Evropa %>% 
  mutate(DRZAVA=gsub("\\[[^]]*\\]","", DRZAVA), DRZAVA=parse_factor(DRZAVA)) %>%
  pivot_longer(-c(1), names_to = "LETO", values_to = "PLACE") %>%
  mutate(PLACE=parse_number(PLACE, na=c("-", "NA"))) %>%
  pivot_wider(names_from = LETO, values_from = PLACE)




  




  
