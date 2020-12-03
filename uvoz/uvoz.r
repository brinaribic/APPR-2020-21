library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# tabela s plačami po regijah glede na spol in starost

uvoz1 <- read_csv2("podatki/SLO_place.csv", skip=2, na=c("z", "Z", "-"),
                   locale=locale(encoding="Windows-1250")) %>%
  mutate(LETO=parse_number(LETO)) %>% 
  rename("STATISTICNA_REGIJA"="STATISTIČNA REGIJA")


spol <- function(stolpci, ime) {
  uvoz1 %>% select(stolpci) %>% 
    rename("Skupaj"=3, "15-64"=4, "15-24"=5,"25-34"=6, "35-44"=7, "45-54"=8, "55-64"=9) %>% 
    mutate(SPOL=ime) %>%
    pivot_longer(c(-1,-2,-10), names_to="STAROST", values_to="PLACE")
}

zenske <- spol(c(1, 2, 5, 8, 11, 14, 17, 20, 23), "ženske")
moski <- spol(c(1, 2, 4, 7, 10, 13, 16, 19, 22),"moški")
skupaj <- spol(c(1, 2, 3, 6, 9, 12, 15, 18, 21),"Skupaj")

place_SLO <- full_join(moski, zenske)
place_SLO <- full_join(place_SLO, skupaj)

place_SLO <- full_join(moski, zenske)
place_SLO <- full_join(place_SLO, skupaj) 


# tabela BDP za Slovenijo

uvoz2 <- read_csv2("podatki/SLO_BDP_regije.csv", skip=2, locale=locale(encoding="cp1250"))
uvoz2 <- uvoz2[-c(12:22),]

uvoz2 <- uvoz2 %>% pivot_longer(c(-1,-2), names_to = "REGIJE", values_to = "BDP")

BDP <- parse_character(uvoz2$MERITVE)
BDP <- gsub("Mio EUR \\(fiksni tečaj 2007\\)", "BDP \\(mio EUR\\)", BDP)
BDP <- gsub("Na prebivalca, EUR \\(tekoči tečaj\\)", "BDP na prebivalca \\(EUR\\)", BDP)
BDP <- parse_factor(BDP)

REGIJE <- parse_character(uvoz2$REGIJE)
REGIJE <- str_replace_all(REGIJE, "\\.", "\\-")
REGIJE <- str_replace_all(REGIJE, "Jugovzhodna\\-Slovenija", "Jugovzhodna\\ Slovenija")
REGIJE <- parse_factor(REGIJE)

LETO <- uvoz2$LETO

MERITEV <- uvoz2$BDP

bdp_regije <- data.frame(BDP, LETO, REGIJE, MERITEV)
bdp_regije <- bdp_regije %>% 
  rename("STATISTICNA_REGIJA"="REGIJE") %>% 
  pivot_wider(-c(1), names_from=BDP, values_from=MERITEV)



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
  mutate(PLACE=parse_number(PLACE, na=c("-", "NA"))) %>% arrange(LETO)
place_Evropa <- place_Evropa[c(2,1,3)]

# tabela BDP in BDP per capita za Evropo

## tabela BDP

uvoz3 <- read_csv("podatki/BDP_Evropa.csv", skip=1,   
                  locale=locale(encoding = "cp1250"), 
                  col_names = c("LETO","DRZAVA", "ENOTA", "MERITEV", "BDP"),
                  na = c(":"))

bdp_Evropa <- uvoz3[-c(3,4)]
DRZAVA <- bdp_Evropa$DRZAVA
DRZAVA <- str_replace_all(DRZAVA, " \\s*\\([^\\)]+\\)","") 
DRZAVA <- parse_factor(DRZAVA)
LETO <- bdp_Evropa$LETO
BDP <- bdp_Evropa$BDP

bdp_Evropa <- data.frame(LETO, DRZAVA, BDP) %>% 
  rename("BDP (mio EUR)"="BDP")

## tabela BDP per capita

uvoz4 <- read_csv("podatki/BDP_per_capita_Evropa.csv", skip=1,   
                  locale=locale(encoding = "cp1250"), 
                  col_names = c("LETO","DRZAVA", "ENOTA", "MERITEV", "BDP"),
                  na = c(":"))

bdp_pc <- uvoz4[-c(3,4)]
DRZAVA <- bdp_pc$DRZAVA
DRZAVA <- str_replace_all(DRZAVA, " \\s*\\([^\\)]+\\)","") 
DRZAVA <- parse_factor(DRZAVA)
LETO <- bdp_pc$LETO
BDP <- round(bdp_pc$BDP)

bdp_pc <- data.frame(LETO, DRZAVA, BDP) %>% 
  rename("BDP na prebivalca (EUR)"="BDP")

# zdruzitev tabele bdp_Evropa in bdp_pc

bdp_skupaj <- inner_join(bdp_pc, bdp_Evropa, by=c("LETO","DRZAVA"))




  
