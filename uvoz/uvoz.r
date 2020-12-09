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
    rename("15-24"=3,"25-34"=4, "35-44"=5, "45-54"=6, "55-64"=7, "65 ali več"=8) %>% 
    mutate(SPOL=ime) %>%
    pivot_longer(c(-1,-2,-9), names_to="STAROST", values_to="PLACE")
}

zenske <- spol(c(1, 2, 4, 6, 8, 10, 12, 14), "ženske")
moski <- spol(c(1, 2, 3, 5, 7, 9, 11, 13), "moški")

place_SLO <- full_join(moski, zenske) %>% 
  arrange(LETO, STATISTICNA_REGIJA, STAROST) %>% drop_na()

write_csv2(place_SLO,"podatki/SLO_place_urejeno.csv")

# tabela BDP za Slovenijo

uvoz2.1 <- read_csv2("podatki/SLO_BDP_regije.csv", 
                     skip=2, 
                     n_max=11, 
                     locale=locale(encoding="cp1250"))

uvoz2.1 <- uvoz2.1 %>% 
  pivot_longer(c(-1,-2), names_to = "REGIJE", values_to = "BDP") %>% 
  rename("STATISTICNA_REGIJA"="REGIJE")

bdp_SLO <- uvoz2.1[,-c(1)]

write_csv2(bdp_SLO,"podatki/SLO_BDP_regije_urejeno.csv")

# tabela BDP na prebivalca za Slovenijo

uvoz2.2 <- read_csv2("podatki/SLO_BDP_regije.csv", 
                     skip=2, 
                     locale=locale(encoding="cp1250"))
uvoz2.2 <- uvoz2.2[-c(1:22),]

uvoz2.2 <- uvoz2.2 %>% 
  pivot_longer(c(-1,-2), names_to = "REGIJE", values_to = "BDP")

bdp_pc_SLO <- uvoz2.2[,-c(1)] %>% 
  rename("STATISTICNA_REGIJA"="REGIJE")

write_csv2(bdp_pc_SLO,"podatki/SLO_BDPpc_regije_urejeno.csv")


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
  mutate(PLACE=parse_number(PLACE, na=c("-", "NA")), LETO=parse_number(LETO)) %>% arrange(LETO) 
place_Evropa <- place_Evropa[c(2,1,3)] %>% drop_na()

write_csv2(place_Evropa,"podatki/place_Evropa_urejeno.csv")

# tabela BDP

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

bdp_Evropa <- data.frame(LETO, DRZAVA, BDP)

write_csv2(bdp_Evropa,"podatki/bdp_Evropa_urejeno.csv")

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

bdp_pc_Evropa <- data.frame(LETO, DRZAVA, BDP) %>% 
  rename("BDP_na_prebivalca"="BDP")

write_csv2(bdp_pc_Evropa,"podatki/bdp_pc_Evropa_urejeno.csv")






  
