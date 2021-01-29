library(readr)
library(dplyr)
library(tidyr)
library(stringr)

uvoz.csv2 <- function(ime, stolpci, od, sep) {
  tabela <- read_csv2(ime,
            skip=od, na=c("z", "Z", "-"), 
            locale=locale(encoding="Windows-1250"), col_names = stolpci)
  return(tabela)
}

# tabela s plačami po regijah glede na spol in starost

stolpci1 <- c("Leto","Regija", "Starost", "Spol", "Placa")
place_SLO <- uvoz.csv2("podatki/SLO_place1.csv", stolpci1, 3) %>% drop_na()

povprecje <- place_SLO %>% 
  .[-c(3)] %>% 
  filter(Leto == 2018) %>% 
  group_by(Regija) %>%
  summarise(Povprecje=round(mean(Placa)))

place <- place_SLO %>% 
  group_by(Leto) %>%
  summarise(Placa=mean(Placa))

# tabela BDP po regijah

stolpci2 <- c("Meritev", "Leto", "Regija", "BDP")
bdp_SLO <- uvoz.csv2("podatki/SLO_bdp1.csv", stolpci2, 3) %>% .[,-c(1)]

# tabela BDP na prebivalca po regijah

stolpci3 <- c("Regija", "Leto", "BDP_na_prebivalca")
bdp_pc_SLO <- uvoz.csv2("podatki/SLO_bdp_pc1.csv", stolpci3, 3) %>% .[c(2,1,3)] %>% arrange(Leto)

# tabela plač za evropske države

library(rvest)

uvoz.plac <- function(){
  link <- "https://en.wikipedia.org/wiki/List_of_European_countries_by_average_wage"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% 
    html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[2]] %>% 
    html_table(fill=TRUE)
  
  place_Evropa <- tabela[c(1, 12:22)]
  place_Evropa <- place_Evropa[-c(1,2),]
  
  colnames(place_Evropa) <- c("Drzava", 2008:2018)
  
  place_Evropa <- place_Evropa %>% 
    mutate(Drzava=gsub("\\[[^]]*\\]","", Drzava)) %>%
    pivot_longer(-c(1), names_to = "Leto", values_to = "Placa") %>%
    mutate(Placa=parse_number(Placa, na=c("-", "NA")), Leto=parse_number(Leto)) %>% arrange(Leto) 
  place_Evropa <- place_Evropa[c(2,1,3)] %>% drop_na()
  return(place_Evropa)
}

place_Evropa <- uvoz.plac()
Drzava <- place_Evropa$Drzava %>% trimws()
Placa <- place_Evropa$Placa
Leto <- place_Evropa$Leto
place_Evropa <- data.frame(Leto,Drzava,Placa)


uvoz.csv <- function(ime, stolpci, od) {
  tabela <- read_csv(ime, skip=od,   
           locale=locale(encoding = "cp1250"), 
           col_names = stolpci,
           na = c(":"))
  return(tabela)
}

# tabela BDP za evropske države

stolpci4 <- c("Leto","Drzava", "Enota","Meritev", "BDP", "x")
bdp_Evropa <- uvoz.csv("podatki/BDP_Evropa.csv", stolpci4, 1) %>% .[,-c(3,4,6)] 
Drzava <- str_replace_all(bdp_Evropa$Drzava, " \\s*\\([^\\)]+\\)","") 
bdp_Evropa <- bdp_Evropa %>% .[,-c(2)] %>% mutate(Drzava = Drzava) %>% .[c(1,3,2)] %>% drop_na()

# tabela BDP na prebivalca za evropske države

stolpci5 <- c("Leto","Drzava", "Enota","Meritev", "BDP_na_prebivalca", "x")
bdp_pc_Evropa <- uvoz.csv("podatki/BDP_per_capita_Evropa.csv", stolpci5, 1) %>% .[,-c(3,4,6)] 
Drzava <- str_replace_all(bdp_pc_Evropa$Drzava, " \\s*\\([^\\)]+\\)","") 
bdp_pc_Evropa <- bdp_pc_Evropa %>% .[,-c(2)] %>% mutate(Drzava = Drzava) %>% .[c(1,3,2)] %>% drop_na()

rast.plac <- place_Evropa %>%
  group_by(Leto) %>% 
  summarise(povprecje=round(mean(Placa)))%>%
  mutate(rast=(povprecje-lag(povprecje))/povprecje*100) %>% select(Leto, rast) %>% drop_na() 

rast.BDP <- bdp_Evropa %>%
  group_by(Leto) %>% 
  summarise(povprecje=round(mean(BDP))) %>%
  mutate(rast=(povprecje-lag(povprecje))/povprecje*100) %>% select(Leto, rast) %>% drop_na()

place.bdp <- left_join(place_Evropa,bdp_Evropa) %>% drop_na()


#shranjevanje tabel
write.csv2(place_SLO,"podatki/SLO_place_posodobljeno.csv")  
write.csv2(bdp_SLO, file="podatki/SLO_bdp_posodobljeno.csv")
write.csv2(bdp_pc_SLO, file="podatki/SLO_bdp_pc_posodobljeno.csv")
write.csv(place_Evropa, file="podatki/Evropa_place_posodobljeno.csv")
write.csv(bdp_Evropa, file="podatki/Evropa_bdp_posodobljeno.csv")
write.csv(bdp_pc_Evropa, file="podatki/SLO_bdp_pc_posodobljeno.csv")

