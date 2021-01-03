require(tmap)
require(RColorBrewer)

source("lib/uvozi.zemljevid.r")
source("lib/libraries.r", encoding = "UTF-8")

zemljevid.regij <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", 
                                   "gadm36_SVN_1") 
koordinate <- coordinates(zemljevid.regij) %>% as_tibble()
zemljevid.regij <- zemljevid.regij %>% fortify()

zemljevid.regij <- zemljevid.regij %>%
  mutate(Regija=NAME_1 %>%
           str_replace("GoriĹˇka", "Goriška") %>%
           str_replace("KoroĹˇka", "Koroška") %>%
           str_replace("Notranjsko-kraĹˇka","Primorsko-notranjska") %>%
           str_replace("Obalno-kraĹˇka","Obalno-kraška") %>%
           str_replace("Spodnjeposavska","Posavska"))

zemljevid.regij <- inner_join(povprecje, zemljevid.regij, by="Regija")

brez.ozadja <-   theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
  )

regije <- c("Gorenjska", "Goriška", "Jugovzhodna Slovenija", "Koroška", "Primorsko-notranjska","Obalno-kraška","Osrednjeslovenska","Podravska","Pomurska","Savinjska","Posavska","Zasavska")

imena.regij <- koordinate %>% mutate(ime=regije)

# zemljevid povprecnih plac v regijah
brks <- quantile(zemljevid.regij$Povprecje, seq(0,1,1/8))
legendaLabele <- paste(as.integer(brks[1:8]),as.integer(brks[2:9]),sep="-")
legendaNaslov <- "Plače v EUR"

zemljevid.plac <- zemljevid.regij %>% 
  mutate(
    kvantil=factor(findInterval(zemljevid.regij$Povprecje, brks, all.inside=TRUE))
  ) %>%
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=kvantil), color="black", size=0.001) + 
  scale_fill_brewer(
    legendaNaslov,
    type=5, palette="Reds", 
    labels=legendaLabele, 
  ) + 
  ggtitle("Povprečne bruto plače za letu 2018 v posameznih regijah") +
  geom_text(data=imena.regij, aes(x=V1, y=V2, label=ime), size=2.5) +
  brez.ozadja
  

# place glede na spol v obdobju 2008-2018 za SLO

graf.spol <- ggplot(
  place_SLO %>% group_by(Leto, Spol) %>% summarise(Placa=round(mean(Placa))),
  aes(x=Leto, y=Placa, color=Spol)
) + 
  geom_point() + 
  geom_line(size=1) +
  ylab("Plača") + 
  scale_x_continuous(breaks=2008:2018) +
  labs(title="Razlike v plačah glede na spol v Sloveniji")

# primerjava BDP na prebivalca in plač v regijah z najvišjimi in najnižjimi povprečnimi plačami

v <- c("Zasavska", "Osrednjeslovenska", "Posavska", "Koroška")

graf.bdp <- ggplot(data = bdp_pc_SLO %>%
                     filter(Regija %in% v), 
                   aes(x=Leto, y=BDP_na_prebivalca, color=Regija)) + 
  geom_line(size=1) +
  ylab("BDP na prebivalca (v EUR)") +
  scale_x_continuous(breaks=2008:2018) +
  ggtitle("BDP na prebivalca v štirih slovenskih regijah")

graf.plac <- ggplot(data = place_SLO %>% 
                       filter(Regija %in% v) %>%
                       group_by(Regija, Leto) %>%
                       summarise(Placa=round(mean(Placa))),
                     aes(x=Leto, y=Placa, col=Regija)) + 
  geom_line(size=1) + 
  ylab("Plača (v EUR)") +
  scale_x_continuous(breaks=2008:2018) +
  ggtitle("Plače v štirih slovenskih regijah")


