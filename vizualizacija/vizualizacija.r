source("lib/uvozi.zemljevid.r")

zemljevid.regij <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", 
                                   "gadm36_SVN_1", 
                                   encoding = "UTF-8")


koordinate <- coordinates(zemljevid.regij) %>% as_tibble()
zemljevid.regij <- zemljevid.regij %>% fortify()

zemljevid.regij <- zemljevid.regij %>%
  mutate(Regija=NAME_1 %>%
           str_replace("Notranjsko-kraška","Primorsko-notranjska") %>%
           str_replace("Obalno-kraška","Obalno-kraška") %>%
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
  
# primerjava BDP na prebivalca in plač v regijah z najvišjimi in najnižjimi povprečnimi plačami

v <- c("Zasavska", "Osrednjeslovenska", "Posavska", "Koroška")

graf.bdp <- ggplot(data = bdp_pc_SLO %>%
                     filter(Regija %in% v), 
                   aes(x=Leto, y=BDP_na_prebivalca, color=Regija)) + 
  geom_line(size=1) +
  ylab("BDP na prebivalca (v EUR)") +
  scale_x_continuous(breaks=2008:2018) +
  labs(title="BDP na prebivalca v štirih slovenskih regijah") +
  theme_bw()

graf.plac <- ggplot(data = place_SLO %>% 
                      filter(Regija %in% v) %>%
                      group_by(Regija, Leto) %>%
                      summarise(Placa=round(mean(Placa))),
                    aes(x=Leto, y=Placa, col=Regija)) + 
  geom_line(size=1) + 
  ylab("Plača (v EUR)") +
  scale_x_continuous(breaks=2008:2018) +
  labs(title="Plače v štirih slovenskih regijah") + 
  theme_bw()

# place glede na spol v obdobju 2008-2018 za za regije z najvišjimi in najnižjimi plačami

graf.spol <- ggplot(
  place_SLO %>% 
    filter(Regija %in% v) %>% 
    group_by(Leto, Spol, Regija) %>% 
    summarise(Placa=round(mean(Placa))),
  aes(x=Leto, y=Placa, color=Spol)
) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~Regija) +
  ylab("Plača") + 
  scale_x_continuous(breaks=2008:2018) +
  labs(title="Razlike v plačah glede na spol v Sloveniji") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45))


# plače v Evropi za leto 2008, 2012 in 2018

graf.placEvropa <- ggplot(place_Evropa %>% 
         filter(Leto %in% c("2008","2018")) %>%
         filter(!(Drzava %in% c("Albania","Kosovo", "Belgium", "Denmark"))), 
       aes(x=Placa, y=reorder(Drzava, -Placa), fill=factor(Leto))) + 
  geom_col(position = "dodge") + 
  xlab("Plače (EUR)")+ 
  ylab("Država") +
  geom_vline(mapping=aes(xintercept = mean(place_Evropa %>% filter(Leto==2018) %>%.$Placa,
                                           fill="Povprecje 2018")), col="#00BFC4") +
  geom_vline(mapping=aes(xintercept = mean(place_Evropa %>% filter(Leto==2008) %>%.$Placa,
                                           fill="Povprecje 2008")), col="#F8766D") +
  labs(title = "Plače v evropskih državah za leto 2008 in 2018", fill="Leto") +
  theme_bw()

# BDP na prebivalca v Evropi
graf.bdpEvropa <- ggplot(bdp_pc_Evropa %>% 
                            filter(Leto %in% c("2008","2018")) %>%
                            filter(!(Drzava %in% c("Liechtenstein","North Macedonia", "Kosovo"))), 
                          aes(x=BDP_na_prebivalca, y=reorder(Drzava, -BDP_na_prebivalca), fill=factor(Leto))) + 
  geom_col(position = "dodge") +
  xlab("BDP na prebivalca (v EUR)")+ 
  ylab("Država")  + 
  geom_vline(mapping=aes(xintercept = mean(bdp_pc_Evropa %>% filter(Leto==2018) %>%.$BDP_na_prebivalca,
                                           fill="Povprecje 2018")), col="#00BFC4") +
  geom_vline(mapping=aes(xintercept = mean(bdp_pc_Evropa %>% filter(Leto==2008) %>%.$BDP_na_prebivalca,
                                           fill="Povprecje 2008")), col="#F8766D") +
  labs(title = "BDP na prebivalca v evropskih državah za leto 2008 in 2018", fill="Leto") + 
  theme_bw()


# plače v posameznih državah v primerjavi s Slovenijo
drzave <- c("Austria", "Slovenia", "Croatia", "Germany","Cyprus")

graf.drzave <- ggplot(place_Evropa %>% filter(Drzava %in% drzave),
                      aes(x=Leto, y=Placa)) + 
  geom_line(aes(color=factor(Drzava))) +
  geom_point(aes(color=factor(Drzava))) + 
  scale_x_continuous(breaks=2008:2018) +
  ylab("Plača (v EUR)") +
  labs(title="Primerjava plač nekaterih evropskih držav", color="Država") +
  theme_bw()



# rast povprecnih plac in bdp za celotno Evropo
barve <- c("Plače" = "#D95F02", "BDP na prebivalca" = "#1B9E77" )

graf.rasti <- ggplot(rast.plac, 
                     aes(x=Leto, y=rast)) + 
  geom_line(aes(color="Plače"), size=1) + 
  geom_line(data=rast.BDP, aes(color="BDP"),size=1) +
  scale_x_continuous(breaks=2009:2018) +
  labs(title="Rast BDP in rast plač v Evropi", y = "Rast (v %)", color="") + 
  theme_bw()


