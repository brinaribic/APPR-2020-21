# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/brinaribic/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/brinaribic/APPR-2020-21/master?urlpath=rstudio) RStudio

## Tematika

Analizirala bom povprečne plače v Sloveniji in Evropi. Za Slovenijo bom pogledala, kako se povprečne plače razlikujejo glede na statistične regije, spol in starost v obdobju 2008 - 2019. Primerjala bom še, kako se plače in BDP spreminjajo skozi leta po statističnih regijah. Nato bom analizirala, kako se povprečne plače razlikujejo po posameznih državah v Evropi. Na koncu si bom ogledala BDP in BDP na prebivalca posamezne države  v obdobju 2010 - 2018, ter kako je povezan s plačo.

Tabele:

1. Plače v SLoveniji glede na statistično regijo, spol in starost
- stolpci: leto, statistična regija, spol, starost, plače

2. BDP v Sloveniji po statističnih regijah
- stolpci: leto, statistična regija, BDP (v mio EUR), BDP na prebivalca (v EUR)

3. BDP na prebivalca v Sloveniji po statističnih regijah
- stolpci: leto, statistična regija, BDP na prebivalca (v EUR)

4. Plače v Evropi po posameznih državah
- stolpci: leto, država, plače

5. BDP in BDP na prebivalca po državah v Evropi
- stolpci: leto, država, BDP (v mio EUR)

6.  BDP na prebivalca po državah v Evropi
- stolpci: leto, država, BDP na prebivalca (v EUR)

Viri:

* https://pxweb.stat.si/SiStat/sl/ (CSV)
* https://www.wikipedia.org/ (HTML)
* https://ec.europa.eu/eurostat (CSV)

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `tmap` - za izrisovanje zemljevidov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-202021)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
