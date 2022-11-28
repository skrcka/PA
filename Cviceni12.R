#//////////////////////////////////////////////////////////////////
#///////////////// Cvičení 12 - Testování hypotéz /////////////////
#//////  R-skript k Doplňujícím příkladům k online cvičení  ///////
#/////////////////      Mgr. Adéla Vrtková        /////////////////
#//////////////////////////////////////////////////////////////////

# Tento skript obsahuje pouze R-příkazy !!!
# Veškeré korektní zápisy a doprovodné komentáře jsou k dispozici v Poznámkách.

# R-příkazy nejsou samy o sobě uznatelným řešením! 
# Slouží případně pouze jako doplněk !!!

#//////////////////////////////////////////////////////////////////
# Příprava prostředí #####

library(readxl)
library(dplyr)
library(ggplot2)
library(moments)
library(rstatix)
library(FSA)
library(lawstat)
library(car)

data = read_excel("data_hraci.xlsx")

# V každém příkladu si data uložíme do samostatné proměnné (data1, data2, data3, atd.)
# a budeme je upravovat na míru daného zadání. Do datasetu data zasahovat nebudeme, 
# tam zůstanou původní importovaná data.

# Příklady jsou vymyšleny tak, aby pokryly téměř všechny dílčí problémy, se kterými je možno se setkat.
# V praxi se samozřejmě na jedné datové sadě řeší pouze některé z nich v závislosti na cíli výzkumu.

#//////////////////////////////////////////////////////////////////
# Příklad 1 #####

#* a) ########################

# POZOR! V Př. 1a byla stanovena hladina významnosti 0,01 !!!
data1a = data

# data1a$system = factor(data1a$system, 
#                        levels = c("Linux", "OSX", "WIN"),
#                        labels = c("Linux", "OS X", "Windows"))

# Odlehlá pozorování a jejich odstranění
boxplot(data1a$odehrane_hod_2018 ~ data1a$system)

outliers = 
  data1a %>% 
  group_by(system) %>% 
  identify_outliers(odehrane_hod_2018)

outliers

data1a =
  data1a %>% 
  mutate(odehrane_hod_2018_out = ifelse(IDhrace %in% outliers$IDhrace, 
                                        NA, 
                                        odehrane_hod_2018))

boxplot(data1a$odehrane_hod_2018_out ~ data1a$system)

podklady = 
  data1a %>% 
  group_by(system) %>% 
  summarise(rozsah = length(na.omit(odehrane_hod_2018_out)),
            sikmost = moments::skewness(odehrane_hod_2018_out, na.rm = T),
            spicatost = moments::kurtosis(odehrane_hod_2018_out, na.rm = T)-3, 
            rozptyl = var(odehrane_hod_2018_out, na.rm = T),
            sm_odch = sd(odehrane_hod_2018_out, na.rm = T),
            prumer = mean(odehrane_hod_2018_out, na.rm = T),
            median = quantile(odehrane_hod_2018_out, 0.5, na.rm = T),
            Shapiruv_Wilkuv_phodnota = shapiro.test(odehrane_hod_2018_out)$p.value)

# Analýza předpokladů - normalita
ggplot(data1a, 
       aes(x = odehrane_hod_2018_out))+
  geom_histogram(bins = 10)+
  facet_wrap("system", 
             ncol = 1)

ggplot(data1a, 
       aes(sample = odehrane_hod_2018_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_wrap("system",
             scales = "free")

# Šikmost, špičatost, Shapirův-Wilkův test - viz podklady

# Analýza předpokladů -> normalita OK -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (největší ku nejmenšímu) < 2 ? - viz podklady

# Exaktně! Test o shodě rozptylů - Bartlettův test (zobecnění dvouvýběrového F-testu)
bartlett.test(data1a$odehrane_hod_2018_out~data1a$system)

# Analýza předpokladů -> normalita OK, shoda rozptylů KO -> kontrola symetrie dat (když je normalita OK, pak je symetrie OK)
# Analýza předpokladů -> normalita OK, shoda rozptylů KO, symetrie OK -> Kruskalův-Wallisův test (test o shodě mediánů)
kruskal.test(data1a$odehrane_hod_2018_out~data1a$system)

# Zamítáme nulovou hypotézu o shodě mediánů - které se ale liší? -> Post-hoc analýza Dunnové metodou
dunnTest(odehrane_hod_2018_out ~ system, 
         data = data1a, 
         method = "bonferroni")

# výběrové mediány (vodítko k seřazení) - viz podklady

# případně efekty - tj. rozdíl výběrového mediánu pro danou skupinu oproti celkovému mediánu
med_sk = podklady$median
med_total = quantile(data1a$odehrane_hod_2018_out, 0.5, na.rm = T)
med_sk - med_total


#* b) ############################

# POZOR! V Př. 1b byla stanovena hladina významnosti 0,05 !!!

data1b = data

# Odlehlá pozorování
boxplot(data1b$odehrane_hod_2019 ~ data1b$system)

outliers = 
  data1b %>% 
  group_by(system) %>% 
  identify_outliers(odehrane_hod_2019)

outliers

data1b =
  data1b %>% 
  mutate(odehrane_hod_2019_out = ifelse(IDhrace %in% outliers$IDhrace, 
                                        NA, 
                                        odehrane_hod_2019))

boxplot(data1b$odehrane_hod_2019_out~ data1b$system)

podklady = 
  data1b %>% 
  group_by(system) %>% 
  summarise(rozsah = length(na.omit(odehrane_hod_2019_out)),
            sikmost = moments::skewness(odehrane_hod_2019_out, na.rm = T),
            spicatost = moments::kurtosis(odehrane_hod_2019_out, na.rm = T)-3, 
            rozptyl = var(odehrane_hod_2019_out, na.rm = T),
            sm_odch = sd(odehrane_hod_2019_out, na.rm = T),
            prumer = mean(odehrane_hod_2019_out, na.rm = T),
            median = quantile(odehrane_hod_2019_out, 0.5, na.rm = T),
            Shapiruv_Wilkuv_phodnota = shapiro.test(odehrane_hod_2019_out)$p.value)

# Analýza předpokladů - normalita
ggplot(data1b, 
       aes(x = odehrane_hod_2019_out))+
  geom_histogram(bins = 10)+
  facet_wrap("system",
             ncol = 1)

ggplot(data1b,
       aes(sample = odehrane_hod_2019_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_wrap("system",
             scales = "free")

# Šikmost, špičatost, Shapirův-Wilkův test - viz podklady

# Analýza předpokladů -> normalita KO -> kontrola symetrie dat testem
tapply(data1b$odehrane_hod_2019_out, 
       data1b$system, 
       symmetry.test, 
       boot = FALSE)

# Analýza předpokladů -> normalita KO, symetrie OK -> Kruskalův-Wallisův test (test o shodě mediánů)

# Pokud bychom testem symetrie symetrii rozdělení v některé skupině zamítli, pak alespoň se podíváme na histogramy, zda
# mají výběry přibližně stejný tvar rozdělení a pokud ano, pak k testu klasicky přistoupíme.
# Jediný problematický případ je ten, kdy rozdělení nejsou symetrická a ještě například v každé podskupině
# je velmi výrazné různé zešikmení (jednou pozitivní jednou negativní) - toto se ale stává zřídka.
# Kruskalův-Wallisův test je stejně to nejrobustnější, co máme a v praxi se tak používá velmi benevolentně
# i na "divoká" data.

# Kdybychom přeci jen potřebovali otestovat shodu rozptylů, pak v případě dat, která nepochází z normálního
# rozdělení volíme Leveneho test (a samozřejmě empirické pravidlo o poměru výběrových rozptylů).
# Empiricky - poměr výběrových rozptylů (větší ku menšímu) < 2 ? - viz podklady
# Exaktně - Leveneho test o shodě rozptylů pro data, která nepochází z normálního rozdělení
leveneTest(data1b$odehrane_hod_2019_out ~ data1b$system)

# Analýza předpokladů -> normalita KO, symetrie OK -> Kruskalův-Wallisův test (test o shodě mediánů)
kruskal.test(data1b$odehrane_hod_2019_out ~ data1b$system)

# Nulovou hypotézu o shodě mediánů NEZAMÍTÁME - tzn. mediány se statisticky významně neliší.
# Všechny tři skupiny lze považovat za homogenní.

# V případě nezamítnutí H0 post-hoc analýzu neděláme!!! Jednoduše proto, že pokud rozdíly mezi skupinami nejsou,
# pak nemáme co post-hoc analýzou hledat.


#* c) ##########################

# POZOR! V Př. 1c byla stanovena hladina významnosti 0,01 !!!

data1c = 
  data %>% 
  mutate(narust = odehrane_hod_2019 - odehrane_hod_2018)

# Odlehlá pozorování
boxplot(data1c$narust ~ data1c$system)

outliers = 
  data1c %>% 
  group_by(system) %>% 
  identify_outliers(narust)

outliers

data1c =
  data1c %>% 
  mutate(narust_out = ifelse(IDhrace %in% outliers$IDhrace, 
                             NA,
                             narust))

boxplot(data1c$narust_out ~ data1c$system)

podklady = 
  data1c %>% 
  group_by(system) %>% 
  summarise(rozsah = length(na.omit(narust_out)),
            sikmost = moments::skewness(narust_out, na.rm = T),
            spicatost = moments::kurtosis(narust_out, na.rm = T)-3, 
            rozptyl = var(narust_out, na.rm = T),
            sm_odch = sd(narust_out, na.rm = T),
            prumer = mean(narust_out, na.rm = T),
            median = quantile(narust_out, 0.5, na.rm = T),
            Shapiruv_Wilkuv_phodnota = shapiro.test(narust_out)$p.value)

# Analýza předpokladů - normalita
ggplot(data1c, 
       aes(x = narust_out))+
  geom_histogram(bins = 10)+
  facet_wrap("system",
             ncol = 1)

ggplot(data1c, 
       aes(sample = narust_out))+
  stat_qq()+ 
  stat_qq_line()+
  facet_wrap("system",
             scales = "free")

# Šikmost, špičatost, Shapirův-Wilkův test - viz podklady

# Analýza předpokladů -> normalita OK (hladina významnosti 0,01) -> shoda rozptylů?
# Empiricky - poměr výběrových rozptylů (největší ku nejmenšímu) < 2 ? - viz podklady
# Exaktně! Test o shodě rozptylů - Bartlettův test
bartlett.test(data1c$narust_out ~ data1c$system)

# Analýza předpokladů -> normalita OK, shoda rozptylů OK -> ANOVA (test o shodě středních hodnot)
vysledky = aov(data1c$narust_out ~ data1c$system)
summary(vysledky)

# Zamítáme nulovou hypotézu o shodě středních hodnot - které se ale liší? -> Post-hoc analýza
TukeyHSD(vysledky)

# výběrové průměry (vodítko k seřazení) - viz podklady

# případně efekty - tj. rozdíl výběrového průměru pro danou skupinu oproti celkovému průměru
prum_sk = podklady$prumer
prum_total = mean(data1c$narust_out, na.rm = T)
prum_sk - prum_total

#//////////////////////////////////////////////////////////////////
# Příklad 2 #####

# Příklad 1a) vedl na Kruskalův-Wallisův test (test o shodě mediánů)
# Doplňkem budou bodové a oboustranné intervalové odhady mediánů (Wilcoxonova statistika, symetrie OK)
# hladina významnosti je 0,01, tj. spolehlivost je 0,99

# Bodové odhady - výběrové mediány - viz podklady příkladu 1a) + rozsahy a sm. odchylky kvůli zaokrouhlení
# Intervalové odhady:
tapply(data1a$odehrane_hod_2018_out, 
       data1a$system, 
       wilcox.test, conf.level = 0.99, conf.int = T)

# Příklad 1b) vedl na Kruskalův-Wallisův test (test o shodě mediánů)
# Doplňkem budou bodové a oboustranné intervalové odhady mediánů (Wilcoxonova statistika, symetrie OK)
# hladina významnosti je 0,05, tj. spolehlivost je 0,95

# Bodové odhady - výběrové mediány - viz podklady příkladu 1b) + rozsahy a sm. odchylky kvůli zaokrouhlení
# Intervalové odhady:
tapply(data1b$odehrane_hod_2019_out, 
       data1b$system, 
       wilcox.test, conf.level = 0.95, conf.int = T)

# Příklad 1c) vedl na ANOVu (test o shodě stř. hodnot)
# Doplňkem budou bodové a oboustranné intervalové odhady stř. hodnot (jednovýb. t-test, normalita OK)
# hladina významnosti je 0,01, tj. spolehlivost je 0,99

# Bodové odhady - výběrové průměry - viz podklady příkladu 1c) + rozsahy a sm. odchylky kvůli zaokrouhlení
# Intervalové odhady:
tapply(data1c$narust_out, 
       data1c$system, 
       t.test, conf.level = 0.99)
