## Exploracni analyza dat

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(moments)
library(rstatix)

# Import dat
data = read_excel("data_hraci.xlsx")

# Překódování kategoriální proměnné, upravit dle vlastní kategoriální proměnné
data$system = factor(data$system, 
                     levels = c("Linux", "OSX", "WIN"),
                     labels = c("Linux", "OS X", "Windows"))

# Slouceni dle atributu
os_table = table(data$system)
# Procentualni rozlozeni atributu
prop.table(os_table)*100

boxplot(data$odehrane_hod_2018)

# Piping identifikovani outliers
data %>% identify_outliers(odehrane_hod_2018)

boxplot(data$odehrane_hod_2018~data$system)

# Piping group by
data %>% group_by(system) %>% identify_outliers(odehrane_hod_2018)

# Odstraneni outliers
outliers = data %>% group_by(system) %>% identify_outliers(odehrane_hod_2018)
data = data %>%
    mutate(odehrane_hod_2018_out = 
        ifelse(IDhrace %in% outliers$IDhrace, NA, odehrane_hod_2018))
data

min(data$odehrane_hod_2018)
max(data$odehrane_hod_2018)

summary(data$odehrane_hod_2018)
sd(data$odehrane_hod_2018)

quantile(data$odehrane_hod_2018, 0.2)

hist(data$odehrane_hod_2018, breaks=10)

# Sikmost <-2; 2> (zaokrouhlit na desetiny)
skewness(data$odehrane_hod_2018)

# Spicatost grafu <-2; 2> (zaokrouhlit na desetiny)
kurtosis(data$odehrane_hod_2018) - 3 # neni normovano proto odecist 3

# Check normality
qqnorm(data$odehrane_hod_2018)
qqline(data$odehrane_hod_2018)

#/////////////////////////////////////////////////////////////////////////////////////
# 1. Krabicový graf (jedna kvant. proměnná) ####

ggplot(data, # uprav
       aes(x = "",
           y = odehrane_hod_2018))+  # uprav
  stat_boxplot(geom = "errorbar",
               width = 0.15)+
  geom_boxplot()+
  labs(x = "", y = "Odehraná doba za rok 2018 (hod)")+ # uprav
  theme_classic()+
  theme(axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black", size = 11))

#/////////////////////////////////////////////////////////////////////////////////////
# 2. Vícenásobný krabicový graf (kvant. proměnná tříděná dle kategoriální) ####

ggplot(data, # uprav
       aes(x = system, # uprav
           y = odehrane_hod_2018))+ # uprav
  stat_boxplot(geom = "errorbar",
               width = 0.15)+
  geom_boxplot()+
  labs(x = "", y = "Odehraná doba za rok 2018 (hod)")+ # uprav
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))

#/////////////////////////////////////////////////////////////////////////////////////
# 3. Histogram (jedna kvant. proměnná) ####

ggplot(data, # uprav
       aes(x = odehrane_hod_2018))+ # uprav
  geom_histogram(binwidth = 20, # uprav
                 color = "black",
                 fill = "grey55")+
  labs(x = "Odehraná doba za rok 2018 (hod)", y = "Četnost")+ # uprav
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))

# Alternativa:
ggplot(data, # uprav
       aes(x = odehrane_hod_2018))+ # uprav
  geom_histogram(aes(y = ..density..),
                 binwidth = 20, # uprav
                 color = "black",
                 fill = "grey55")+
  geom_density()+
  labs(x = "Odehraná doba za rok 2018 (hod)", # uprav
       y = "Hustota pravděpodobnosti")+ 
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))

#/////////////////////////////////////////////////////////////////////////////////////
# 4. Sada histogramů (kvant. proměnná tříděná dle kategoriální) ####

ggplot(data, # uprav
       aes(x = odehrane_hod_2018))+ # uprav
  geom_histogram(binwidth = 20, # uprav
                 color = "black",
                 fill = "grey55")+
  labs(x = "Odehraná doba za rok 2018 (hod)", y = "Četnost")+ # uprav
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))+
  facet_wrap("system",  # uprav
             dir = "v")

# Alternativa:
ggplot(data, # uprav
       aes(x = odehrane_hod_2018))+ # uprav
  geom_histogram(aes(y = ..density..),
                 binwidth = 20, # uprav
                 color = "black",
                 fill = "grey55")+
  geom_density()+
  labs(x = "Odehraná doba za rok 2018 (hod)", # uprav
       y = "Hustota pravděpodobnosti")+ 
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))+
  facet_wrap("system",  # uprav
             dir = "v")

#/////////////////////////////////////////////////////////////////////////////////////
# 5. QQ-graf (jedna kvant. proměnná) ####

ggplot(data, # uprav
       aes(sample = odehrane_hod_2018))+ # uprav
  stat_qq()+
  stat_qq_line()+
  labs(x = "Teoretické normované kvantily", y = "Výběrové kvantily")+
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))

#/////////////////////////////////////////////////////////////////////////////////////
# 6. Sada QQ-grafů (kvant. proměnná tříděná dle kategoriální) ####

ggplot(data, # uprav
       aes(sample = odehrane_hod_2018))+ # uprav
  stat_qq()+
  stat_qq_line()+
  labs(x = "Teoretické normované kvantily", y = "Výběrové kvantily")+
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 11))+
  facet_wrap("system", # uprav
             ncol = 3, # uprav
             scales = "free")

#/////////////////////////////////////////////////////////////////////////////////////
# 7. Tabulka se sumárními charakteristikami (jedna kvant. proměnná) ####

# V každém řádku s "odehrane_hod_2018" je nutné upravit název proměnné
moje_tab = 
  data %>%
    summarise(rozsah = length(na.omit(odehrane_hod_2018)),
              minimum = min(odehrane_hod_2018, na.rm=T),
              Q1 = quantile(odehrane_hod_2018, 0.25, na.rm=T),
              prumer = mean(odehrane_hod_2018, na.rm=T),
              median = median(odehrane_hod_2018, na.rm=T),
              Q3 = quantile(odehrane_hod_2018, 0.75, na.rm=T),
              maximum = max(odehrane_hod_2018, na.rm=T),
              rozptyl = var(odehrane_hod_2018, na.rm=T),
              smerodatna_odchylka = sd(odehrane_hod_2018,na.rm=T),
              variacni_koeficient = (100*(smerodatna_odchylka/abs(prumer))), 
              sikmost = (moments::skewness(odehrane_hod_2018, na.rm=T)),
              stand_spicatost = (moments::kurtosis(odehrane_hod_2018, na.rm=T)-3),
              dolni_mez_hradeb = Q1-1.5*(Q3-Q1),
              horni_mez_hradeb = Q3+1.5*(Q3-Q1))

t(moje_tab)

#/////////////////////////////////////////////////////////////////////////////////////
# 8. Tabulka se sumárními charakteristikami (kvant. proměnná tříděná dle kategoriální) ####

# V každém řádku s "odehrane_hod_2018" je nutné upravit název proměnné
moje_tab2=
  data %>%
  group_by(system) %>%  # uprav
  summarise(rozsah = length(na.omit(odehrane_hod_2018)),
            minimum = min(odehrane_hod_2018, na.rm=T),
            Q1 = quantile(odehrane_hod_2018, 0.25, na.rm=T),
            prumer = mean(odehrane_hod_2018, na.rm=T),
            median = median(odehrane_hod_2018, na.rm=T),
            Q3 = quantile(odehrane_hod_2018, 0.75, na.rm=T),
            maximum = max(odehrane_hod_2018, na.rm=T),
            rozptyl = var(odehrane_hod_2018, na.rm=T),
            smerodatna_odchylka = sd(odehrane_hod_2018,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/abs(prumer))), 
            sikmost = (moments::skewness(odehrane_hod_2018, na.rm=T)),
            stand_spicatost = (moments::kurtosis(odehrane_hod_2018, na.rm=T)-3),
            dolni_mez_hradeb = Q1-1.5*(Q3-Q1),
            horni_mez_hradeb = Q3+1.5*(Q3-Q1))

t(moje_tab2)

#/////////////////////////////////////////////////////////////////////////////////////
# 9. Odstranění odlehlých pozorování (jedna kvant. proměnná) ####

outliers = 
  data %>% # uprav nazev dat
   identify_outliers(odehrane_hod_2019)  # uprav nazev promenne

data = data %>%  # uprav nazev dat
            mutate(odehrane_hod_2019_out = # uprav nazev nove promenne
                     ifelse(IDhrace %in% outliers$IDhrace,  # uprav IDhrace
                            NA, 
                            odehrane_hod_2019)) # uprav nazev promenne


#/////////////////////////////////////////////////////////////////////////////////////
# 10. Odstranění odlehlých pozorování (kvant. proměnná tříděná dle kategoriální) ####

outliers = 
  data %>% # uprav nazev dat
  group_by(system) %>% # uprav nazev promenne
  identify_outliers(odehrane_hod_2018)  # uprav nazev promenne

data = data %>%  # uprav nazev dat
  mutate(odehrane_hod_2018_out = # uprav nazev nove promenne
           ifelse(IDhrace %in% outliers$IDhrace,  # uprav IDhrace
                  NA, 
                  odehrane_hod_2018)) # uprav nazev promenne

