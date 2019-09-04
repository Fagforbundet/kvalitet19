library(readxl)
library(tidyverse)
library(ggpubr)
library(openxlsx)

#Importerer bakgrunnstallene fra NHOanalysen og tall på privatisering fra SSB 12367
dea <- read_excel("/Users/thomastallaksen/Documents/R/Prosjekter/Kvalitet19/NHOs tall.xlsx")%>%
  mutate(Kommunenr = str_pad(Kommunenr, width=4, side="left", pad="0"))
ssb <- read_excel("/Users/thomastallaksen/Documents/R/Prosjekter/Kvalitet19/SSB-tall.xlsx")
ssb$Beløp <- as.numeric(ssb$Beløp)


samlet <- ssb%>%
  filter(Artikkelnummer %in% c("AG2", "A370", "AGD4", "A590"))%>%
  filter(Funksjon != "Institusjonslokaler" | Artikkelnummer != "A370")%>%
  group_by(Kommunenummer, Kommune, Funksjon, Artikkelnummer, Artikkel)%>%
  summarise(Kostnad = sum(Beløp, na.rm = TRUE))%>%  
  ungroup()%>%   
  mutate(Deler = ifelse(Artikkelnummer %in% c("A370", "AG2"), "Kjøp", "Kostnader"))%>%
  mutate(Kostnad = ifelse(Artikkelnummer == "A590", -Kostnad, Kostnad))%>%
  group_by(Kommunenummer, Kommune, Deler)%>%
  summarise(Kostnad = sum(Kostnad))%>%
  mutate(Andel = round(Kostnad/sum(Kostnad)*100, digits = 1))%>%
  filter(Deler == "Kjøp")

nho_inndeling <- dea%>%
  select(Kommunenr, `Kommune fullt navn`, `Score, kvalitet`)%>%
  left_join(samlet, by = c("Kommunenr" = "Kommunenummer"))%>%
  mutate(Snitt = ifelse(Andel > 6.5, "Over", "Under"))

write.xlsx(nho_inndeling, "bakgrunnstall.xlsx")
  
t_test <- nho_inndeling%>%  
  na.omit()%>%
  group_by(Snitt)%>%
  summarise("Antall kommuner" = n(), Snittkvalitet = round(mean(`Score, kvalitet`), digits = 2), Standardavvik = sd(`Score, kvalitet`))

graf <- dea%>%
  select(Kommunenr, `Kommune fullt navn`, `Score, kvalitet`)%>%
  left_join(samlet, by = c("Kommunenr" = "Kommunenummer"))

ggplot(graf, aes(y = `Score, kvalitet`, x = Andel))+
  geom_point()+
  xlim(0,40)+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 25, label.y = 30)

  