library(readxl)
library(tidyverse)
library(ggpubr)

#Importerer bakgrunnstallene fra NHOanalysen og tall på privatisering fra SSB 12367
dea <- read_excel("/Users/thomastallaksen/Documents/R/Prosjekter/Kvalitet19/190702_dea-analyse-resultat-og-input.xlsx")%>%
  mutate(Kommunenr = str_pad(Kommunenr, width=4, side="left", pad="0"))
ssb <- read_excel("/Users/thomastallaksen/Documents/R/Prosjekter/Kvalitet19/KOSbelop0000.xlsx")
ssb$`2017` <- as.numeric(ssb$`2017`)

per_funksjon <- ssb%>%
  filter(Art_nr %in% c("A370", "AGD4"))%>%
  group_by(Kommunenummer, Kommunenavn, Funksjon, Art)%>%
  summarise(Kostnad = sum(`2017`, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(Kommunenummer, Kommunenavn, Funksjon)%>%
  mutate(Andel = round(Kostnad/sum(Kostnad)*100, digits = 1))

samlet <- ssb%>%
  filter(Art_nr %in% c("A370", "AGD4"))%>%
  group_by(Kommunenummer, Kommunenavn, Art)%>%
  summarise(Kostnad = sum(`2017`, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(Kommunenummer, Kommunenavn)%>%
  mutate(Andel = round(Kostnad/sum(Kostnad)*100, digits = 1))%>%
  filter(Art == "Kjøp fra andre (private)")%>%
  select(Kommunenummer, Kommunenavn, Andel)

sammenkoblet <- dea%>%
  select(Kommunenr, `Kommune fullt navn`, `Score, kvalitet`)%>%
  left_join(samlet, by = c("Kommunenr" = "Kommunenummer"))

ggplot(sammenkoblet, aes(x = `Score, kvalitet`, y = Andel))+
  geom_point()+
  ylim(0,40)+
  geom_smooth(method='lm')+
  stat_cor(method = "pearson", label.x = 25, label.y = 30)

  