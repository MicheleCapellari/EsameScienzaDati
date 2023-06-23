dir()
setwd("C:/Users/Michele/Desktop/Scuola/Università/Esame Scienza dei dati/datasets")
library(tinytex)
library(tibble)
library(tidyr)
library("dplyr")
library("readr")
library(tidyverse)
library("readxl")
library(ggplot2)
library(modelr)
library(ggrepel)
personale <- read_excel("Personale.xlsx")
partita <- read_excel("partita.xlsx")
avversario <- read_excel("Avversario.xlsx")
partitaAvv <- read_excel("partitaAvv.xlsx")
#View(personale)
#View(partita)
#View(avversario)
#View(partitaAvv)

#calcolo il wirate totale 
WinRateTot = partita %>%
              group_by(Risultato) %>%
                summarise(n = n()) %>%
                  mutate(WinRate = n/sum(n)*100)
WinRateTot

#Campioni più usati
CampUsati = personale %>% 
              group_by(Campione) %>% 
                count() %>%
                  filter(n>5)
ggplot(CampUsati, aes(reorder(x=Campione, -n), y=n, fill = Campione)) +
  geom_col() + 
    ggtitle("Campioni più usati") + 
      labs(y = "", x = "Campioni") 
        


#Win rate per ogni campione
k <- personale[,c("ID", "Campione")] #38-40 creo una tabella con ID Campione e risultato
g <- partita[,c("ID", "Risultato")]
WinRateCamp = merge(k,g, by="ID", all=FALSE) 
WinRateCamp = WinRateCamp %>% # modifico la tabella in modo che abbia ripetuto solo 1/2 volta il campione e che abbia nelle colonne a fianco 
                              # il numero di vittorie e il numero di sonfitte, infine calcolo il win rate (anche per le sconfitte)
                group_by(Campione, Risultato) %>%
                  summarise(n = n()) %>%
                    mutate(WinRate = n/sum(n))
#View(WinRateCamp)
WinRateCamp = filter(WinRateCamp, Risultato=="V" & WinRate<0.75 & WinRate>0.25) #seleziono solo le righe con Risultato == "V"
ggplot(WinRateCamp, aes(reorder(Campione,-WinRate), y = WinRate, fill = Campione)) +  #creo un grafico con il winrate per ogni campione rinominando assi e dando un titolo
  geom_col() + 
    ggtitle("Win Rate Campioni") + 
      labs(y = "Win rate", x = "Campioni") +
        scale_y_continuous(labels = scales::percent) +
          scale_x_discrete(guide = guide_axis(n.dodge=2)) +
            scale_fill_manual(values = c("darkgoldenrod", "grey", "lightgreen", "gold", "red", "lightblue", "chocolate4", "brown", "darkgreen", "green"))


#Calcolo il winrate solo con Zeri
#WinRateZeri = partita %>% 
#                filter(Bottom == "Zeri") %>%
#                 group_by(Risultato) %>%
#                    summarise(n = n()) %>%
#                      mutate(WinRate = n/sum(n)*100)
#View(WinRateZeri)

#Kill participation La media a sivler è 47,6% circa
t <- partita[,c("ID", "Uccisioni")]
colnames(t)[2] = "UccisioniTeam"
j <- personale[,c("ID", "Uccisioni", "Assist")] 
colnames(j)[2] = "UccisioniPers"
colnames(j)[3] = "AssistPers"
KillPart = merge(t,j, by="ID", all=FALSE) 
KillPart = cbind(KillPart, (KillPart$UccisioniPers+KillPart$AssistPers) / KillPart$UccisioniTeam)
colnames(KillPart)[5] = "KillParticipation"
#View(KillPart)
ggplot(KillPart, aes(1:nrow(personale),KillParticipation, color="red")) +
    geom_point() +
      geom_smooth(color="blue", se = FALSE) +
        ggtitle("Grafico Kill partecipation") + 
          labs(y = "Kill partecipation", x = "Partite") +
            scale_y_continuous(labels = scales::percent) 
#la linea va verso il basso perchè ho cambiato stile di gioco lasciando il team fightare mentre io pusho sulle side lane.
#e anche perchè il top gioco campioni da 1 vs 1


#Confronto oro squadre e vedere dove ho vinto o perso
OroSquadMia = partita[,c("ID", "Risultato", "Gold")]
OroSquadAvv = partitaAvv[,c("ID", "Gold")] 
colnames(OroSquadAvv)[2] = "GoldAvv"
Oro = merge(OroSquadMia,OroSquadAvv, by="ID", all=FALSE) 
Oro$TipoDiVittoria = ifelse((Oro$Gold>Oro$GoldAvv & Oro$Risultato == "V"), "Vittoria Oro>Avversario", 
                            ifelse((Oro$Gold<Oro$GoldAvv & Oro$Risultato == "V"), "Vittoria Oro<Avversario", 
                                   ifelse((Oro$Gold>Oro$GoldAvv & Oro$Risultato == "F"), "Sconfitta Oro>Avversario", "Sconfitta Oro<Avversario")))
Confronto = Oro %>% 
              group_by(TipoDiVittoria) %>% 
                summarise(n = n()) %>%
                  mutate(Percentuale = n/sum(n)*100)
Confronto
Oro
ggplot(Oro, aes(x=Risultato, y=Gold, fill=Risultato))+
  geom_boxplot() +
    scale_fill_manual(values = c("red", "seagreen3")) +
      labs(y = "Oro Squadra", x = "Vittorie/Sconfitte") +
        ggtitle("Confronto oro tra le vittorie e le sconfitte")

#WinRate a giornata
giorni = partita %>%
          group_by(Data, Risultato) %>%
            summarise(Vittore_Sconfitte = n()) %>%
              mutate(Giornaliere = Vittore_Sconfitte/sum(Vittore_Sconfitte) * 100)
giorni = filter(giorni, Risultato == "V")
giorni
ggplot(giorni, aes(x=Vittore_Sconfitte, y=Data, fill=Giornaliere)) + 
    geom_tile(color="black", width=0.9) +
      scale_fill_continuous(low="red",
                            high="green",
                            name= "Percentuale delle vittore\nsulle partite totali") +
        labs(x = "Vittorie al giorno") +
        ggtitle("Percentuale vittorie in 1 giorno")
  

#Grafico LP
lp <- personale[,c("ID", "LP.Totali", "LP.Partita")]
Barre <- data.frame(start = c(-137, -37, 63, 163),  # Create data with breaks
                    end = c(-37, 63, 163, 263),
                    ranks = factor(4:1))
ggplot() +                           
  geom_rect(data = Barre,
            aes(ymin = start,
                ymax = end,
                xmin = - Inf,
                xmax = Inf,
                fill = ranks),
            alpha = 0.3) +
              geom_line(data = lp,
                        aes(x = 1:nrow(personale),
                            y = LP.Totali)) +
                        labs(y = "LP", x = "Partite") +
                          ggtitle("Grafico LP") +
                            scale_fill_discrete(labels = c("Silver 4", "Silver 3", "Silver 2", "Silver 1"))
print(lp[56:65,3])
print(lp[94:102,3])
#Cs/m
Cs1 <- partita[,c("ID", "Durata")]
Cs2 <- personale[,c("ID", "Cs")]
CS = merge(Cs1, Cs2, by="ID", all=FALSE)
CS$Cs_m = personale$Cs/partita$Durata
ggplot(CS, aes(x=1, y=Cs_m)) +
  geom_boxplot(fill="lightblue")  




