# Packete importieren
# Package names
packages <- c("readxl", "naniar", "ggplot2","nnet",
              "glm.predict", "ggalt","tidyverse",
              "remotes", "rJava","tabulizerjars", "tabulizer",
              "ggrepel")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


##Dataset GMMP

# Import data
Daten_GMMP <- read_excel("~/Studium/Master/S1/Datenjournalismus/Daten_GMMP.xlsx")
View(Daten_GMMP)
summary(Daten_GMMP)

# Recode variables
summary(Daten_GMMP$media)
Daten_GMMP$mediaf <-as.factor(Daten_GMMP$media)
Daten_GMMP$mediaf <- recode(Daten_GMMP$mediaf,"TV"= "TV", "Twitter"="Twitter", "Zeitung"="Zeitung", "Internet"="Internet",
                            "Radio"="Radio",.default =NA_character_)

Daten_GMMP$layerf <- as.factor(Daten_GMMP$layer) 
Daten_GMMP$layerf<- recode(Daten_GMMP$layerf, "1"="1", "2"="2","3"="3","4"="4","5"="5","6"="6","7"="7",
       "8"="8","9"="9","10"="10","11"="11", "12"="12","13"="13", .default = NA_character_)

Daten_GMMP$idf <- as.factor(Daten_GMMP$id)

Daten_GMMP$scopef <- as.factor(Daten_GMMP$scope)
Daten_GMMP$scopef <- recode(Daten_GMMP$scopef,  "(1) Local" = "Local", "(2) National" = "National", "(2) No"= "National",
                            "(3) Sub-Regional and Regional"= "Regional", "(4) Foreign/International" ="International",
                            .default = NA_character_ )

Daten_GMMP$equality_rights_reff <- as.factor(Daten_GMMP$equality_rights_ref)
Daten_GMMP$equality_rights_reff <- recode(Daten_GMMP$equality_rights_reff, "(1) Yes"  = "Yes", "(2) No"  = "No",
                                          .default = NA_character_)

Daten_GMMP$women_relatedf <- as.factor(Daten_GMMP$women_related)
Daten_GMMP$women_relatedf <- recode(Daten_GMMP$women_relatedf, "(1) Yes" = "Yes", "(2) No" = "No",
                                    .default = NA_character_)

Daten_GMMP$inequality_issuesf <- as.factor(Daten_GMMP$inequality_issues)
Daten_GMMP$inequality_issuesf <- recode(Daten_GMMP$inequality_issuesf, "(1) Agree" = "Agree", "(1) Yes"="Agree",
                                        "(2) Disagree"= "Disagree", "(2) No"= "Disagree",
                                        .default = NA_character_)

Daten_GMMP$gender_stereotypes_challf <- as.factor(Daten_GMMP$gender_stereotypes_chall)
Daten_GMMP$gender_stereotypes_challf <- recode(Daten_GMMP$gender_stereotypes_challf, "(1) Agree" = "Agree",
                                               "(2) Disagree" = "Disagree", "(2) No" = "Disagree",
                                               .default = NA_character_)

Daten_GMMP$sex_journf <- as.factor(Daten_GMMP$sex_journ)
Daten_GMMP$sex_journf <- recode(Daten_GMMP$sex_journf, "(1) Female"= "Frauen", "(2) Male"= "Männer",
                                "(3) Other: transgender, gender non-conforming, non-binary, two-spirit, third genderâ???¦" = "Other",
                                .default = NA_character_) 

Daten_GMMP$age_journf <- as.factor(Daten_GMMP$age_journ)
Daten_GMMP$age_journf <- recode(Daten_GMMP$age_journf, "(3) 19-34"= "19-34", "(4) 35-49"= "35-49",
                                "(5) 50-64"="50-64", "(6) 65 and above"= "65+",
                                .default = NA_character_)

Daten_GMMP$sex_agentf <- as.factor(Daten_GMMP$sex_agent)
Daten_GMMP$sex_agentf <- recode(Daten_GMMP$sex_agentf, "(1) Female"= "Frauen", "(2) Male"= "Männer",
                                "(3) Other: transgender, gender non-conforming, non-binary, two-spirit, third genderâ???¦" = "Other",
                                .default = NA_character_) 

Daten_GMMP$age_agentf <- as.factor(Daten_GMMP$age_agent)
Daten_GMMP$age_agentf <- recode(Daten_GMMP$age_agentf, "(3) 19-34"= "19-34", "(4) 35-49"= "35-49",
                                "(5) 50-64"="50-64", "(6) 65 and above"= "65+",
                                .default = NA_character_)

Daten_GMMP$story_functionf <- as.factor(Daten_GMMP$story_function)
Daten_GMMP$story_functionf <- recode(Daten_GMMP$story_functionf, "(0) Do not know"="Do not know", "(1) Subject"="Subject",
                                     "(2)Politician / Member Of Parliament"="Politician", "(2) Spokesperson"="Spokesperson",
                                     "(3) Expert or commentator"="Expert/commentator", "(4) Personal experience"="Personal experience",
                                     "(5) Eye witness"="Eye witness","(6) Popular opinion"="Popular opinion", 
                                     "(7) Other. Use only as a last resort (Describe the function in 'comments' section of coding sheet)."="Other",
                                     .default = NA_character_)

Daten_GMMP$photographedf <- as.factor(Daten_GMMP$photographed)
Daten_GMMP$photographedf <- recode(Daten_GMMP$photographedf, "(1) Yes"="Yes", "(2) No"="No",
                                   "(3) Don't know"="Don't know", .default = NA_character_)

Daten_GMMP$titlef <- as.factor(Daten_GMMP$title)
to_filter_out_factors <- c("(0) Not Applicable (Person Is Identified Solely As A Victim)",
                           "(1) Yes", "(2) No", "(4) Victim Of Other Non-Domestic Crime",
                           "Instagrammer ...\"\"","Intelligence Officer", "NA", "Referee â???¦\"\"",
                           "Referee â€¦\"\"")
Daten_GMMP$titlef <- droplevels((replace_with_na(data = Daten_GMMP, replace = list(titlef = to_filter_out_factors)))$titlef)
summary(Daten_GMMP$titlef)



Daten_GMMP$sprachregionf<- as.factor(Daten_GMMP$sprachregion)
Daten_GMMP$sprachregionf <- recode(Daten_GMMP$sprachregionf, "Deutschschweiz"="Deutschschweiz",
                                   "FranzÃ¶sische Schweiz"="Französische Schweiz", "Italienische Schweiz"="Italienische Schweiz",
                                   "Romanische Schweiz"= "Romanische Schweiz", .default = NA_character_)


#Create new dataset with recoded variables only
summary(Daten_GMMP)
Daten_GMMP_clean <- Daten_GMMP[,37:52]

#Title needs some further recoding
Daten_GMMP_clean$titlefclean <- recode(Daten_GMMP_clean$titlef, "@20min"="20Minuten_de", "@24heuresch"="24Heures", "@Blickch"="Blick",
  "@CdT_Online"="CorriereDelTicino", "@LeTemps"="LeTemps", "@NZZ"="NZZ", "@RTRSRG"="RTR",
  "@srfnews"="SRF", "@tagesanzeiger"="Tagesanzeiger", "@tdgch"="TribuneDeGenève",
  "20 Minuten"="20Minuten_de", "20 Minutes Lausanne"="20Minuten_fr", "20min.ch"="20Minuten_de",
  "20min.ch_ro"="20Minuten_fr", "24 Heures"="24Heures", "Basler Zeitung"="BaslerZeitung",
  "Blick"="Blick", "Blick.ch"="Blick", "Corriere del Ticino"="CorriereDelTicino",
  "Corriere.ch"="CorriereDelTicino", "Der Bund"="DerBund",
  "La LibertÃ©"="LaLiberté", "La Quotidiana"="LaQuotidiana", "La RÃ©gion Nord Vaudois"="LaRégionNordVaudois",
  "Le Nouvelliste"="LeNouvelliste", "Le Temps"="LeTemps", "Le Temps.ch"="LeTemps",
  "Luzerner Zeitung"="LuzernerZeitung", "Nau.ch"="Nau.ch", "Neue ZÃ¼rcher Zeitung (NZZ)"="NZZ",
  "Nordwestschweiz"="Nordwestschweiz", "Radio_RSI_ReteUnoRadiogiornale_08.00"="RSI",
  "Radio_RSI_ReteUnoRadiogiornale_12.30"="RSI", "Radio_RTR_Actualidad"="RTR",
  "Radio_RTS_La1ere_08.00"="RTS", "Radio_RTS_La1ere_12.30"="RTS", "Radio_SRF_06.30"="SRF",
  "Radio_SRF_12.00"="SRF", "Radio_SRF_17.30"="SRF", "RSI.ch"="RSI", "RTR.ch"="RTR", "RTS.ch"="RTS",
  "SRF.ch"="SRF", "Tagesanzeiger"="Tagesanzeiger", "Tagesanzeiger.ch"="Tagesanzeiger",
  "Tio.ch"="Tio.ch", "Tribune de GenÃ¨ve"="TribunedeGenève", "TV_RSI_Telegiornale_12.30"="RSI",
  "TV_RSI_Telegiornale_19.30"="RSI", "TV_RTR_Telesguard"="RTR", "TV_RTS_LeJournal_12.45"="RTS",
  "TV_RTS_LeJournal_19.30"="RTS", "TV_SRF1_Tagesschau_Abend"="SRF", "TV_SRF1_Tagesschau_Mittag"="SRF",
  "TV_SRF1_Tagesschau_Nacht"="SRF", "Watson.ch"="Watson.ch", .default = NA_character_)

Daten_GMMP_clean$verlag <- recode(Daten_GMMP_clean$titlefclean,
                                  "20Minuten_de"="TXGroup", "24Heures"="TXGroup","Blick"="Rignier",              
                                  "CorriereDelTicino"="CorriereDelTicino", "LeTemps"="TXGroup", "NZZ"="NZZ-Mediengruppe",               
                                  "RTR"="SRG","SRF"="SRG", "Tagesanzeiger"="TXGroup", "TribuneDeGenève"="TXGroup",
                                  "20Minuten_fr"="TXGroup", "BaslerZeitung"="TXGroup", "DerBund"="TXGroup", "LaLiberté"="Holding St. Paul",          
                                  "LaQuotidiana"="Somedia", "LaRégionNordVaudois"="La Région Hebdo SA", "LeNouvelliste"="Editions Le Nouvelliste",      
                                  "LuzernerZeitung"="CH Media", "Nau.ch"="Nau media AG", "Nordwestschweiz"="CH Media",
                                  "RSI"="SRG", "RTS"="SRG", "Tio.ch"="TXGroup", "TribunedeGenève"="TXGroup","Watson.ch"="AZ Medien")


##Analyses


#Über wie viele Frauen wurde geschrieben?
ArtikelGeschlechtVerteilung <- Daten_GMMP_clean%>%
  filter(!is.na(sex_agentf))%>%
  ggplot(aes(sex_agentf))+
  geom_bar(aes(fill = sex_agentf), width = 0.6)+
  labs(y="Anzahl Artikel",
       x="Geschlecht der erwähnten Person",
       title = "Verteilung der Erwähnungen nach Geschlecht",
       caption = "Quelle: GMMP Switzerland 2020")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("Männer"="#F8CD86","Frauen"="#704D9E"))

ArtikelGeschlechtVerteilung

#Wieviele Frauen schreiben?
JounrnalistinnenGeschlecht <-Daten_GMMP_clean %>%
  filter(!is.na(sex_journf))%>%
  ggplot(aes(sex_journf))+
  geom_bar(aes(fill = sex_journf), width = 0.6)+
  labs(y="Anzahl Artikel",
       x="Geschlecht der Journalist:in",
       title = "Verteilung der Journalist:innen nach Geschlecht",
       caption = "Quelle: GMMP Switzerland 2020")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("Männer"="#F8CD86","Frauen"="#704D9E"))

JounrnalistinnenGeschlecht

#Haben Frauen mehr über Frauen geschrieben?
FrauenEinfluss <- Daten_GMMP_clean %>%
  filter(!is.na(sex_journf))%>%
  filter(!is.na(sex_agentf))%>%
  ggplot(aes(sex_agentf, sex_journf))+
  geom_jitter(alpha=0.2)+
  theme_classic()+
  labs(y=" Geschlecht der Journalist:in",
       x="Geschlecht der Person im Artikel",
       title = "Verhähltniss der Geschlechter zwischen Journalist:innen und Akteur:innen",
       caption = "Quelle: GMMP Switzerland 2020")+
  geom_text(x="Frauen", y="Frauen", label="10%", color="#704D9E", size=10)+
  geom_text(x="Frauen", y="Männer", label="15%", color="#704D9E", size=10)+
  geom_text(x="Männer", y="Frauen", label="29%", color="#704D9E", size=10)+
  geom_text(x="Männer", y="Männer", label="46%", color="#704D9E", size=10)
  
FrauenEinfluss


#Wie korrelieren die Anzahl der Berichte über Frauen mit dem Anteil Journalistinnen?
TotalCount <- Daten_GMMP_clean%>%
  filter(!is.na(sex_journf) & !is.na(sex_agentf))%>%
  group_by(titlefclean)%>%
  summarise(total_articles = length(titlefclean))

GenderedCountJourn <-  Daten_GMMP_clean%>%
  filter(!is.na(sex_journf) & !is.na(sex_agentf))%>%
  group_by(titlefclean, sex_journf)%>%
  summarise(gendered_count_journ = length(sex_journf))

JoinedCountJourn = inner_join(TotalCount, GenderedCountJourn, by = "titlefclean")%>%
  mutate(percentage_gender_journ = gendered_count_journ / total_articles)

GenderedCountAgent <-  Daten_GMMP_clean%>%
  filter(!is.na(sex_journf) & !is.na(sex_agentf))%>%
  group_by(titlefclean, sex_agentf)%>%
  summarise(gendered_count_agent = length(sex_agentf))

JoinedCountAgent = inner_join(TotalCount, GenderedCountAgent, by = "titlefclean")%>%
  mutate(percentage_gender_agent = gendered_count_agent / total_articles)

BothJoined = inner_join(JoinedCountAgent, JoinedCountJourn, by = c("titlefclean" = "titlefclean", "sex_agentf" = "sex_journf"))
BothWithoutMen = BothJoined %>% filter(as.integer(sex_agentf) == 1)

PercentageBoth <- BothWithoutMen%>%
  ggplot(aes(percentage_gender_agent, percentage_gender_journ))+
  geom_point()+
  geom_label_repel(aes(label = titlefclean),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   max.overlaps = Inf)+
  theme_classic()+
  labs(y="Anteil Journalistinnen",
       x="Anteil Frauen in Berichterstattung",
       title = "Anteil der Frauen in Berichterstattung und bei den Journalistinnen nach Medium",
       caption = "Quelle: GMMP Switzerland 2020")
   
PercentageBoth


PercentageBoth2 <- BothWithoutMen%>%
  ggplot(aes(percentage_gender_agent, percentage_gender_journ))+
  geom_smooth(method = lm)+
  theme_classic()+
  labs(y="Anteil Journalistinnen",
       x="Anteil Frauen in Berichterstattung",
       title = "Zusammenhang der Frauen in Berichterstattung und bei den Journalistinnen nach Medium",
       caption = "Quelle: GMMP Switzerland 2020, Medienmonitor Schweiz")
 
PercentageBoth2


#Wie korrelieren die Anzahl der Berichte über Frauen mit dem Anteil Journalistinnen nach Verlag?
TotalCount <- Daten_GMMP_clean%>%
  filter(!is.na(sex_journf) & !is.na(sex_agentf))%>%
  group_by(verlag)%>%
  summarise(total_articles = length(verlag))

GenderedCountJourn <-  Daten_GMMP_clean%>%
  filter(!is.na(sex_journf) & !is.na(sex_agentf))%>%
  group_by(verlag, sex_journf)%>%
  summarise(gendered_count_journ = length(sex_journf))

JoinedCountJourn = inner_join(TotalCount, GenderedCountJourn, by = "verlag")%>%
  mutate(percentage_gender_journ = gendered_count_journ / total_articles)

GenderedCountAgent <-  Daten_GMMP_clean%>%
  filter(!is.na(sex_journf) & !is.na(sex_agentf))%>%
  group_by(verlag, sex_agentf)%>%
  summarise(gendered_count_agent = length(sex_agentf))

JoinedCountAgent = inner_join(TotalCount, GenderedCountAgent, by = "verlag")%>%
  mutate(percentage_gender_agent = gendered_count_agent / total_articles)

BothJoined = inner_join(JoinedCountAgent, JoinedCountJourn, by = c("verlag" = "verlag", "sex_agentf" = "sex_journf"))
BothWithoutMen = BothJoined %>% filter(as.integer(sex_agentf) == 1)


PercentageBothVerlag <- BothWithoutMen%>%
  ggplot(aes(percentage_gender_agent, percentage_gender_journ))+
  geom_point()+
  geom_label_repel(aes(label = verlag),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   max.overlaps = Inf)+
  theme_classic()+
  labs(y="Anteil Journalistinnen",
       x="Anteil Frauen in Berichterstattung",
       title = "Anteil der Frauen in Berichterstattung und bei Journalistinnen nach Verlag",
       caption = "Quelle: GMMP Switzerland 2020, Medienmonitor Schweiz")
PercentageBothVerlag


PercentageBothVerlag2 <- BothWithoutMen%>%
  ggplot(aes(percentage_gender_agent, percentage_gender_journ))+
  geom_smooth(method = lm)+
  theme_classic()+
  labs(y="Anteil Journalistinnen",
       x="Anteil Frauen in Berichterstattung",
       title = "Zusammenhang Berichterstattung und Journalistinnen nach Verlag",
       caption = "Quelle: GMMP Switzerland 2020, Medienmonitor Schweiz")

PercentageBothVerlag2


