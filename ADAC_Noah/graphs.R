library(ggplot2)
library(tidyverse)
library(dplyr)
data <- read.csv("cleandedData.csv")
#Thema 7 C02 Verbrauch , herstellerangabe vs Adac Ecotest
deutscheMarken <- c("Mercedes-Benz", "Opel", "Porsche", "smart", "ALPINA", "Audi", "BMW", "VW", "VW Nutzfahrzeuge")

# Spalte für deutsch/ausländisch erstellen
data <- data %>% 
  mutate(deutscher.Hersteller = Marke %in% deutscheMarken,
         deutscher.Hersteller = ifelse(deutscher.Hersteller, "deutsch", "ausländisch"))
# Baureihenstart zu Date umformatieren
data <- data %>% 
  mutate(Baureihenstart = as.Date(paste("01/", Baureihenstart, sep = ""), "%d/%m/%y"))
#top n marken rausfiltern 
data %>%
  count(Marke) %>%
  top_n(5, n) %>%
  pull(Marke) -> top_Marken
#CO2 Ausstoß in 2 Spalten teilen (Hersteelerangabe und ADAC ecotest)

data$C02.Ausstoß <- gsub("g pro km", "", data$C02.Ausstoß)
data$C02.Ausstoß <- gsub("\\(.*\\)","", data$C02.Ausstoß)

data <- data %>%
  mutate(spalten_getrennt = strsplit(data$C02.Ausstoß, "/")) %>%
  mutate(C02.Ausstoß.Hersteller = sapply(spalten_getrennt, `[`, 1),
         C02.Ausstoß.ADAC = sapply(spalten_getrennt, `[`, 2)) %>%
  select(-spalten_getrennt)  %>%
mutate(across(c(C02.Ausstoß.Hersteller, C02.Ausstoß.ADAC), as.numeric))
# Daten in dennen es ein wert Für Adac Ekotest und für den Herstellerangabe gibt ( damit mann es fair vergleichen kann).
gefilterte_daten <- data %>% filter(!is.na(C02.Ausstoß.Hersteller) & !is.na(C02.Ausstoß.ADAC))
#deutsche Autos
deutsche_autos <- data %>% filter( deutscher.Hersteller == "deutsch")
# top deutscher Hersteller 
data %>% filter(top_deutscher.Hersteller == "deutsch") %>% select(Marke) %>% count(Marke) %>% top_n(3, n) %>% pull(Marke) -> top_deutsche_Marken
# ausländische  Autos (gefiltert)
ausländische_autos_gefiltert <- gefilterte_daten %>% filter( deutscher.Hersteller == "ausländisch")

# Graphik deutsche Autos
deutsche_autos_gefiltert <- gefilterte_daten %>% filter( deutscher.Hersteller == "deutsch")
 Graphik_deutsch <- ggplot(deutsche_autos_gefiltert , aes(x = Baureihenstart,  )) + 
   geom_line(aes(y = C02.Ausstoß.ADAC, color = "ADAC Ekotest")) +
geom_line(aes(y = C02.Ausstoß.Hersteller , color = "Herstellerangaben")) +
  labs( x = "Bauenreihenstart", y= "C02 Ausstoß(g/km) " , title = "CO2-Ausstoß von Herstellerangaben und ADAC Ekotest über die Zeit bei deutschen Autos") +
  scale_color_manual(values = c("ADAC Ekotest" = "blue", "Herstellerangaben" = "red"),
                     labels = c("ADAC Ekotest", "Herstellerangaben"),
                     name = "Legende") +
  theme_minimal()
 Graphik_deutsch
#Graphik ausländische autos
Graphik_ausländisch  <-ggplot(ausländische_autos_gefiltert , aes(x = Baureihenstart,  )) + 
  geom_line(aes(y = C02.Ausstoß.ADAC, color = "ADAC Ekotest")) +
  geom_line(aes(y = C02.Ausstoß.Hersteller , color = "Herstellerangaben")) +
  labs( x = "Bauenreihenstart", y= "C02 Ausstoß(g/km) " , title = "CO2-Ausstoß von Herstellerangaben und ADAC Ekotest über die Zeit bei ausländischen Autos") +
  scale_color_manual(values = c("ADAC Ekotest" = "blue", "Herstellerangaben" = "red"),
                     labels = c("ADAC Ekotest", "Herstellerangaben"),
                     name = "Legende") +
  theme_minimal()
Graphik_ausländisch
