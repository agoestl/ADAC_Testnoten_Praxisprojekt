data <- read.csv("adac_test.csv")

# subset of data
dataSub <- data[, 276:364]

# fehlende Werte Standarisieren, also "" -> NA etc.
dataSub[dataSub == ""] <- NA
dataSub[dataSub == "n.b."] <- NA
dataSub[dataSub == "nicht bekannt"] <- NA
dataSub[dataSub == "k.A."] <- NA
dataSub[dataSub == "keine Angaben"] <- NA
dataSub[dataSub == "Keine Angaben"] <- NA
dataSub[dataSub == "keine Angabe"] <- NA
dataSub[dataSub == "Keine Angabe"] <- NA

#konkrete fehlende Werte anpassen
dataSub$Leistung...Drehmoment..Elektromotor.2. <- gsub("n.b.", "-", dataSub$Leistung...Drehmoment..Elektromotor.2.)
dataSub[dataSub == "- kW (- PS) / - Nm"] <- NA

# Anteil der NA pro Spalte überprüfen
percentageNA <- apply(dataSub, 2, function(col) sum(is.na(col))) / nrow(data)

# leere Splaten (also Spalten nur mit NA Einträgen) finden
emptyCols <- names(percentageNA[percentageNA == 1])

# leere Spalten entfernen, da sie keine Informationen beinhalten (meisten Zeilen enthalten, aber auch zu >90% nichts)
dataSub[emptyCols] <- NULL

# Einheiten der Einträgen von  interessanten Spalten entfernen
# Spalte "Reichweite.WLTP..elektrisch."
dataSub$Reichweite.WLTP..elektrisch. <- gsub(" km", "", dataSub$Reichweite.WLTP..elektrisch.)
colnames(dataSub)[colnames(dataSub) == "Reichweite.WLTP..elektrisch."] <- "Reichweite.WLTP..elektrisch.in.km"
dataSub$Reichweite.WLTP..elektrisch.in.km <- as.numeric(dataSub$Reichweite.WLTP..elektrisch.in.km)
# Spalte "Reichweite.WLTP.City..elektrisch."
dataSub$Reichweite.WLTP.City..elektrisch. <- gsub(" km", "", dataSub$Reichweite.WLTP.City..elektrisch.)
colnames(dataSub)[colnames(dataSub) == "Reichweite.WLTP.City..elektrisch."] <- "Reichweite.WLTP.City..elektrisch.in.km"
dataSub$Reichweite.WLTP.City..elektrisch.in.km<- as.numeric(dataSub$Reichweite.WLTP.City..elektrisch.in.km)
# Spalte "Batteriekapazität..Netto..in.kWh"
dataSub$Batteriekapazität..Netto..in.kWh <- as.numeric(dataSub$Batteriekapazität..Netto..in.kWh)
# Spalte "CO2.Wert.kombiniert..WLTP."
dataSub$CO2.Wert.kombiniert..WLTP. <- gsub(" g/km", "", dataSub$CO2.Wert.kombiniert..WLTP.)
colnames(dataSub)[colnames(dataSub) == "CO2.Wert.kombiniert..WLTP."] <- "CO2.Wert.kombiniert..WLTP.in.g.per.km"
dataSub$CO2.Wert.kombiniert..WLTP.in.g.per.km <- as.numeric(dataSub$CO2.Wert.kombiniert..WLTP.in.g.per.km)
