data <- read.csv("adac_test.csv")

# fehlende Werte Standarisieren, also "" -> NA etc.
data[data == ""] <- NA
data[data == "n.b."] <- NA
data[data == "nicht bekannt"] <- NA
data[data == "k.A."] <- NA
data[data == "keine Angaben"] <- NA
data[data == "Keine Angaben"] <- NA
data[data == "keine Angabe"] <- NA
data[data == "Keine Angabe"] <- NA

# Anteil der NA pro Spalte überprüfen
percentageNA <- apply(data, 2, function(col) sum(is.na(col))) / nrow(data)

# leere Splaten (also Spalten nur mit NA Einträgen) finden
emptyCols <- names(percentageNA[percentageNA == 1])

# leere Spalten entfernen, da sie keine Informationen beinhalten (meisten Zeilen enthalten, aber auch zu >90% nichts)
data[emptyCols] <- NULL

# doppelte Zeilen entfernen
data <- data[!duplicated(data), ]

# abspeichern
write.csv(data, "cleandedData.csv")
