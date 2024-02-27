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

# Anteil der NA pro Spalte überprüfen
percentageNA <- apply(dataSub, 2, function(col) sum(is.na(col))) / nrow(data)

# leere Splaten (also Spalten nur mit NA Einträgen) finden
emptyCols <- names(percentageNA[percentageNA == 1])

# leere Spalten entfernen, da sie keine Informationen beinhalten (meisten Zeilen enthalten, aber auch zu >90% nichts)
dataSub[emptyCols] <- NULL

# Einheiten von den Einträgen der Spalten entfernen
# TODO