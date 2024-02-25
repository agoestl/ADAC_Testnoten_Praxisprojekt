data <- read.csv("adac_test.csv")

# subset the first 91 columns
x <- data[,c(1:91)]

# Count empty, na, n.b. cells in the current column
empty_na_nb_kA <- numeric(length(x))

for(i in seq_along(x)) {
  empty_na_nb_kA[i] <- sum(x[[i]] == "" | is.na(x[[i]]) | x[[i]] == "k.A.")
}

# convert it to a data frame 
as_df <- data.frame(columne = colnames(x), counter = empty_na_nb_kA)

#calculate the percentage of the filtered values to all rows
as_df$in_percent <- round((as_df[[2]] / nrow(x) * 100), digits = 2)


#filter those categories out where are to manny missing etc. values are (77 cause Gesamtzuggewicht could be calculated)
cutoff = 77
not_usable <- subset(as_df, in_percent >= cutoff)
sprintf("%s Spalten sind NICHT benutzbar" , nrow(not_usable))

#data that can be used 
usable <- subset(as_df, in_percent < cutoff)
sprintf("%s Spalten sind benutzbar" , nrow(usable))

# the data set we started with gets updated and the useless columnes removed
cleaned_data <- subset(x, select = -c(which(colnames(x) %in% not_usable$columne)))


#######################################

# validate the data 
ncol(cleaned_data)
# Baureihe is a combination of many colums, TSN and HSN just shows brand and modell number, 
# Leistung...Drehmoment..Verbrennungsmotor. is combination of columns, Hubraum..Verbrennungsmotor. is used to calculate Leistung
# Leistung.maximal.bei.U.min...Verbrennungsmotor. is used for Leistung, Drehmoment.maximal.bei.U.min...Verbrennungsmotor. is used for Leistung

validate_data <- subset(cleaned_data, select = -c(Baureihe, HSN.Schlüsselnummer, TSN.Schlüsselnummer, Leistung...Drehmoment..Verbrennungsmotor., 
                                                  Hubraum..Verbrennungsmotor., Leistung.maximal.bei.U.min...Verbrennungsmotor.,
                                                  Drehmoment.maximal.bei.U.min...Verbrennungsmotor.))
ncol(validate_data)
#check for diffrent modells and the frequency
unique_Modell <- data.frame(sort(table(validate_data$Modell), decreasing = TRUE))
head(unique_Modell)
#some modells appear often than others -> no equal distribiution

#check for diffrent Marken and the frequency
unique_Marke <- data.frame(sort(table(validate_data$Marke), decreasing = TRUE))
head(unique_Marke)

#we remove the (z.b. S-Klasse) for better analysis later 
validate_data[["Fahrzeugklasse"]] <- gsub("\\s*\\([^\\)]+\\)", "", validate_data[["Fahrzeugklasse"]])

#check for diffrent Fahrzeugklassen and the frequency
unique_Fahrzeugklassen <- data.frame(sort(table(validate_data$Fahrzeugklasse), decreasing = TRUE))
head(unique_Fahrzeugklassen)
#could be interesting for further analysis

#### calculate the Gesamtzuggewicht
# remove the kg to analysis it easier
columne_to_remove_kg <- c("Leergewicht..EU.", "Gesamtzuggewicht" ,"Zul..Gesamtgewicht", "Zuladung", "Anhängelast.gebremst.12.", "Anhängelast.ungebremst","Stützlast","Dachlast")
for (col in columne_to_remove_kg) {
  validate_data[[col]] <- gsub(" kg$", "", validate_data[[col]])
}

# columne as numeric 
validate_data$Zul..Gesamtgewicht <- as.numeric(validate_data$Zul..Gesamtgewicht)
validate_data$Anhängelast.gebremst.12. <- as.numeric(validate_data$Anhängelast.gebremst.12.)

#calculate
validate_data$Gesamtzuggewicht <- ifelse(is.na(validate_data$Zul..Gesamtgewicht) | is.na(validate_data$Anhängelast.gebremst.12.), NA,
                                         validate_data$Zul..Gesamtgewicht + validate_data$Anhängelast.gebremst.12.)
#decide wheater we use Modellstart/ende or Baureihenstart/ende 
#validate_data <- subset(validate_data, select = -c())

#bei Zusatzausstattung den Aufpreis durch einen anderen Wert ersetzen zb. (180€ --> Aufpreis)
