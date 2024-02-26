dataSubset <- data[,92:183]
View(dataSubset)

#Define missing values, "keine Angaben", "Keine Angabe", "nicht bekannt" "k.A." and "n.b" as NA
dataSubset[dataSubset == ""] <- NA
dataSubset[dataSubset == "n.b"] <- NA
dataSubset[dataSubset == "k.A."] <- NA
dataSubset[dataSubset == "keine Angaben"] <- NA
dataSubset[dataSubset == "nicht bekannt"] <- NA
dataSubset[dataSubset == "Keine Angabe"] <- NA

# Calculate the proportion of NA's per column
na.proportion <- sapply(dataSubset, function(x) mean(is.na(x)))

#remove every column with na.proportion > cutoff (1 for now, they hold no information)
cutoff <- 1
keep.columns <- names(na.proportion[na.proportion < cutoff])
cleaned.dataSubset <- dataSubset[, keep.columns]

View(cleaned.dataSubset)


