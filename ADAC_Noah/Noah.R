data <- read.csv("adac_test.csv") 
# subset columns 184-275 
sdata <- data[,184:275]
 # missende Werte alle als N.A definieren 
sdata[sdata == ""] <- NA 
sdata[sdata == "k.a"] <- NA 
sdata[sdata == "n.b."] <- NA
sdata[sdata == "Keine Angabe"] <- NA
sdata[sdata == "nicht bekannt"] <- NA
sdata[sdata == "keine Angaben"] <- NA

View(sdata)
# percentage of NA  in each column. 
  NApercentage <- sapply(sdata, function(x) mean(is.na(x)))
#Spalten die nicht benutzbar sind also nur NAs
uselles <- names(NApercentage[NApercentage == 1])
View(uselles)
# Spalten die ( bis zu weiteren infos ) benutzbar sind 
usefull <- names(NApercentage[NApercentage < 1])
View(usefull)
#remove the uselles columns of the dataset (sdata)
clean_sdata <- sdata[, usefull]
View(clean_sdata)
