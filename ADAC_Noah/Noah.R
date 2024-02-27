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
