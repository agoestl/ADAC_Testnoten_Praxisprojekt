data <- read.csv("adac_test.csv")

# subset the first 91 columns
x <- data[,c(1:91)]

# Count empty, na, n.b. cells in the current column
empty_na_nb <- numeric(length(x))

for(i in seq_along(x)) {
  empty_na_nb[i] <- sum(x[[i]] == "" | is.na(x[[i]]) | x[[i]] == "n.b.")
}

# convert it to a data frame 
empty_na_nb_df <- data.frame(columne = colnames(x), counter = empty_na_nb)

#calculate the percentage of the filtered values to all rows
empty_na_nb_df$in_percent <- round((empty_na_nb_df[[2]] / nrow(x) * 100), digits = 2)


#filter those categories out where are to manny missing etc. values are 
cutoff = 25
not_usable <- subset(empty_na_nb_df, in_percent >= cutoff)
sprintf("%s Spalten sind NICHT benutzbar" , nrow(not_usable))

#data that can be used 
usable <- subset(empty_na_nb_df, in_percent < cutoff)
sprintf("%s Spalten sind benutzbar" , nrow(usable))

# the data set we started with gets updated and the useless columnes removed
cleaned_data <- subset(x, select = -c(which(colnames(x) %in% not_usable$columne)))



