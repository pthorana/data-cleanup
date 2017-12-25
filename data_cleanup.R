#######################################################################
# ANLY 510 Analytics II: Principles & Appl                            #
# TEAM FAKE DATA SCIENTISTS                                           #
# Team Members:                                                       #
#   1. Pavan Kulkarni                                                 #
#   2. Mengqian Zha                                                   #
#   3. Praveen Thoranathula                                           #
# Assignment #7-collaboration on repository and data cleanup          #
# DATE: 12/24/2017                                                    #
#######################################################################

library(tidyr) 
library(dplyr)

# Load the Data
df <- read.csv("dirty_data.csv", sep = ",", na.strings = "")

# Convert the streets to Character String
df$Street <- as.character(df$Street)
df$Street.2 <- as.character(df$Street.2)

# Impute the Area Column using MICE Package

#Populate the missing values in the Area variable with an appropriate values (Birmingham, Coventry, Dudley, Sandwell, Solihull, Walsall or Wolverhampton)
if (!require(mice)) {install.packages("mice"); require(mice)}
#init = mice(df, maxit=0) 
#meth = init$method
#predM = init$predictorMatrix
#meth[c("Strange.HTML")]=""

set.seed(103)
imputed = mice(df, m=5)
imputed <- complete(imputed)

# Check to make sure all NAs are imputed
sapply(imputed, function(x) sum(is.na(x)))


# Remove special characters, padding (the white space before and after the text) from Street 1 and Street 2 variables. Make sure the first letters of street names are capitalized and the street denominations are following the same standard (for example, all streets are indicated as “str.”, avenues as “ave.”, etc.
df.imputed = imputed
if (!require(stringr)) {install.packages("stringr"); require(stringr)}
df.imputed$Street <- str_replace_all(df.imputed$Street, "[^[:alnum:]&\\,]", " ")
df.imputed$Street.2 <- str_replace_all(df.imputed$Street.2, "[^[:alnum:]&\\,]", " ")
df.imputed$Street <- str_to_title(df.imputed$Street)
df.imputed$Street.2 <- str_to_title(df.imputed$Street.2)

replace.words = data.frame(find=character(),replace=character())
replace.words <- rbind(replace.words, data.frame(find="Road",replace="Rd"))
replace.words <- rbind(replace.words, data.frame(find="Street",replace="St"))
replace.words <- rbind(replace.words, data.frame(find="Raod",replace="Rd"))

for (i in 1:nrow(replace.words)){
  patt <- paste0('\\b', replace.words[i,1], '\\b')
  repl <- paste(replace.words[i,2])
  df.imputed$Street <-gsub(patt, repl, df.imputed$Street)
  df.imputed$Street.2 <-gsub(patt, repl, df.imputed$Street.2)
}

# If the value in Street 2 duplicates the value in Street 1, remove the value in Street 2
df.imputed$Street.2[c(which(df.imputed$Street.2==df.imputed$Street))]<-NA

# Remove the “Strange HTML column”
df.imputed<-subset(df.imputed,select = -c(Strange.HTML))

# Write the Data back to csv file
write.csv(df.imputed,"clean_data.csv", na="", row.names = F)

#######################################################################
#                                                                     #
#                         END OF CODE                                 #
#                                                                     #
#######################################################################

