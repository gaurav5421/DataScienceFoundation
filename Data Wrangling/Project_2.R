library(tidyr)
library(dplyr)

data1 <- read.csv("C:/Data Science Foundation/Data Wrangling/titanic_original.csv",header = TRUE)

data1$embarked[(data1$embarked == "")] <- as.character('S')
 
data1$age[is.na(data1$age)] <- as.character(ceiling(mean(data1$age , na.rm = TRUE))) 

data1$boat[data1$boat == ""] <- NA

mutate(data1,has_cabin_number = ifelse(cabin == "" , 0 ,1)) -> data1

write.csv(data1, file = "C:/Data Science Foundation/Data Wrangling/titanic_clean.csv")