library(tidyr)
library(dplyr)
library(stringr)

data1 <- read.csv("C:/Data Science Foundation/Data Wrangling/WIN_FTP_logs.csv",header = TRUE)

separate(data1,IP,c("date","IP"), sep = " - ") -> data2

separate(data2,IP,c("IP","comments"), sep = ">") -> data3

data4 <- data.frame(data3$IP)

data4 <- unique(data4)

separate(data4,data3.IP,c("comments","IP"), sep = "User connected from") -> data5

separate(data5,IP,c("CRAP","IP"), sep = " ") -> data6

unique(data6) -> data7


dfread <- read.csv("C:/Data Science Foundation/Data Wrangling/IP.csv",header = TRUE)

all <- rbind(dfread,data7)


write.csv((all), file = "C:/Data Science Foundation/Data Wrangling/IP.csv",row.names = FALSE)

