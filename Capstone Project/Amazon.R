library(tidyr)
library(dplyr)
library(arules)
library(RSQLite)


Amazon <- read.csv("C:/Data Science Foundation/EDA/Reviews.csv", header = TRUE, stringsAsFactors = FALSE , encoding = "UTF-8")
# 
# Amazon_full <- read.csv("C:/Data Science Foundation/EDA/Reviews_orig.csv", header = TRUE, stringsAsFactors = FALSE , encoding = "UTF-8")
# 

Amazon_ID <- Amazon[,c("ProductId","UserId","Score")]

Amazon_ID$UserId <- factor(Amazon_ID$UserId)

Amazon_ID[!duplicated(Amazon_ID[1:2]),] ## To get unique values

Amazon_ID_sample <- head(Amazon_ID,100)

write.csv(Amazon_ID, file = "C:/Data Science Foundation/csvtest.csv" ,row.names = FALSE)

rev_by_user <- split(x= Amazon_ID$ProductId, f = Amazon_ID$UserId)

rev_by_user <- lapply(rev_by_user, unique)

rev_by_user <- as(rev_by_user, "transactions")

itemFrequencyPlot(rev_by_user, cex.names = 1.5)


con <- dbConnect(RSQLite::SQLite(), ":memory:")

dbWriteTable(con, "Amazon", Amazon, row.names = FALSE)

res <- dbSendQuery(con, "select UserId, ProductId, Score, ROW_NUMBER() over (Partition By UserId Order)")

dbFetch(res)