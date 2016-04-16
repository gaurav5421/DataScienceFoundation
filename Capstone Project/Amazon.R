library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)
library(dplyr)

Amazon <- read.csv("C:/DataScienceFoundation/Datasets/Reviews.csv", header = TRUE, 
                   col.names = c("ID","ProductId","UserId","HelpfulnessNumerator","HelpfulnessDenominator","Score",
                                 "Time","Summary","Text"),
                   colClasses = c("NULL","character","character","NULL","NULL","integer","NULL","NULL","NULL"))

Amazon <- Amazon[,c("UserId","ProductId","Score")]

Amazon <- Amazon[!duplicated(Amazon[1:2]),] ## To get unique values

Amazon <- subset(Amazon, table(Amazon$UserId) > 1)

rev_by_user <- split(x= Amazon[,"ProductId"], f = Amazon$UserId)

rev_by_user <- lapply(rev_by_user, unique)

rev_by_user <- as(rev_by_user, "transactions")

itemFrequencyPlot(rev_by_user, support = .08 ,cex.names = 0.8)

rules <- apriori(rev_by_user,parameter = list(support =0.02, confidence = 0.6))



# for (i in 1:length(rev_by_user))
# {
#   if(length(rev_by_user[[i]]) > 1)
#   {
#     rev[i] <- rev_by_user[i]
#   }
#   
# }
# 
# rev <- rev[! sapply(rev,is.null)] #to remove the nulls
