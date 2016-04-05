library(caTools)

Reviews <- read.csv("C:/Data Science Foundation/Data Analysis in Depth/quality.csv",header = TRUE)


set.seed(88)

split <- sample.split(Reviews$PoorCare , SplitRatio = 0.75)

ReviewsTrain <- subset(Reviews , split == TRUE)
ReviewsTest <- subset(Reviews , split == FALSE)

ReviewsLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = ReviewsTrain, family = binomial)