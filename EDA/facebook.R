library(ggplot2)
library(gridExtra)
library(dplyr)
library(alr3)
library(zoo)
library(tidyr)
library(recommenderlab) 

pf <- read.csv("C:/Data Science Foundation/EDA/data_sets/pseudo_facebook.tsv" , sep = '\t')


# qplot(x = dob_day , data  = pf) +
#   stat_bin(binwidth = 0.01)
#   scale_x_discrete(breaks = 1:31)

ggplot(aes(x = dob_day), data = pf) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month, ncol = 3)


ggplot(aes(x = friend_count), data = subset(pf, !is.na(gender))) +
  geom_histogram(binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)


table(pf$gender)
by(pf$friend_count,pf$gender, summary)

ggplot(aes(x = tenure/365), data = pf) + 
  geom_histogram(binwidth = 0.25, color = 'black', fill = '#099DD9') +
  scale_x_continuous(breaks = seq(1,7,1) , limits = c(0,7)) +
  xlab('Number of years using Facebook') + 
  ylab('Number of users in sample')

ggplot(aes(x = age), data = pf) + 
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9') +
  scale_x_continuous(breaks = seq(0,120,10) , limits = c(0,125)) +
  xlab('Age') + 
  ylab('Number of users in sample')

p1 <- ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram(binwidth = 25, color = 'black', fill = '#099DD9') +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +

p2 <- ggplot(aes(x = friend_count + 1), data = pf) +
  geom_histogram(binwidth = 0.1, color = 'black', fill = '#099DD9') +
  scale_x_log10() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

p3 = ggplot(aes(x = friend_count), data = pf) +
  geom_histogram(binwidth = 25, color = 'black', fill = '#099DD9') +
  scale_x_sqrt() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

grid.arrange(p1,p2,p3 , ncol = 1)



ggplot(aes(x = www_likes) ,data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender)) + 
  scale_x_log10() + 
  xlab('Number of likes') + 
  ylab('Percentage of users with that friend count')

ggplot(aes(x = www_likes), data = subset(pf, !is.na(gender))) +
  geom_histogram() + 
  scale_x_log10() +  
  facet_wrap(~gender)

by(pf$www_likes , pf$gender, sum)

ggplot(aes(x = gender, y = friendships_initiated), data = subset(pf, !is.na(gender))) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,130))



by(pf$friendships_initiated, pf$gender,  summary)



############################################ Moving onto exploring with 2 variables#######################################

ggplot(aes(x = age, y = friend_count), data = pf) +  ## Scatterplot
  geom_point() +
  xlim(13,90)


ggplot(aes(x = age, y = friend_count), data = pf) +  ## Scatterplot
  geom_jitter(alpha = 1/20) +  
  xlim(13,90) +


 ggplot(aes(x = age, y = friend_count), data = pf) +  ## Scatterplot
  geom_point(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13,90) +
  coord_trans(y = 'sqrt') 


ggplot(aes(x = age, y = friendships_initiated), data = pf) +  ## Scatterplot
  geom_point(alpha = 1/20 , position = position_jitter(h = 0)) +
  scale_x_continuous(breaks = seq(10, 120, 10)) +
  coord_trans(y = 'sqrt') 


pf.fc_by_age <- pf %>% 
  group_by(age) %>%
  summarise(friend_count_mean  = mean(friend_count),
            friend_count_median  = median(friend_count),
            n = n()) %>% 
  arrange(age)
  
ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +  ## line Graph
  geom_line() 

ggplot(aes(x = age, y = friend_count), data = pf) +  ## Overlaying summaries 
  geom_point(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
  xlim(13,90) +
  coord_trans(y = 'sqrt') +
  geom_line(stat = 'summary', fun.y = mean)

cor.test(x = pf$age, y = pf$friend_count, method = c("pearson"))


with( subset(pf , age <=70) , cor.test(age, friend_count))


ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +  ## scatterplot
  geom_point() +
  xlim(0,quantile(pf$www_likes_received , 0.95)) +
  ylim(0,quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm' , color = 'red')


cor.test(x = pf$www_likes_received , y = pf$likes_received, method = c("pearson"))

ggplot(aes(x = Month, y = Temp), data = Mitchell) +  ## scatterplot
  geom_point() +
  scale_x_discrete(breaks = seq(0, 203, 11))


cor.test(x = Mitchell$Month, y = Mitchell$Temp, method = c("pearson"))

pf$age_with_months <- pf$age +  (12 - pf$dob_month) /12


pf.fc_by_age_months <- pf %>% 
  group_by(age_with_months) %>%
  summarise(friend_count_mean  = mean(friend_count),
            friend_count_median  = median(friend_count),
            n = n()) %>% 
  arrange(age_with_months)

p1 <- ggplot(aes(x = age_with_months, y = friend_count_mean),
            data = subset(pf.fc_by_age_months , age_with_months < 71)) +  ## line Graph
  geom_line() 

 p2 <- ggplot(aes(x = age, y = friend_count_mean),
       data = subset(pf.fc_by_age , age < 71)) +  ## line Graph
  geom_line() 

 grid.arrange(p1,p2,ncol = 1)
 
 ############################################ Moving onto exploring with 3 variables####################################### 

 pf.fc_by_age_gender <- pf %>% 
   filter(!is.na(gender)) %>% 
   group_by(age,gender) %>% 
   summarise(mean_friend_count = mean(friend_count),
          median_friend_count = median(friend_count),
          n = n()) %>% 
   ungroup() %>%  
   arrange(age)

 ggplot(aes(x = age, y = mean_friend_count),
        data = subset(pf.fc_by_age_gender)) +  ## line Graph
   geom_line(aes(color = gender)) 

pf.fc_by_age_gender.wide <- spread(subset(pf.fc_by_age_gender[c('age' , 'gender', 'median_friend_count')],!is.na(gender)) , gender , median_friend_count)


ggplot(aes(x = age, y = female/male),
       data = subset(pf.fc_by_age_gender.wide)) +  ## line Graph
  geom_line() +
  geom_hline(yintercept = 1 , linetype = 2)

pf$year_joined <- 2014 - ceiling(pf$tenure/365)
 

pf$year_joined.bucket <- cut(pf$year_joined , breaks = c(2004,2009,2011,2012,2014))



ggplot(aes(x = age, y = friend_count),
       data = subset(pf , !is.na(year_joined.bucket))) +  ## line Graph
  geom_line(aes(color = year_joined.bucket) , stat = 'summary' , fun.y = median) 

  ggplot(aes(x = age, y = friend_count),
         data = subset(pf , !is.na(year_joined.bucket))) +  ## line Graph
  geom_line(aes(color = year_joined.bucket) , stat = 'summary' , fun.y = mean) +
  geom_line(stat = 'summary' , fun.y = mean , linetype = 2)
  
  with(subset(pf , tenure >= 1),  summary(friend_count / tenure))
  
  
  ggplot(aes(x = tenure, y = friendships_initiated / tenure),
         data = subset(pf , tenure >= 1)) +  ## line Graph
    geom_smooth(aes(color = year_joined.bucket))
  
  
  
  ##################################       Y O G U R T  -  D A T A S E T    ####################################################
  
  yo <- read.csv("C:/Data Science Foundation/EDA/data_sets/yogurt.csv")
  str(yo)
  
  yo$id <- factor(yo$id)
  
  ggplot(aes(x = price), data = yo) + 
    geom_histogram() 
  
yo <-  yo %>%   mutate(all.purchases = blueberry + pina.colada + plain + mixed.berry + strawberry) 

ggplot(aes(x = time, y = price), data = yo) +  ## scatterplot
  geom_point(alpha = 1/10) 
