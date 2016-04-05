library(ggplot2)
library(dplyr)

data(diamonds)

diamonds$color

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9') +
  scale_x_continuous(breaks = seq(0,2000,100) , limits = c(0,2000))

ggsave('diamond_price.png')

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(color = 'black', fill = '#099DD9') +
  scale_x_continuous() +
  facet_wrap(~cut)
  
filter(diamonds , price >= max(diamonds$price))
filter(diamonds , price <= min(diamonds$price))

diamonds %>% group_by(cut) %>% summarise(median = median(price))

ggplot(aes(x = price), data = diamonds) + 
  geom_histogram(color = 'black', fill = '#099DD9') +
  scale_x_continuous() +
  facet_wrap(~cut , scales = "free_y")


ggplot(aes(x = price/carat), data = diamonds) + 
  geom_histogram(binwidth = 0.05,color = 'black', fill = '#099DD9') +
  scale_x_log10() +
  facet_wrap(~cut , scales = "free_y")

ggplot(aes(x = cut, y = price), data = subset(diamonds, !is.na(cut))) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,7000))

ggplot(aes(x = clarity, y = price), data = subset(diamonds, !is.na(clarity))) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,7000))

ggplot(aes(x = color, y = price), data = subset(diamonds, !is.na(color))) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,8000))



ggplot(aes(x = color, y = price/carat), data = subset(diamonds, !is.na(color))) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0,6000))


ggplot(aes(x = carat) ,data = subset(diamonds)) +
  geom_freqpoly(aes(color = carat),binwidth = 0.1) + 
  scale_x_continuous(breaks = seq(0, 5, 0.1)) +
  scale_y_continuous(breaks = seq(0, 10000, 1000)) +
  xlab('Carat Size') + 
  ylab('Number of diamonds with that carat size')

################################################### Moving onto exploring with 2 variables#########################################################

ggplot(aes(x = price, y = x), data = diamonds) +  ## Scatterplot
  geom_point(alpha = 1/20) +
  xlim(326,7500) +
  ylim(3,9) 

# Observations :- 
# 1) There are a few outliers .
# 2) There is a concentration of price between 326$ and 3000$ 
# 3) There is no diamonds priced around 1500$.

cor.test(x = diamonds$price, y = diamonds$x, method = c("pearson")) - 0.88

cor.test(x = diamonds$price, y = diamonds$y, method = c("pearson")) - 0.86

cor.test(x = diamonds$price, y = diamonds$z, method = c("pearson")) - 0.86

ggplot(aes(x = price, y = depth), data = diamonds) +  ## Scatterplot
  geom_point(alpha = 1/20) +
  ylim(50,70)

cor.test(x = diamonds$price, y = diamonds$depth, method = c("pearson"))


##The correlation coefficient is very low which tells there is not much of a relation between the two
x  = floor(1/100 * nrow(diamonds))

ggplot(aes(x = carat, y = price), data = diamonds[floor(1/100 * nrow(diamonds) + 1) :53940 , ] ) +  ## Scatterplot
  geom_point()


diamonds$volume <- (diamonds$x*diamonds$y*diamonds$z)


ggplot(aes(x = volume, y = price), data = diamonds) +  ## Scatterplot
  geom_jitter(alpha = 1/20) +
  xlim(0,500) 


# 1) Almost all the observations are within volume 500 with a few outliers
# 2) The observations are dense within volume 175
# 3) There is some kind of positive correlation between the 2 variables.

diamonds_subset <- subset(diamonds , volume > 0 & volume <= 800)

cor.test(x = diamonds_subset$volume , y = diamonds_subset$price, method = c("pearson")) - 0.92


ggplot(aes(x = volume, y = price), data = diamonds_subset) +  ## Scatterplot
  geom_point() +
  geom_smooth(method = 'lm' , color = 'red')
  
diamondsByClarity <- diamonds %>% 
  group_by(clarity) %>%
  summarise(mean_price  = mean(price),
            median_price  = median(price),
            min_price  = min(price),
            max_price = max(price),
            n = n()) %>% 
  arrange(clarity)



diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

p3 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +  ## Scatterplot
  geom_bar(stat = "identity")

p4 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +  ## Scatterplot
  geom_bar(stat = "identity")

grid.arrange(p3,p4)


# 
# Mean price by clarity :- 
#   
#   1) SI2 has the highest mean price and VVS1 has the lowest
# 
# Mean price by color:- 
#   
#   1) J has the highest mean price and E has the lowest


