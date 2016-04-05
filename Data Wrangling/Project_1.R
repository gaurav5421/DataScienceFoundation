library(tidyr)
library(dplyr)
library(stringr)

data1 <- read.csv("C:/Data Science Foundation/Data Wrangling/refine_original.csv",header = TRUE)


phill <- data1 %>% filter(str_detect(data1$company,"^f|p|P")) 
akzo <- data1 %>% filter(str_detect(data1$company,"^a|A")) 
van <- data1 %>% filter(str_detect(data1$company,"^v|V")) 
uni <- data1 %>% filter(str_detect(data1$company,"^u|U")) 

phill$company <- 'phillips'
akzo$company <- 'akzo'
van$company <- 'van houten'
uni$company <- 'unilever'


data2 <- bind_rows(akzo,phill,uni,van)

data3 <- separate(data2,Product.code...number,c("product_code","product_number"), sep = "-")

# mutate(data3, product_category = ifelse(grepl('x' , data3$product_code), "Laptop",
#                                   ifelse(grepl('p' , data3$product_code), "Smartphone",
#                                   ifelse(grepl('v' , data3$product_code), "TV" , "Tablet"))))

data3 %>% filter(product_code == 'x') %>% mutate(product_category = 'Laptop') -> x
data3 %>% filter(product_code == 'p') %>% mutate(product_category = 'Smartphone') -> p
data3 %>% filter(product_code == 'v') %>% mutate(product_category = 'TV') -> v
data3 %>% filter(product_code == 'q') %>% mutate(product_category = 'Tablet') -> q

data4 <- bind_rows(x,p,v,q)

data4 <- unite(data4,"full_address", address, city, country, sep = ",")

# data4[c("company_philips", "company_akzo", "company_van_houten", "company_unilever",
#         "product_smartphone", "product_tv", "product_laptop" , "product_tablet")] <- 0

mutate(data4,company_phillips = ifelse(company %in% c("phillips") , 1 ,0)) -> cp
mutate(data4,company_akzo = ifelse(company %in% c("akzo") , 1 ,0)) -> ca
mutate(data4,company_unilever = ifelse(company %in% c("unilever") , 1 ,0)) -> cu
mutate(data4,company_van_houten = ifelse(company %in% c("van houten") , 1 ,0)) -> cvh
mutate(data4,product_smartphone = ifelse(product_code %in% c("p") , 1 ,0)) -> ps
mutate(data4,product_tv = ifelse(product_code %in% c("v") , 1 ,0)) -> ptv
mutate(data4,product_laptop  = ifelse(product_code %in% c("x") , 1 ,0)) -> pl
mutate(data4,product_tablet = ifelse(product_code %in% c("q") , 1 ,0)) -> pt

data4 <- left_join(data4, cp, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, ca, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, cu, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, cvh, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, ps, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, ptv, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, pl, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))
data4 <- left_join(data4, pt, by = c("company" , "product_code" , "product_number" ,"full_address" , "name" , "product_category"))

write.csv(arrange(data4,company), file = "C:/Data Science Foundation/Data Wrangling/refine_clean.csv")