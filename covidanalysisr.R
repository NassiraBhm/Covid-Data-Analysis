rm(list=ls()) 
install.packages('Hmisc')
library(Hmisc)
library(tidyverse)

data <- read_csv('COVID19_line_list_data.csv')
View(data)
glimpse(data)
describe(data)
names(data)

# What's the death rate?

data <- data %>% 
  mutate(death_dummy = as.integer(data$death != 0))

data$death_dummy

(sum(data$death_dummy) / nrow(data)) * 100


# Age

data %>% 
  filter(death_dummy == 1) %>% 
  summarise(meanAge_d = mean(age, na.rm = TRUE) )

data %>% 
  filter(death_dummy == 0) %>% 
  summarise(meanAge_a = mean(age, na.rm = TRUE) )

# This initial analysis reveals a considerable difference in average age
# between people who died (68.6) and those who survived (48.1).
# We'll perform a t test to check if this difference is statistically
# significant.


# t-test:   H0: there is no difference in mean age between those who died 
#               and those who survived
#           H1: the mean age of people who died is higher than those who
#               survived
# p-value threshold: 5%


dead <-data %>% 
  filter(death_dummy == 1) 

alive <-data %>% 
  filter(death_dummy == 0)

t.test(dead$age, alive$age, alternative="greater")

# p-value < 2.2e-16 : the p value is too small compared to the threshold
# so we can reject the null hypothesis and accept the alternative one;
# the difference in mean age between the two categories is statistically
# significant.


# Gender

male <- data %>% 
  filter(gender == 'male')

male %>% 
  summarise(mean(death_dummy, na.rm = TRUE))


female <- data %>% 
  filter(gender == 'female')

female %>% 
  summarise(mean(death_dummy, na.rm = TRUE))


#We notice a difference of about 5% between the mean deaths for males (8,46%)
# and femmales (3,66).
# Is this difference statistically significant?

# t-test:   H0: there is no difference in deaths between males and females 
#             
#           H1: males have higher chance of dying
#               
# p-value threshold: 5%

t.test(male$death_dummy, female$death_dummy, alternative="greater")

# p-value = 0.001053 is too small compared to our treshhold, we reject
# the null hypothesis and accept the alternative one; males have higher
# chance of death













