library(tidyverse)
library(broom)


task <- read_csv("https://raw.githubusercontent.com/jaywhitmire/myrepo/master/sample.csv")

names(task)
head(task)
str(task)
summary(task)
glimpse(task)


####  1. How many recommendation sets are in this data sample?

length(unique(task$recommendation_id))

# 2100  unique recommendation sets

  
  
####  2. Each recommendation set shows from 1 to 15 Taskers, what is:
##  - average number of Taskers shown
taskers_per_rec <- task %>% 
  group_by(recommendation_id) %>% 
  summarise(Taskers = n())

mean(taskers_per_rec$Taskers)

#14.29

##  - median  number of Taskers shown
median(taskers_per_rec$Taskers)

#15

####  3. How many total unique Taskers are there in this data sample?

length(unique(task$tasker_id))

# 830 unique taskers  
  
####  4. Which Tasker has been shown the most?

task %>% 
  group_by(tasker_id) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# tasker_id 1014508755 has been shown 608 times

##  Which Tasker has been shown the least?

task %>% 
  group_by(tasker_id) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  filter(count == 1)

#The 68 taskers above are tied for the least showings with 1 each
  
  
####  5. Which Tasker has been hired the most?

task %>% 
  group_by(tasker_id) %>% 
  summarise(hirings = sum(hired)) %>% 
  arrange(desc(hirings))

# tasker_id 1012043028 has been hired 59 times


##  Which Tasker has been hired the least?


task %>% 
  group_by(tasker_id) %>% 
  summarise(hirings = sum(hired)) %>% 
  arrange(hirings) %>% 
  filter(hirings == 0)

# the 518 taskers above have not been hired at all  
  
####  6. If we define the "Tasker conversion rate" as the number of times a Tasker has been hired, out of the number of times the Tasker has been shown, how many Taskers have a conversion rate of 100%

task %>% 
  group_by(tasker_id) %>% 
  summarise(count = n(),
            hirings = sum(hired),
            conversion_rate = hirings / count * 100) %>% 
  filter(conversion_rate == 100) %>% 
  nrow()

# 6 taskers have a conversion rate of 100%


####  7. Would it be possible for all Taskers to have a conversion rate of 100% Please explain your reasoning.

task %>% 
  group_by(recommendation_id) %>% 
  summarise(count_rec = n(),
            hirings = sum(hired),
            conversion_rate = hirings / count_rec * 100) %>% 
  arrange(desc(conversion_rate)) %>% 
  ggplot(aes(x = count_rec)) +
  geom_histogram(bins = 15)


# Only if there was one recommended tasker per recommendation and that person was hired everytime. Otherwise its not possible since only one tasker can get hired per posting 


####  8. For each category, what is the average position of the Tasker who is hired?

task %>% 
  filter(hired == 1) %>% 
  group_by(category) %>% 
  summarise(avg_position = mean(position))

# Furniture Assembly = 3.61, Mounting = 4.60, Moving Help = 4.15

  
####  9. For each category, what is the average hourly rate and average number of completed tasks for the Taskers who are hired?

task %>% 
  filter(hired == 1) %>% 
  group_by(category) %>% 
  summarise(avg_hourly_rate = mean(hourly_rate),
            avg_completed_tasks = mean(num_completed_tasks))
  
  
####  10. Based on the previous, how would you approach the question of:
  
##  How can we use market data to suggest hourly rates to Taskers that would maximize their opportunity to be hired?
  
##  Please describe in detail, with code and formulas that support your model.

# To build a a model that recommends an hourly rate first we must build a model that predicts hourly rate.  We will filter the data to only winners then use multiple regression using category, position, and number of completed tasks to predict hourly rate, then use the model output to calcualte a recommended rate

### model using data only from winners

winners <- task %>% 
  filter(hired == 1)

model_winners <- lm(hourly_rate ~ category + position + num_completed_tasks, data = task)
summary(model_winners)
tidy(model_winners)

task$recommended_rate <- predict(model_winners, newdata = task)


# This model only explains 30% of the variance in hourly rate prices so we should look to add variables to explain a larger percentage of the variance.  These might be the location, time of year, relative supply and demand in the local market, job degree of difficulty, and mining text data like user reviews for key words and sentiment.





