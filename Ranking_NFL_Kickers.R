library(tidyverse)
library(Rcmdr)
library(broom)
library(scales)
library(stringr)


kick <- read_csv("https://raw.githubusercontent.com/jaywhitmire/myrepo/master/FGAttemps.csv")

names(kick)
head(kick)
str(kick)
summary(kick)


unique(kick$StadiumName)
table(kick$StadiumName)
kick %>% 
  group_by(StadiumName) %>% 
  summarize(Kicks = n(),
            FGpercent = mean(IsScoringPlay)) %>% 
  arrange((FGpercent))
#70 unique stadiums including NA

unique(kick$KickerName)
kick %>% 
  group_by(KickerName) %>% 
  summarize(Kicks = n(),
            FGpercent = mean(IsScoringPlay)) %>% 
  arrange(desc(Kicks))
#114 kickers

unique(kick$StadiumTurf)
# 30 unique turfs

unique(kick$StadiumType)
unique(kick$GameWeather)
unique(kick$WindSpeed)
unique(kick$KickerName)
unique(kick$PlayDesc)



#Histogram of Kicks from each yardline

ggplot(kick, aes(x=AbsoluteYardLine)) +
  geom_histogram(bins = 50) + 
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70))

ggplot(kick, aes(y=AbsoluteYardLine, col = factor(IsScoringPlay))) +
  geom_boxplot() + 
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  coord_flip()

ggplot(kick, aes(x=AbsoluteYardLine, col = factor(IsScoringPlay))) +
  geom_density() + 
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70))

# the Large majority of our data comes on kicks inside the 40 yardline, these are really 57 yard kicks and are usually only attempted at end of half or end of game situations but are incredibly important because the kicker is deciding the game.


## Plotting FG Pct by Yardline
kick <- kick %>% 
  mutate(Blocked = str_detect(tolower(kick$PlayDesc), pattern = "blocked"))

yardlinestats <- kick %>% 
  group_by(AbsoluteYardLine) %>% 
  summarize(Kicks = n(),
            FGpercent = mean(IsScoringPlay),
            FGpctNotBlocked = mean(ifelse(Blocked == FALSE, IsScoringPlay, NA), na.rm = TRUE),
            PctBlocked = mean(Blocked))

ggplot(yardlinestats, aes(x=AbsoluteYardLine, y = FGpercent)) +
  geom_line()

ggplot(yardlinestats, aes(x=AbsoluteYardLine, y = FGpctNotBlocked)) +
  geom_line()

#FG pct drops below 50% around the 40 yardline

ggplot(yardlinestats, aes(x=AbsoluteYardLine, y = PctBlocked)) +
  geom_line()
# based on my intuition and this graph, at 40 yards blocks become the kickers falt for having a low trajectory on long kicks


####  Building a model of kick difficulty

## Grass Vs Turf

unique(kick$StadiumTurf)

kick <- kick %>% 
  mutate(Surface = ifelse(str_detect(StadiumTurf, pattern = "Turf"), "Turf", "Grass"))

table(kick$StadiumTurf, kick$Surface)

## Game Weather,  Wet vs Dry

unique(kick$GameWeather)


kick <- kick %>% 
  mutate(Wet = ifelse(str_detect(tolower(kick$GameWeather), pattern = paste(c("storm", "snow", "shower", "rain", "flur"),collapse = '|')), "Wet", "Dry")) %>% 
  mutate(Wet = ifelse(str_detect(tolower(kick$GameWeather), pattern = "^0%"), "Dry", kick$Wet))

###  "0% chance of rain.", we look for rain then get rid of it if it begins with 0%

kick$Wet[is.na(kick$Wet)] <- "Unknown"

unique(kick$Wet)

table(kick$Wet)

table(kick$GameWeather, kick$Wet)

# Imputing NAs with Medians

any(is.na(kick$AbsoluteYardLine))

any(is.na(kick$Temperature))


medianTemp <- median(kick$Temperature, na.rm =TRUE)
naTemp <- is.na(kick$Temperature)

kick$Temperature[naTemp] <- medianTemp

any(is.na(kick$Temperature))

any(is.na(kick$Humidity))


medianHum <- median(kick$Humidity, na.rm =TRUE)
naHum <- is.na(kick$Humidity)

kick$Humidity[naHum] <- medianHum

any(is.na(kick$Temperature))


any(is.na(kick$Surface))
any(is.na(kick$OutdoorStadium))

names(kick)


### Our model of kick difficulty takes AbsoluteYardLine, Wet, Surface, OutdoorStadium, Temperature, and Humidity into account and creates a 0 to 1 prediction.  the lower the prediction, the more difficult the attempt, and the higher the prediction, the more difficult the attempt.  We did not use wind as a variable because we cannot know whether the kick was wind assisted or against the wind.  


model <- glm(IsScoringPlay ~ AbsoluteYardLine + Wet + Surface + OutdoorStadium + Temperature + Humidity,  data = kick, family = "binomial")

summary(model)

kick$Difficulty <- model$fitted.values


###  To evaluate which kickers are the best we want to see how they did relative to the difficulty of their kicks attempted.  We will calculate the points gained or lost as whether they made it or not (1 or 0) minus the difficulty of the kick.  Making a difficult kick results in a huge point gained while missing a gimme results in a large points lost.  

kick <- kick %>% 
  mutate(Points_Gained = IsScoringPlay - Difficulty)

kicker_data <- kick %>% 
  group_by(KickerName) %>% 
  summarise(Kicks = n(), 
            FGpercent = mean(IsScoringPlay),
            FGpctNotBlocked = mean(ifelse(Blocked == FALSE, IsScoringPlay, NA), na.rm = TRUE),
            PctBlocked = mean(Blocked),
            Total_Points_Gained = sum(Points_Gained),
            Avg_Points_Gained = mean(Points_Gained),
            StDev_Points_Gained = sd(Points_Gained),
            Avg_Distance = mean(AbsoluteYardLine),
            StDev_Distance = sd(AbsoluteYardLine),
            Long = max(kick$AbsoluteYardLine[kick$IsScoringPlay == 1 & kick$KickerName == KickerName]))


kicker_data_Long <- kick %>% 
  filter(AbsoluteYardLine > 33) %>% 
  group_by(KickerName) %>% 
  summarise(Kicks = n(), 
            FGpercent = mean(IsScoringPlay),
            FGpctNotBlocked = mean(ifelse(Blocked == FALSE, IsScoringPlay, NA), na.rm = TRUE),
            PctBlocked = mean(Blocked),
            Total_Points_Gained = sum(Points_Gained),
            Avg_Points_Gained = mean(Points_Gained),
            StDev_Points_Gained = sd(Points_Gained),
            Avg_Distance = mean(AbsoluteYardLine),
            StDev_Distance = sd(AbsoluteYardLine),
            Long = max(kick$AbsoluteYardLine[kick$IsScoringPlay == 1 & kick$KickerName == KickerName])) %>% 
  filter(Kicks > 4)



### to be considered as a kicker today, the player must have at least 10 attempts in since 2016 to ensure we are not getting ranking retired players.  Playerts that do not meet this criterion will not be ranked.

kicker_data_recent  <- kick %>% 
  filter(Season > 2016) %>% 
  group_by(KickerName) %>% 
  summarise(Kicks = n(), 
            FGpercent = mean(IsScoringPlay),
            FGpctNotBlocked = mean(ifelse(Blocked == FALSE, IsScoringPlay, NA), na.rm = TRUE),
            PctBlocked = mean(Blocked),
            Total_Points_Gained = sum(Points_Gained),
            Avg_Points_Gained = mean(Points_Gained),
            StDev_Points_Gained = sd(Points_Gained),
            Avg_Distance = mean(AbsoluteYardLine),
            StDev_Distance = sd(AbsoluteYardLine),
            Long = max(kick$AbsoluteYardLine[kick$IsScoringPlay == 1 & kick$KickerName == KickerName])) %>% 
  filter(Kicks > 10)

kicker_data_recent %>% 
  ggplot(aes(x = Kicks, y = Avg_Points_Gained)) +
  geom_text(aes(label = KickerName))


#To penalize inconsistent kickers we will subtract the StDev from the mean of Points Gained

kickers_ranked <- kicker_data_recent %>% 
  mutate(rank_objective = Avg_Points_Gained - StDev_Points_Gained) %>% 
  mutate(Rank = rank(desc(rank_objective))) %>% 
  arrange(Rank)

kickers_ranked <- kickers_ranked %>% 
  mutate(Rating = ((rank_objective-min(rank_objective))/(max(rank_objective)-min(rank_objective)))*10) %>% 
  mutate(Rating = round(Rating, digits = 1))

kicker_csv <- kickers_ranked %>% 
  select(playerID = KickerName, Whitmire_Rank = Rank, Whitmire_Rating = Rating)

kickers_old <- kicker_data %>% 
  filter(!KickerName %in% as.vector(unique(kicker_csv$playerID))) %>% 
  select(playerID = KickerName) %>% 
  mutate(Whitmire_Rank = 42,
         Whitmire_Rating = 0)

kicker_csv <- bind_rows(kicker_csv, kickers_old)

write.csv(kicker_csv, file = "Whitmire_Kickers.csv")




###  Looking at the Three Specified Kickers

selected_kickers <- kickers_ranked %>% 
  filter(KickerName %in% c("Myles Daniell", "Garrett Levron", "Jacques Sorge"))

selected_kickers <- selected_kickers %>% 
  select(KickerName, Rank, Rating, Kicks, "FG%" = FGpercent, Long, Avg_Distance, Avg_Points_Gained)

selected_kickers_years <- kick %>% 
  filter(KickerName %in% c("Myles Daniell", "Garrett Levron", "Jacques Sorge")) %>% 
  group_by(KickerName, Season) %>% 
  summarise(Kicks = n(), 
            FGpercent = mean(IsScoringPlay),
            FGpctNotBlocked = mean(ifelse(Blocked == FALSE, IsScoringPlay, NA), na.rm = TRUE),
            PctBlocked = mean(Blocked),
            Total_Points_Gained = sum(Points_Gained),
            Avg_Points_Gained = mean(Points_Gained),
            StDev_Points_Gained = sd(Points_Gained),
            Avg_Distance = mean(AbsoluteYardLine),
            StDev_Distance = sd(AbsoluteYardLine),
            Long = max(kick$AbsoluteYardLine[kick$IsScoringPlay == 1 & kick$KickerName == KickerName]))

kick %>% 
  filter(Season > 2016) %>% 
  group_by(KickerName, AbsoluteYardLine) %>% 
  summarise(FGPct = mean(IsScoringPlay)) %>% 
  filter(KickerName %in% c("Myles Daniell", "Garrett Levron", "Jacques Sorge")) %>% 
  ggplot(aes(x = AbsoluteYardLine, y = FGPct, col = KickerName)) +
  geom_point() +
  geom_line()

