library(dplyr);
library(ggplot2);

NFLGameData <- read.csv("https://raw.githubusercontent.com/UTSportsAnalytics/data/main/NFLGameData.csv");
# Let's just work with the 2021 season
NFLGameData <- NFLGameData %>% 
  filter(season==2021);


# 1. Find mean home and away points scored for each QB 
HomeQBPoints <- NFLGameData %>% 
  group_by(home_qb_name) %>% 
  summarise(meanHomeQBPoints = mean(home_score))  %>% 
  arrange(desc(meanHomeQBPoints));

# rename home_qb_name to QB
HomeQBPoints <- HomeQBPoints %>% 
  rename(QB = home_qb_name);


AwayQBPoints <- NFLGameData %>% 
  group_by(away_qb_name) %>% 
  summarise(meanAwayQBPoints = mean(away_score))  %>% 
  arrange(desc(meanAwayQBPoints));

# rename away_qb_name to QB
AwayQBPoints <- AwayQBPoints %>% 
  rename(QB = away_qb_name);

# Join HomeQBPoints and AwayQBPoints by QB
QBPoints <- HomeQBPoints %>% 
  full_join(AwayQBPoints, by = "QB");

print(QBPoints, n = Inf);

# Plot looks ugly, needs work
# Plot QBpoints as a bar chart where x is QB and y is mean points with one bar for home and one for away
# ggp <- ggplot(QBPoints, aes(x = QB, y = meanHomeQBPoints, fill = "Home")) + 
#   geom_bar(stat = "identity", position = "dodge") + 
#   geom_bar(aes(y = meanAwayQBPoints, fill = "Away"), stat = "identity", position = "dodge") + 
#   labs(title = "Mean Home and Away Points Scored by QB", x = "QB", y = "Mean Points") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# # increase the x axis tick spacing
# ggp <- ggp + scale_x_discrete(limits = QBPoints$QB, expand = c(0, 1))

# print(ggp);


#2: Which QB played the most overtime games from 2015 to 2022?
OTData <- read.csv("https://raw.githubusercontent.com/UTSportsAnalytics/data/main/NFLGameData.csv");
OTData <- OTData %>% 
  filter(season >= 2015 & overtime == 1);

# group by QB and count the number of games
HomeData <- OTData %>% 
  group_by(home_qb_name) %>% 
  summarise(OTGames = n()) %>% 
  arrange(desc(OTGames));

# rename home_qb_name to QB
HomeData <- HomeData %>% 
  rename(QB = home_qb_name);

# Away QBs
AwayData <- OTData %>% 
  group_by(away_qb_name) %>% 
  summarise(OTGames = n()) %>% 
  arrange(desc(OTGames));

# rename home_qb_name to QB
AwayData <- AwayData %>% 
  rename(QB = away_qb_name);

# join the two data frames by QB and add the OTGames columns
OTData <- HomeData %>% 
  full_join(AwayData, by = "QB")  %>% 
  # create OT Games column that is sum of OTGames.x and OTGames.y, account for NA
    mutate(OTGames = ifelse(is.na(OTGames.x), OTGames.y, ifelse(is.na(OTGames.y), OTGames.x, OTGames.x + OTGames.y))) %>%
    # remove the OTGames.x and OTGames.y columns
    select(-OTGames.x, -OTGames.y) %>% 
    # sort by OTGames
    arrange(desc(OTGames));
  

print(OTData, n = Inf);

# Kirk Cousins played the most overtime games from 2015 to 2022 with a total of 9 overtime games


