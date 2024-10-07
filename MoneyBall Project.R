library(ggplot2)
library(ggthemes)
library(plotly)

batting <- read.csv('Batting.csv')
#head(batting)
#summary(batting)
#str(batting)

#head(batting$AB)
#head(batting$X2B)

#Adding three more statistics that were used in Moneyball!
# 1. Batting Average (BA)
# 2. On Base Percentage (OBP)
# 3. Slugging Percentage (SLG)

# creating 1B which is not a column yet is important to calculate SLG thus
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR


batting$BA <- batting$H / batting$AB

batting$OBP <- (batting$H + batting$BB + batting$HBP) / 
  (batting$AB + batting$BB + batting$HBP + batting$SF)

batting$SLG <- (batting$X1B + (2*batting$X2B)+(3*batting$X3B)+(4*batting$HR))/
  batting$AB
#str(batting)

#to know the most undervalued players we need the Salaries data
salary <- read.csv('Salaries.csv')
#summary(salary)

#making the data in batting table with YearID starting same year as YearID in Salary to be bat

bat <- subset(batting, yearID > 1984)
#summary(bat)

#Merging the batting and salary data frame

bat_sal <- merge(batting,salary,by = c('playerID', 'yearID'))
#summary(bat_sal)



# -------  ANALYSING FOR THE LOST PLAYERS --------


lp <- c('giambja01','damonjo01','saenzol01')

lost_players <- subset(bat_sal, playerID %in% lp)
#summary(lost_players)

#Selecting the year 2001 where all the 3 players was lost
rel_year <- subset(lost_players, yearID == 2001) #rel means relevant

rel_col_lost_players <- rel_year %>% select(playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB)
#summary(rel_col_lost_players)



# -------  FINDING REPLACEMENT PLAYERS FOR THE KEY 3 PLAYERS LOST--------

#CONSTRAINT
#1. The total combined salary of the three players can not exceed 15 million dollars
#2. Their combined number of At Bats(AB) needs to be >= the lost players
#3. Their mean OBP have to be >= that of the lost players

mean_obp_lost_players <- mean(rel_col_lost_players$OBP)
mean_AB_lost_players <- mean(rel_col_lost_players$AB)

rep_player_tot <- subset(bat_sal, yearID==2001 & OBP >= mean_obp_lost_players &
                           salary <8000000 & AB >= mean_AB_lost_players)
#summary(rep_player_tot)

pplayers <- rep_player_tot[c('playerID','OBP','AB','salary')]
posplayers <- pplayers %>% arrange(desc(OBP))


##printing top 3 based on OBP data without any of the released player
possible_players <- head(posplayers %>% 
                           filter(!(playerID %in% c('giambja01','damonjo01','saenzol01'))), 3)

print(possible_players)

##TOTAL MONEY SPENT ON SALARY
#totalsalary <- head(possible_players %>%
#                      filter(!(playerID %in% c('giambja01','damonjo01','saenzol01'))), 3)
#print(sum(totalsalary$salary))

