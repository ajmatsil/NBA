# NBA
Scraping NBA data and making models from it.
---
title: "ScrapeNBA"
author: "Adam Matsil"
date: "December 20, 2015"
output: html_document
---

Create data frame for games results up through 12/18.
```{r}
# install.packages("jsonlite")
library(jsonlite)

months <- c('10','11','12')
days <- 1:31
full_scores <- as.data.frame(c())

for (i in 1:3)
  {
  for (j in 1:31)
    {
    if ((months[i]=='11' & days[j]==31) | months[i]=='10' & days[j]<27)
      {
        1
      }
    else
      {
        url <- paste('http://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=',months[i],'%2F',paste(days[j]),'%2F2015',sep = '')
        games <- fromJSON(txt=url)
        scores <- as.data.frame(games$resultSets$rowSet[2],stringsAsFactors = FALSE)
        if (length(scores)==0)
          {
            1
          }
        else
          {
            full_scores <- rbind(full_scores,scores)
          } 
        }
      }
  }

names(full_scores) <- c("GAME_DATE_EST","GAME_SEQUENCE","GAME_ID","TEAM_ID","TEAM_ABBREVIATION","TEAM_CITY_NAME","TEAM_WINS_LOSSES","PTS_QTR1","PTS_QTR2","PTS_QTR3","PTS_QTR4","PTS_OT1","PTS_OT2","PTS_OT3","PTS_OT4","PTS_OT5","PTS_OT6","PTS_OT7","PTS_OT8","PTS_OT9","PTS_OT10","PTS","FG_PCT","FT_PCT","FG3_PCT","AST","REB","TOV")

full_scores <- na.omit(full_scores) # Remove NAs

write.csv(full_scores,file="nba_scores_2015_12_18.csv") # Save as CSV for faster loading
```

Create data frame that has how many points score a team has in every game so far (through 12/18).
```{r}
full_scores <- read.csv("nba_scores_2015_12_18.csv") # Read CSV

# Create smaller data frame
full_scores_short <- as.data.frame(cbind(full_scores$GAME_ID,full_scores$TEAM_ID,full_scores$PTS),stringsAsFactors = FALSE)
names(full_scores_short) <- c("GAME_ID","TEAM_ID","PTS")

# Convert PTS and TEAM_ID to integers
full_scores_short$PTS <- as.integer(full_scores_short$PTS)
full_scores_short$TEAM_ID <- as.integer(full_scores_short$TEAM_ID)

# Identify if team is home or away during the game they're playing in.  Append to full_scores_short
ht <- rep(c(0,1),length(full_scores_short$PTS)/2)
full_scores_short <- cbind(full_scores_short,ht)
```

Let's now get the opponent for each team.
```{r}
# Join full_scores_short onto itself
scores <- join(full_scores_short,full_scores_short,type = 'left',by = 'GAME_ID')
names(scores)<-c('GAME_ID','TEAM_ID','PTS','ht','TEAM_ID_OPP','PTS_OPP','ht_OPP')

# There are now duplicate rows - select rows where TEAM_ID does not equal TEAM_ID_OPP
scores <- subset(scores,scores$TEAM_ID != scores$TEAM_ID_OPP)

# Create data frame that's essentially the same as full_scores_short with an added column for the opponent's TEAM_ID
scores <- as.data.frame(cbind(scores$GAME_ID,scores$TEAM_ID,scores$PTS,scores$ht,scores$TEAM_ID_OPP),stringsAsFactors = FALSE)
names(scores)<-c('GAME_ID','TEAM_ID','PTS','ht','TEAM_ID_OPP')
```

Get defensive team overall stats.
```{r}
# URL for overall defensive team stats
url <- 'http://stats.nba.com/stats/leaguedashptteamdefend?Conference=&DateFrom=&DateTo=&DefenseCategory=Overall&Division=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision='

# Pull data and create data frame for it
def_raw <- fromJSON(txt=url)
def <- as.data.frame(def_raw$resultSets$rowSet,stringsAsFactors = FALSE)
names(def) <- c("TEAM_ID","TEAM_NAME","TEAM_ABBREVIATION","GP","G","FREQ","D_FGM","D_FGA","D_FG_PCT","NORMAL_FG_PCT","PCT_PLUSMINUS")

# Pull out defensive team stats
def_short <- as.data.frame(cbind(def$TEAM_ID,def$D_FGM,def$D_FGA,def$D_FG_PCT,def$NORMAL_FG_PCT,def$PCT_PLUSMINUS),stringsAsFactors = FALSE)
names(def_short) <- c("TEAM_ID_OPP","D_FGM","D_FGA","D_FG_PCT","NORMAL_FG_PCT","PCT_PLUSMINUS")

# Convert strings to numeric types
def_short$TEAM_ID <- as.integer(def_short$TEAM_ID)
def_short$D_FGM <- as.numeric(def_short$D_FGM)
def_short$D_FGA <- as.numeric(def_short$D_FGA)
def_short$D_FG_PCT <- as.numeric(def_short$D_FG_PCT)
def_short$NORMAL_FG_PCT <- as.numeric(def_short$NORMAL_FG_PCT)
def_short$PCT_PLUSMINUS <- as.numeric(def_short$PCT_PLUSMINUS)
```

Let's now join a team's defensive data onto the scores data frame.
```{r}
obs <- join(scores,def_short,type = 'left',by = 'TEAM_ID_OPP')
```

Let's now create a linear model.
```{r}
set.seed(15)
observations <- length(obs$PTS) # Find how many observations are in the data set.
train <- sample(observations,round(observations/10)*7) # Create a vector of indexes for the training set.

train_lm <- lm(PTS~factor(TEAM_ID)+ht+D_FGM+D_FGA+D_FG_PCT+NORMAL_FG_PCT+PCT_PLUSMINUS,obs,subset = train)
summary(train_lm)
```

Let's try Poisson.
```{r}
obs$TEAM_ID <- as.integer(obs$TEAM_ID)
obs$PTS <- as.integer(obs$PTS)
obs$ht <- as.integer(obs$ht)
obs$D_FGM<- as.numeric(obs$D_FGM)
obs$D_FGA <- as.numeric(obs$D_FGA)
obs$D_FG_PCT <- as.numeric(obs$D_FG_PCT)
obs$PCT_PLUSMINUS <- as.numeric(obs$PCT_PLUSMINUS)

train_pois <- glm(PTS~factor(TEAM_ID)+ht+D_FGM+D_FGA+D_FG_PCT+NORMAL_FG_PCT+PCT_PLUSMINUS,data=obs,subset = train,family = 'poisson')
summary(train_pois)
```

Test against 12/19:
```{r}
url <- 'http://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=12%2F19%2F2015'

yest <- fromJSON(txt=url)
yest_scores <- as.data.frame(yest$resultSets$rowSet[2],stringsAsFactors = FALSE)
names(yest_scores) <- c("GAME_DATE_EST","GAME_SEQUENCE","GAME_ID","TEAM_ID","TEAM_ABBREVIATION","TEAM_CITY_NAME","TEAM_WINS_LOSSES","PTS_QTR1","PTS_QTR2","PTS_QTR3","PTS_QTR4","PTS_OT1","PTS_OT2","PTS_OT3","PTS_OT4","PTS_OT5","PTS_OT6","PTS_OT7","PTS_OT8","PTS_OT9","PTS_OT10","PTS","FG_PCT","FT_PCT","FG3_PCT","AST","REB","TOV")

# Create smaller data frame
yest_scores_short <- as.data.frame(cbind(yest_scores$GAME_ID,yest_scores$TEAM_ID,yest_scores$PTS),stringsAsFactors = FALSE)
names(yest_scores_short) <- c("GAME_ID","TEAM_ID","PTS")

# Convert PTS and TEAM_ID to integers
yest_scores_short$PTS <- as.integer(yest_scores_short$PTS)
yest_scores_short$TEAM_ID <- as.integer(yest_scores_short$TEAM_ID)

# Identify if team is home or away during the game they're playing in.  Append to full_scores_short
ht <- rep(c(0,1),length(yest_scores_short$PTS)/2)
yest_scores_short <- cbind(yest_scores_short,ht)

# Join full_scores_short onto itself
trial <- join(yest_scores_short,yest_scores_short,type = 'left',by = 'GAME_ID')
names(trial)<-c('GAME_ID','TEAM_ID','PTS','ht','TEAM_ID_OPP','PTS_OPP','ht_OPP')

# There are now duplicate rows - select rows where TEAM_ID does not equal TEAM_ID_OPP
trial <- subset(trial,trial$TEAM_ID != trial$TEAM_ID_OPP)

# Create data frame that's essentially the same as full_scores_short with an added column for the opponent's TEAM_ID
trial <- as.data.frame(cbind(trial$GAME_ID,trial$TEAM_ID,trial$PTS,trial$ht,trial$TEAM_ID_OPP),stringsAsFactors = FALSE)
names(trial)<-c('GAME_ID','TEAM_ID','PTS','ht','TEAM_ID_OPP')

trial <- join(trial,def_short,type = 'left',by = 'TEAM_ID_OPP')

yest_data <- join(yest_scores_short,yest_scores_short,type = 'left',by = 'GAME_ID')
names(yest_data)<-c('GAME_ID','TEAM_ID','PTS','ht','TEAM_ID_OPP','PTS_OPP','ht_OPP')
yest_data <- subset(yest_data,yest_data$TEAM_ID != yest_data$TEAM_ID_OPP & yest_data$ht==0)

actual_results = c()

for (i in 1:length(yest_data$PTS))
  {
  if (yest_data$PTS[i]>yest_data$PTS_OPP[i])
    {
    actual_results[i] = 0
    }
  else
    {
     actual_results[i] = 1 
    }
  }

trial$TEAM_ID <- as.integer(trial$TEAM_ID)
trial$PTS <- as.integer(trial$PTS)
trial$ht <- as.integer(trial$ht)
trial$D_FGM<- as.numeric(trial$D_FGM)
trial$D_FGA <- as.numeric(trial$D_FGA)
trial$D_FG_PCT <- as.numeric(trial$D_FG_PCT)
trial$PCT_PLUSMINUS <- as.numeric(trial$PCT_PLUSMINUS)

preds_a = predict.glm(train_pois,subset(trial,trial$ht==0),type='response')
preds_h = predict.glm(train_pois,subset(trial,trial$ht==1),type='response')

pred_results = c()
for (i in 1:length(yest_data$PTS))
  {
  if (preds_a[i]>preds_h[i])
    {
    pred_results[i] = 0
    }
  else
    {
     pred_results[i] = 1 
    }
  }

yest_data <- cbind(yest_data,preds_a,preds_h,pred_results)

correct <- c()
for (i in 1:length(yest_data$PTS))
  {
  if (yest_data$actual_results[i]==yest_data$pred_results[i])
    {
    correct[i] = 1
    }
  else
    {
     correct[i] = 0 
    }
  }
```
