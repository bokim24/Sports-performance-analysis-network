## don't think I actually need all these libraries, but I Can't get the script to run 
## if I don't actually have all of them and all of them in this exact order... 
## too lazy to check if this is true. If this site blows up then maybe I'll pay someone
## to look it over. Pretty sure we're only using rjson here. plotly is what we use to 
## construct the time series, but this might change in the future. Again, if this site
## blows up, then maybe I'll get someone to look at that too :)

## This is my first real exercise posting on the web. I started out writing this script
## for Lebron's stats, but noticed after awhile that if I want to compare a player's 
## performance in one year to another, then this poses an issue with games missed, and
## how we want to treat those missed games on the time series plot (do we settle with 
## 0's on those missed games? or do we piecewise the dataset? this might be explored
## further in the future). Also, datasets in both NBA.com and basketball.reference.com 
## have their own pros and cons, but mostly shortcomings. It was a decision between 
## which site gives the data that seems the least difficult to work with for my purpose,
## and that was NBA.com.

library(RJSONIO)
library(jsonlite)
library(rjson)
library(RCurl)
library(plyr)
library(plotly)

## Kat only has three seasons. Suck me if u want me to write a loop to go thru to
## find all his "active years". If I end up doing this for more players, AKA this
## site blows up, then maybe I'll pay someone to write that for me.
## You get the theme here.


## first we get the url

kat1718url <- "https://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo
=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month
=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period
=0&PlayerID=1626157&PlusMinus=N&Rank=N&Season=2017-18&SeasonSegment=&SeasonType
=Regular+Season&ShotClockRange=&VsConference=&VsDivision="

kat1617url <- "https://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo
=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month
=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period
=0&PlayerID=1626157&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType
=Regular+Season&ShotClockRange=&VsConference=&VsDivision="

kat1516url <- "https://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo
=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month
=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period
=0&PlayerID=1626157&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType
=Regular+Season&ShotClockRange=&VsConference=&VsDivision="

## next we get the tables in the url via. fromJSON and make into simple dataframe

kat1718json <- fromJSON(paste(readLines(kat1718url, warn = FALSE), collapse = ""))
kat1617json <- fromJSON(paste(readLines(kat1617url, warn = FALSE), collapse = ""))
kat1516json <- fromJSON(paste(readLines(kat1516url, warn = FALSE), collapse = ""))

df1 <- as.data.frame(do.call("rbind", kat1718json[["resultSets"]][[1]][["rowSet"]]))
df2 <- as.data.frame(do.call("rbind", kat1617json[["resultSets"]][[1]][["rowSet"]]))
df3 <- as.data.frame(do.call("rbind", kat1516json[["resultSets"]][[1]][["rowSet"]]))

firstrow1 = c(kat1718json[["resultSets"]][[1]][["headers"]])
names(df1) <- c(firstrow1)

firstrow2 = c(kat1617json[["resultSets"]][[1]][["headers"]])
names(df2) <- c(firstrow2)

firstrow3 = c(kat1516json[["resultSets"]][[1]][["headers"]])
names(df3) <- c(firstrow3)

## NBA.com data shows the last game on top, which creates somewhat of a confusion
## for R. I think R likes the first game to be at the first index, and since 
## I do as well, I'll write some code (finally) to flip the dataframe 

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

## I didn't write it actually smh... stackoverflow is a great tool xD

newdf1 <- flip(df1)
newdf2 <- flip(df2)
newdf3 <- flip(df3)

## Now we get the fantasy week and the game # for ease of access later
## yeah yeah i could easily write some code to do all this in one loop, but 
## I'm not really worried about the elegance of my code if you hadn't noticed.

dates1 <- c(newdf1$GAME_DATE)
dates2 <- c(newdf2$GAME_DATE)
dates3 <- c(newdf3$GAME_DATE)

unlisted1 <- unlist(dates1)
unlisted2 <- unlist(dates2)
unlisted3 <- unlist(dates3)

betterDates1 <- as.Date(unlisted1, format = "%Y-%m-%dT%H:%M:%S")
betterDates2 <- as.Date(unlisted2, format = "%Y-%m-%dT%H:%M:%S")
betterDates3 <- as.Date(unlisted3, format = "%Y-%m-%dT%H:%M:%S")

gameweek1 <- strftime(unlisted1, format = "%V")
gameweek2 <- strftime(unlisted2, format = "%V")
gameweek3 <- strftime(unlisted3, format = "%V")

numgameweek1 <- as.numeric(gameweek1)
fantasyweek1 <- ifelse(numgameweek1>41, numgameweek1 - 41, numgameweek1 + 11)

numgameweek2 <- as.numeric(gameweek2)
fantasyweek2 <- ifelse(numgameweek2>42, numgameweek2 - 42, numgameweek2 + 12)

numgameweek3 <- as.numeric(gameweek3)
fantasyweek3 <- ifelse(numgameweek3>43, numgameweek3 - 43, numgameweek3 + 13)

game_number <- c(1:82)

pts1718 <- as.numeric(as.character(unlist(newdf1$PTS)))  
rebs1718 <- as.numeric(as.character(unlist(newdf1$REB)))
asts1718 <- as.numeric(as.character(unlist(newdf1$AST)))
stls1718 <- as.numeric(as.character(unlist(newdf1$STL)))
blks1718 <- as.numeric(as.character(unlist(newdf1$BLK)))
TOs1718 <- as.numeric(as.character(unlist(newdf1$TOV)))
fgpct1718 <- as.numeric(as.character(unlist(newdf1$FG_PCT)))
ftpct1718 <- as.numeric(as.character(unlist(newdf1$FT_PCT)))
treys1718 <- as.numeric(as.character(unlist(newdf1$FG3M)))

pts1617 <- as.numeric(as.character(unlist(newdf2$PTS)))
rebs1617 <- as.numeric(as.character(unlist(newdf2$REB)))
asts1617 <- as.numeric(as.character(unlist(newdf2$AST)))
stls1617 <- as.numeric(as.character(unlist(newdf2$STL)))
blks1617 <- as.numeric(as.character(unlist(newdf2$BLK)))
TOs1617 <- as.numeric(as.character(unlist(newdf2$TOV)))
fgpct1617 <- as.numeric(as.character(unlist(newdf2$FG_PCT)))
ftpct1617 <- as.numeric(as.character(unlist(newdf2$FT_PCT)))
treys1617 <- as.numeric(as.character(unlist(newdf2$FG3M)))

pts1516 <- as.numeric(as.character(unlist(newdf3$PTS)))
rebs1516 <- as.numeric(as.character(unlist(newdf3$REB)))
asts1516 <- as.numeric(as.character(unlist(newdf3$AST)))
stls1516 <- as.numeric(as.character(unlist(newdf3$STL)))
blks1516 <- as.numeric(as.character(unlist(newdf3$BLK)))
TOs1516 <- as.numeric(as.character(unlist(newdf3$TOV)))
fgpct1516 <- as.numeric(as.character(unlist(newdf3$FG_PCT)))
ftpct1516 <- as.numeric(as.character(unlist(newdf3$FT_PCT)))
treys1516 <- as.numeric(as.character(unlist(newdf3$FG3M)))

## trial run because this is my first time doing compiling different years together,
## aka I don't actually know if it'll work.

trialrun <- data.frame("game #" = game_number, pts1718, pts1617, pts1516,
                       rebs1718, rebs1617, rebs1516, asts1718, asts1617, asts1516)

fantasydf1718 <- data.frame("date" = betterDates1, game_number, fantasyweek1,
                            fgpct1718, ftpct1718, treys1718, pts1718, rebs1718,
                            asts1718, stls1718, blks1718, TOs1718)

fantasydf1617 <- data.frame("date" = betterDates2, game_number, fantasyweek2,
                            fgpct1617, ftpct1617, treys1617, pts1617, rebs1617,
                            asts1617, stls1617, blks1617, TOs1617)

fantasydf1516 <- data.frame("date" = betterDates3, game_number, fantasyweek3,
                            fgpct1516, ftpct1516, treys1516, pts1516, rebs1516,
                            asts1516, stls1516, blks1516, TOs1516)


## now to get avg stats by fantasy week

savefreq1 <- count(fantasydf1718, "fantasyweek1")
aggdata1 <-aggregate(fantasydf1718, by=list(fantasyweek1), FUN=mean, na.rm=TRUE)
aggdata1$number_of_games <- savefreq1$freq
aggdata1$betterDates <- NULL
aggdata1$game_number <- NULL
aggdata1$Group.1 <- NULL

savefreq2 <- count(fantasydf1718, "fantasyweek2")
aggdata2 <-aggregate(fantasydf1718, by=list(fantasyweek2), FUN=mean, na.rm=TRUE)
aggdata2$number_of_games <- savefreq2$freq
aggdata2$betterDates <- NULL
aggdata2$game_number <- NULL
aggdata2$Group.1 <- NULL

savefreq3 <- count(fantasydf1718, "fantasyweek3")
aggdata3 <-aggregate(fantasydf1718, by=list(fantasyweek3), FUN=mean, na.rm=TRUE)
aggdata3$number_of_games <- savefreq3$freq
aggdata3$betterDates <- NULL
aggdata3$game_number <- NULL
aggdata3$Group.1 <- NULL

katdata <- plot_ly(trialrun, x = ~game_number) %>%
  add_lines(y = ~pts1718, name = "points1718") %>%
  add_lines(y = ~rebs1718, name = "rebs1718") %>%
  add_lines(y = ~asts1718, name = "asts1718") %>%
  
  layout(
    title = "BitKat",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 2,
            label = "2 wk",
            step = "week",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 2,
            label = "2 mo",
            step = "month",
            stepmode = "backward"),
          list(step = "all"))),
      
      rangeslider = list(type = "game #")),
    
    yaxis = list(title = "Points"))

katdata


katdata2 <- plot_ly(trialrun, x = ~game_number, width=1600, height=1000) %>%
  add_lines(y = ~pts1718, name = "points1718") %>%
  add_lines(y = ~pts1617, name = "points1617") %>%
  add_lines(y = ~pts1516, name = "points1516") %>%
  
  layout(
    title = "BitKat",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 2,
            label = "2 wk",
            step = "week",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 2,
            label = "2 mo",
            step = "month",
            stepmode = "backward"),
          list(step = "all"))),
      
      rangeslider = list(type = "game #")),
    
    yaxis = list(title = "Points"))

katdata2
