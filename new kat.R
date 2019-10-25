

library(RJSONIO, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(rjson, warn.conflicts = FALSE)
library(RCurl, warn.conflicts = FALSE)
library(plyr, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(knitr, warn.conflicts = FALSE)


## We're also going to cheat by taking out 17-18, and 16-17 seasons just to make it easier for R

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

szndisplay <- c("1819", "1718", "1617", "1516")

getyrs <- c("2018-19", "2017-18", "2016-17", "2015-16")

grouprow <- c("2018", "2017",
              "2016", "2015")


parsebysznfKAT <- paste0("http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo",
                        "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                        "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                        "=N&PerMode=Totals&Period=0&PlayerID=1626157&PlusMinus",
                        "=N&Rank=N&Season=")

parsebysznl <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange",
                      "=&VsConference=&VsDivision=")

listofURLsKAT <- list()

for(i in 1:4){
  listofURLsKAT[[i]]<- paste0(parsebysznfKAT, getyrs[i], parsebysznl)
}



listofdfsKAT <- list()

for(i in 1:4){
  jsonconvKAT <- fromJSON(paste0(readLines(listofURLsKAT[[i]], warn = FALSE)))
  listofdfsKAT[[i]] <- as.data.frame(do.call("rbind", jsonconvKAT[["resultSets"]][[1]][["rowSet"]]))
  categories <- c(jsonconvKAT[["resultSets"]][[1]][["headers"]])
  names(listofdfsKAT[[i]]) <- c(categories)
}


for(i in seq_along(listofdfsKAT)){
  for(j in seq_along(listofdfsKAT[[1]])){
    listofdfsKAT[[i]][[j]] <-  as.character(rev(unlist(listofdfsKAT[[i]][[j]])))
  }
  assign(paste0("KATszn",i), as.data.frame(listofdfsKAT[[i]]))
}

## Tobias played for two different teams in the 17-18 seasons, detroit and LA. lets see if we 
## can ask R to find this out for us

ListofteamURLsKAT <- list()


MINTWid <- "1610612750"

parsebyteamidf <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo",
                         "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                         "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                         "=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=")
parsebyteamidm <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=")
parsebyteamidl <- paste0("&VsConference=&VsDivision=")



for(i in 1:4){
  ListofteamURLsKAT[[i]] <- paste0(parsebyteamidf, getyrs[i], parsebyteamidm, MINTWid, parsebyteamidl)
}

ListofteamdfsKAT <- list()

for(i in 1:4){
  jsonconvteamsKAT <- fromJSON(paste0(readLines(ListofteamURLsKAT[[i]], warn = FALSE)))
  ListofteamdfsKAT[[i]] <- as.data.frame(do.call("rbind", jsonconvteamsKAT[["resultSets"]][[1]][["rowSet"]]))
  categoriestm <- c(jsonconvteamsKAT[["resultSets"]][[1]][["headers"]])
  names(ListofteamdfsKAT[[i]]) <- c(categoriestm)
}


for(i in seq_along(ListofteamdfsKAT)){
  for(j in seq_along(ListofteamdfsKAT[[1]])){
    ListofteamdfsKAT[[i]][[j]] <-  as.character(rev(unlist(ListofteamdfsKAT[[i]][[j]])))
  }
  assign(paste0("KAT", i), ListofteamdfsKAT[[i]])
}


## Everything unlisted (sortable), and nicely iterated, but first we'll find the process it takes to make the full career gamelog

## (first name in caps)(season number*last season first) <- i.e. TOBY10, KOBE5 
##   -> these are the gamelogs of the teams that said player played on.
##      if we have a case like Tobias where he has multiple dataframes for
##      a single season, we cut and rbind (refer to toby code) where appropriate.
##      otherwise, we make (player_initials)gamelog(season) by merging. 





## 2015-16 szn <-- last season first** Kobe's last active season is the 15-16 season.
## Since we have 20 dataframes to merge and match, maybe it's best for us to make a loop/function...
KATmerge <- list()


## possibly the most beautiful for loop I've written thus far.
## code should work for any player who did not have a mid season trade 


for(i in 1:4){
  KATmerge[[i]] <- ListofteamdfsKAT[[i]][c(1:nrow(ListofteamdfsKAT[[i]])),c(1,5:7)]
  KATmerge[[i]]$GAME_NUMBER <- c(1:nrow(ListofteamdfsKAT[[i]]))
  KATmerge[[i]]$SEASON_DISPLAY <- as.numeric(grouprow[[i]])
  listofdfsKAT[[i]]$GAMES_PLAYED <- c(1:nrow(listofdfsKAT[[i]]))
  assign(paste0("KATgamelogz", szndisplay[[i]]), merge(listofdfsKAT[[i]], KATmerge[[i]], by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE))
}


KATcareerlog <- rbind(KATgamelogz1516, KATgamelogz1617, KATgamelogz1718, KATgamelogz1819)


KATcareerlognum <- KATcareerlog[c(1:328), c(1, 11:65, 69:70)]

for (i in seq_along(KATcareerlognum)){
  KATcareerlognum[[i]] <- as.numeric(unlist(KATcareerlognum[[i]]))
}


KAT <- group_by(KATcareerlognum, SEASON_DISPLAY)


KAT[is.na(KAT)] <- 0


yearlycomp <- plot_ly(KAT, x = ~GAME_NUMBER, frame = ~SEASON_DISPLAY) %>%
  add_lines(y = ~PTS)

yearlycomp

KAT$CAREER_GAMES <- c(1:328)

careerview <- plot_ly(KAT, x = ~CAREER_GAMES, y = ~PTS, type='scatter', mode='lines')

careerview

pts1819 <- as.numeric(as.character(unlist(KATgamelogz1819$PTS))) 
pts1718 <- as.numeric(as.character(unlist(KATgamelogz1718$PTS))) 
pts1617 <- as.numeric(as.character(unlist(KATgamelogz1617$PTS))) 
pts1516 <- as.numeric(as.character(unlist(KATgamelogz1516$PTS))) 

rebs1819 <- as.numeric(as.character(unlist(KATgamelogz1819$REB))) 
rebs1718 <- as.numeric(as.character(unlist(KATgamelogz1718$REB))) 
rebs1617 <- as.numeric(as.character(unlist(KATgamelogz1617$REB))) 
rebs1516 <- as.numeric(as.character(unlist(KATgamelogz1516$REB))) 

asts1819 <- as.numeric(as.character(unlist(KATgamelogz1819$AST))) 
asts1718 <- as.numeric(as.character(unlist(KATgamelogz1718$AST))) 
asts1617 <- as.numeric(as.character(unlist(KATgamelogz1617$AST))) 
asts1516 <- as.numeric(as.character(unlist(KATgamelogz1516$AST))) 



game_number <- c(1:82)
trialrun <- data.frame("game #" = game_number, pts1819, pts1718, pts1617, pts1516,
                       rebs1819, rebs1718, rebs1617, rebs1516, asts1819, asts1718, asts1617, asts1516)

katptyrcomp <- plot_ly(trialrun, x = ~game_number) %>%
  add_lines(y = ~pts1819, name = "points 18-19") %>%
  add_lines(y = ~pts1718, name = "points 17-18") %>%
  add_lines(y = ~pts1617, name = "points 16-17") %>%
  add_lines(y = ~pts1516, name = "points 15-16") %>%
  
  layout(
    title = "KAT pts by year",
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


katptyrcomp

katrebyrcomp <- plot_ly(trialrun, x = ~game_number) %>%
  add_lines(y = ~rebs1819, name = "rebounds 18-19") %>%
  add_lines(y = ~rebs1718, name = "rebounds 17-18") %>%
  add_lines(y = ~rebs1617, name = "rebounds 16-17") %>%
  add_lines(y = ~rebs1516, name = "rebounds 15-16") %>%
  
  layout(
    title = "KAT rebs by year",
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
    
    yaxis = list(title = "rebounds"))


katrebyrcomp

katastyrcomp <- plot_ly(trialrun, x = ~game_number) %>%
  add_lines(y = ~asts1819, name = "assists 18-19") %>%
  add_lines(y = ~asts1718, name = "assists 17-18") %>%
  add_lines(y = ~asts1617, name = "assists 16-17") %>%
  add_lines(y = ~asts1516, name = "assists 15-16") %>%
  
  layout(
    title = "KAT asts by year",
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
    
    yaxis = list(title = "assists"))


katastyrcomp
