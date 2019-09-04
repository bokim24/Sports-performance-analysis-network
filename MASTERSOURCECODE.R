
library(RJSONIO)
library(jsonlite)
library(rjson)
library(RCurl)
library(plyr)
library(plotly)
library(rvest)

player_info <- read_html(paste0(parsebysznf, getyrs[1], parsebysznl))


player_info




rturl <- paste0("http://www.rottentomatoes.com/browse/in-theaters")

table <- read_html(rturl)

table <- as.data.frame(table)


rtjson <- fromJSON(paste(readLines(rturl, warn = FALSE), collapse = ""))

rtdf <- as.data.frame(do.call("rbind", rtjson))


parsebysznf <- paste0("https://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo",
                     "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                     "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                     "=N&PerMode=Totals&Period=0&PlayerID=2544&PlusMinus",
                     "=N&Rank=N&Season=")

parsebysznl <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange",
                     "=&VsConference=&VsDivision=")


listofdfs <- list()

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}
  


gettingplayerids <- paste0("http://www.nba.com/players/active_players.json")

playeridsjson <- fromJSON(paste(readLines(gettingplayerids, warn = FALSE), collapse = ""))

playerinfodf <- as.data.frame(do.call("rbind", playeridsjson))



szndisplay <- c("1718", "1617", "1516", "1415", "1314",
                "1213", "1112", "1011", "0910", "0809",
                "0708", "0607", "0506", "0405", "0304",
                "0203", "0102", "0001", "9900", "9899",
                "9798", "9697")


getyrs <- c("2017-18", "2016-17", "2015-16", "2014-15", "2013-14",
            "2012-13", "2011-12", "2010-11", "2009-10", "2008-09",
            "2007-08", "2006-07", "2005-06", "2004-05", "2003-04",
            "2002-03", "2001-02", "2000-01", "1999-00", "1998-99",
            "1997-98", "1996-97")

## for now, swap out the id of desired player 
## i.g. lebron's player id is 2544 -> "playerID=2544" 

parsebysznf <- paste0("http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo",
                      "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                      "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                      "=N&PerMode=Totals&Period=0&PlayerID=2544&PlusMinus",
                      "=N&Rank=N&Season=")

parsebysznl <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange",
                      "=&VsConference=&VsDivision=")


listofdfs <- list()

## so this beautiful loop goes thru each of the urls iterated by getyrs starting from the 
## latest season to hopefully allow for early break if possible. I haven't played around 
## with figuring out how to make it break once it sees a consecutive missed season so
## the page still might take awhile to load

for(i in seq_along(getyrs)){
  jsonconv <- fromJSON(paste(readLines(paste0(parsebysznf, getyrs[i], parsebysznl), warn = FALSE), collapse = ""))
  if(!length(jsonconv[["resultSets"]][[1]][["rowSet"]]) == 0){
    listofdfs[[i]] <- as.data.frame(do.call("rbind", jsonconv[["resultSets"]][[1]][["rowSet"]]))
    categories <- c(jsonconv[["resultSets"]][[1]][["headers"]])
    names(listofdfs[[i]]) <- c(categories)
        
  }
}

## saving each season as separate dataframe. Note the use of the flip function here

  
flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}


for(i in seq_along(listofdfs)){
  assign(paste0("leb", szndisplay[i]), flip(listofdfs[[i]]))
}

## so right now each of the columns for each of the data frames we just created for each of his seasons (wow)
## are saved as a list, which I guess means that it's "unsortable" leading to problems later when we try to parse out 
## missed games or make graphs even. If you guys noticed in the code for KAT's page, you would have seen that I have
## multiple copy/paste commands to unlist each column and make a new dataframe. This time I'm gonna try to do that with lebron
## using loops and everything I've learned so far. I'm such a good student


for(i in seq_along(listofdfs)){
  for(j in seq_along(listofdfs[[1]])){
    listofdfs[[i]][[j]] <-  as.character(unlist(listofdfs[[i]][[j]]))
  }
}

## That was so easy. one line of command inside a nested for loop. Took me like three days to figure it out oof...

## So now (this part is semi "optional" but I just tried it out and the NA spots in the graphs conveniently
## show piecewise graphs, so we don't have to make a matrix-like dataframe for each season, i can just leave them 
## once i match game numbers to respective games of the season for that particular team), we have to make sure that 
## if lebron's 74th game played in the season, for example, is cleveland's 80th game in regular season (meaning, that 
## lebron missed 6 games in the season prior to that game), we have to match cleveland's game schedule to lebron's.

## this is necessary because we're cleaning up NBA.com advanced stats json data. If we were to use basketball.reference, 
## I would guess that we have to mess around with excel or notesheet, I honestly have no clue, since you can print out 
## csvs for each player's schedule, and they DO take account missed games. BUT, I noticed that when I tried copying out
## basketball.reference url to R, then the table gets all funky from duplicated column names that they use every time a 
## page breaks (I don't want to go into this, but if you plan on using data from basketball.reference, just know that
## you're going to have a whole separate list of issues to work out before that data becomes fully 'usable').

## since we already conveniently figured out which seasons we have NBA data for lebron, we're going to use that to 
## get schedules for those seasons from lebron's particular team. 

## actually, now I want to make a for loop to make game_ID dataframes for any player i want. This is also prolly gonna
## take awhile...

## we aren't dealing with mid season trades with any of our players YET. so we don't have the issue of having more than one team_ID
## in a season for a single player(if we have more than one team_IDs for a player, that means that the player was traded mid season)
## so I don't wanna go into that yet.

## lets make team game_ID dataframes in a list and unlist them so that we can merge later on

## nvm I wanna figure it out for everyone, so i can have a very general formula. Here comes Isaiah Thomas the big little man.
## oops, nvm forgot he got traded b4 season.

## Since I'm trying to apply for this job for the clippers, i'll use Tobias, whose player ID = 202699



gettingplayerids <- paste0("http://www.nba.com/players/active_players.json")

playeridsjson <- fromJSON(paste(readLines(gettingplayerids, warn = FALSE), collapse = ""))

playerinfodf <- as.data.frame(do.call("rbind", playeridsjson))

## scroll down to T -> his player ID is 202738

parsebysznTHf <- paste0("http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo",
                      "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                      "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                      "=N&PerMode=Totals&Period=0&PlayerID=202699&PlusMinus",
                      "=N&Rank=N&Season=")


listofdfsTH <- list()


for(i in length(getyrs)){
  jsonconvTH <- fromJSON(paste(readLines(paste0(parsebysznTHf, getyrs[i], parsebysznl), warn = FALSE), collapse = ""))
  if(!length(jsonconvTH[["resultSets"]][[1]][["rowSet"]]) == 0){
    listofdfsTH[[i]] <- as.data.frame(do.call("rbind", jsonconvTH[["resultSets"]][[1]][["rowSet"]]))
    categories <- c(jsonconvTH[["resultSets"]][[1]][["headers"]])
    names(listofdfsTH[[i]]) <- c(categories)
    
  }
}
## double pat on the back for me, the for loop works for I.T. so far, showing 7 seasons played.
## with unlisted values, we can just use rev() instead of our flip function


for(i in seq_along(listofdfsTH)){
  for(j in seq_along(listofdfsTH[[1]])){
    listofdfsTH[[i]][[j]] <-  as.character(rev(unlist(listofdfsTH[[i]][[j]])))
  }
}

## Tobias played for two different teams in the 17-18 seasons, detroit and LA. lets see if we 
## can ask R to find this out for us

teamsznsdisplay <- list()

for(i in seq_along(listofdfsTH)){
  teamids <- unique(listofdfsTH[[i]][[4]])
  teamnames <- unique(listofdfsTH[[i]][[5]])
  parsebyteamidf <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo",
                           "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                           "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                           "=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=")
  parsebyteamidm <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=")
  parsebyteamidl <- paste0("&VsConference=&VsDivision=")
  if(!length(unique(teamids)) == 1){
    for(j in seq_along(teamids)){
      jsonconvteamsTH <- fromJSON(paste0(readLines(paste0(parsebyteamidf, getyrs[i], parsebyteamidm, teamids[j], parsebyteamidl), warn = FALSE)))
      assign(paste0("TH", i,".",j), flip(as.data.frame(do.call("rbind", jsonconvteamsTH[["resultSets"]][[1]][["rowSet"]]))))
      
    }
  }else{
    jsonconvteamsTH <- fromJSON(paste0(readLines(paste0(parsebyteamidf, getyrs[i], parsebyteamidm, teamids , parsebyteamidl), warn = FALSE)))
    assign(paste0("TH", i,".",1), flip(as.data.frame(do.call("rbind", jsonconvteamsTH[["resultSets"]][[1]][["rowSet"]]))))
    
  }
}

listofteamdfsTH <- list(TH1.1, TH1.2, TH2.1, TH3.1, TH3.2, TH4.1, TH5.1, TH6.1, TH6.2, TH7.1)


for(i in seq_along(listofteamdfsTH)){
  for(j in seq_along(listofteamdfsTH[[1]])){
    listofteamdfsTH[[i]][[j]] <-  as.character(unlist(listofteamdfsTH[[i]][[j]]))
  }
  assign(paste0("TOBY", i), listofteamdfsTH[[i]])
}

## I don't have an easier way of cleaning the mid season trades, so I'm gonna cheat and use his WIKI
## page, which states (citation bitches: https://en.wikipedia.org/wiki/Tobias_Harris) the following:
## FEB 21, 2013 TRADED MIL -> ORL
## FEB 16, 2016 TRADED ORL -> DET
## JAN 29, 2018 TRADED DET -> LAC

## poor Toby, but third times the charm - hope he finds a home with the Clippers:)
## start with the team he started the year with since x.2 is conveniently saved as his posttrade team

##

game_number <- c(1:82)
gamelistdf <- data.frame( "s1718" = NA, "s1718.2" =NA, "s1617" = NA,
                          "s1516" = NA, "s1516.2" = NA, "s1415" = NA, "s1314" = NA, "s1213" = NA,
                          "s1213.2" = NA, "s1112" = NA, "game_number" = game_number)

gamelistdf$s1718 <-  as.character(unlist(TH1.1$V5))
gamelistdf$s1718.2 <-  as.character(unlist(TH1.2$V5))
gamelistdf$s1617 <-  as.character(unlist(TH2.1$V5))
gamelistdf$s1516 <-  as.character(unlist(TH3.1$V5))
gamelistdf$s1516.2 <-  as.character(unlist(TH3.2$V5))
gamelistdf$s1415 <-  as.character(unlist(TH4.1$V5))
gamelistdf$s1314 <-  as.character(unlist(TH5.1$V5))
gamelistdf$s1213 <-  as.character(unlist(TH6.1$V5))
gamelistdf$s1213.2 <-  as.character(unlist(TH6.2$V5))

## 2011-12 season is shorter, so we have to add each entry one by one. This works fine because we're matching game number, not game date

TH1112 <-  as.character(unlist(TH7.1$V5))


for(i in 1:66){
  gamelistdf[[10]][[i]] <- TH1112[i]
}

## he played all 48 games before being traded in 2018 so from 49-82 we'll say he played on clippers.

for(i in 51:82){
  gamelistdf[[1]][[i]] <- gamelistdf[[2]][[i]]
}

## also played all 49 games before being traded in 2016 so from 50-82 we'll say he played on detroit
## SKIRT SKKIRT WE HAVE A PIVOT
## so game IDs get fcked up so we actually want to start the copying of second team's schedule to the first
## for when he made his FIRST APPEARANCE for his second team. makes more sense, since we don't care WHEN 
## he was traded per se... LEts see if this works. We'll switch 1718 to 51 since his first game with the clippers is on 
# the clippers' 51st game vs the Bulls

for(i in 55:82){
  gamelistdf[[4]][[i]] <- gamelistdf[[5]][[i]]
}


## Milwakee's 48th game is Tobias's 28th game in 12-13 season, which is his last game with the Bucks,
## so we'll go with Orlando from 49 to 82

for(i in 56:82){
  gamelistdf[[8]][[i]] <- gamelistdf[[9]][[i]]
}

gamelistdf$s1718.2 <- NULL
gamelistdf$s1516.2 <- NULL
gamelistdf$s1213.2 <- NULL

## Games 50-54 in 15-16 season are fcked up cuz they're unnecessary so we're gonna
## swap games 52-54 to some random numbers less than 0021500806


gamelistdf[[3]][[52]] <- "0021500803"
gamelistdf[[3]][[53]] <- "0021500804"
gamelistdf[[3]][[54]] <- "0021500805"

## his first appearance is in ORLANDO's 56th game but since game numbers 53-55 for MIL
## are higher than the ones for ORL's so we'll create dummy ones like for 15-16 season

gamelistdf[[6]][[53]] <- "0021200822"
gamelistdf[[6]][[54]] <- "0021200823"
gamelistdf[[6]][[55]] <- "0021200824"
## Lets make separate dataframes for each of toby's seasons to merge with

for(i in 1:7){
  assign(paste0("THszn",i), as.data.frame(listofdfsTH[[i]]))
}

for(i in 1:7){
  assign(paste0("THszns",szndisplay[i]), merge(get(paste0("THszn", i)), gamelistdf, by.x = 7, by.y = i, all = TRUE))
  
}

szncompfinaldfTH <- data.frame("Game_Number" = game_number, "s1112" = THszns1112[[31]], "s1213" = THszns1213[[31]], "s1314" = THszns1314[[31]], 
                                  "s1415" =THszns1415[[31]], "s1516" = THszns1516[[31]], "s1617" = THszns1617[[31]], "s1718" = THszns1718[[31]])

## This below does not work, but I can subset the gamelistdf and merge separately season by season
## and then combine desired columns together to get desired graph
## which is easier than doing it this way anyway

TH1112final <- subset()  
  
  
teamgamelogslac <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=1610612746&VsConference=&VsDivision=")

lacjson <- fromJSON(paste(readLines(teamgamelogslac, warn = FALSE), collapse = ""))

lacdf1718 <- as.data.frame(do.call("rbind", lacjson[["resultSets"]][[1]][["rowSet"]]))
clenames = c(clejson[["resultSets"]][[1]][["headers"]])
names(cledf1617) <- c(clenames)
newcledf1617 <- flip(cledf1617)

game_number <- c(1:82)
gameid <- as.character(unlist(newcledf1617$GAME_ID))
  
cle1617gameid <- data.frame("GAME_NUMBER" = game_number, "GAME_ID" = gameid)  

leb1617merged <- merge(leb1617, cle1617gameid, by.x = 7, by.y = 2, all.y = TRUE)

THdata <- plot_ly(szncompfinaldfTH, x = ~game_number) %>%
  add_lines(y = ~s1112, name = "points 11-12") %>%
  add_lines(y = ~s1213, name = "points 12-13") %>%
  add_lines(y = ~s1314, name = "points 13-14") %>%
  add_lines(y = ~s1415, name = "points 14-15") %>%
  add_lines(y = ~s1516, name = "points 15-16") %>%
  add_lines(y = ~s1617, name = "points 16-17") %>%
  add_lines(y = ~s1718, name = "points 17-18") %>%
  
  layout(
    title = "TOBY",
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
    
    yaxis = list(title = "Points"),
    
    updatemenus = list(
      list(
        type = "buttons",
        label = 'Category',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
               label = "View all"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = "Hide all"))), 
      list(
        type = "buttons",
        label = 'Category',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = "S1112"),
          list(method = "restyle",
               args = list('visible', c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = "S1213"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
               label = "S1314"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
               label = "S1415"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
               label = "S1516"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
               label = "S1617"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
               label = "S1718")))))
    

THdata



## Apparently Kobe's Player ID is 977. I cheated by going to his stats page on NBA.com since he's not a current active
## player, he will not be on the current active player list obvi.


library(RJSONIO, warn.conflicts = FALSE)
library(jsonlite, warn.conflicts = FALSE)
library(rjson, warn.conflicts = FALSE)
library(RCurl, warn.conflicts = FALSE)
library(plyr, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)



flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}

szndisplay <- c("1718", "1617", "1516", "1415", "1314",
                "1213", "1112", "1011", "0910", "0809",
                "0708", "0607", "0506", "0405", "0304",
                "0203", "0102", "0001", "9900", "9899",
                "9798", "9697")


getyrs <- c("2017-18", "2016-17", "2015-16", "2014-15", "2013-14",
            "2012-13", "2011-12", "2010-11", "2009-10", "2008-09",
            "2007-08", "2006-07", "2005-06", "2004-05", "2003-04",
            "2002-03", "2001-02", "2000-01", "1999-00", "1998-99",
            "1997-98", "1996-97")

parsebysznKBf <- paste0("http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo",
                        "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                        "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                        "=N&PerMode=Totals&Period=0&PlayerID=977&PlusMinus",
                        "=N&Rank=N&Season=")

parsebysznl <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange",
                      "=&VsConference=&VsDivision=")

listofdfsKB <- list()


for(i in 1:22){
  jsonconvKB <- fromJSON(paste(readLines(paste0(parsebysznKBf, getyrs[i], parsebysznl), warn = FALSE), collapse = ""))
  if(!length(jsonconvKB[["resultSets"]][[1]][["rowSet"]]) == 0){
    listofdfsKB[[i]] <- as.data.frame(do.call("rbind", jsonconvKB[["resultSets"]][[1]][["rowSet"]]))
    categories <- c(jsonconvKB[["resultSets"]][[1]][["headers"]])
    names(listofdfsKB[[i]]) <- c(categories)
    
  }
}
## double pat on the back for me, the for loop works for I.T. so far, showing 7 seasons played.
## with unlisted values, we can just use rev() instead of our flip function


for(i in seq_along(listofdfsKB)){
  for(j in seq_along(listofdfsKB[[1]])){
    listofdfsKB[[i]][[j]] <-  as.character(rev(unlist(listofdfsKB[[i]][[j]])))
  }
}

## Tobias played for two different teams in the 17-18 seasons, detroit and LA. lets see if we 
## can ask R to find this out for us


for(i in 1:7){
  teamids <- unique(listofdfsKB[[i]][[4]])
  teamnames <- unique(listofdfsTH[[i]][[5]])
  parsebyteamidf <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo",
                           "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                           "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                           "=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=")
  parsebyteamidm <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=")
  parsebyteamidl <- paste0("&VsConference=&VsDivision=")
  if(!length(unique(teamids)) == 1){
    for(j in seq_along(teamids)){
      jsonconvteamsTH <- fromJSON(paste0(readLines(paste0(parsebyteamidf, getyrs[i], parsebyteamidm, teamids[j], parsebyteamidl), warn = FALSE)))
      assign(paste0("TH", i,".",j), flip(as.data.frame(do.call("rbind", jsonconvteamsTH[["resultSets"]][[1]][["rowSet"]]))))
      
    }
  }else{
    jsonconvteamsTH <- fromJSON(paste0(readLines(paste0(parsebyteamidf, getyrs[i], parsebyteamidm, teamids , parsebyteamidl), warn = FALSE)))
    assign(paste0("TH", i,".",1), flip(as.data.frame(do.call("rbind", jsonconvteamsTH[["resultSets"]][[1]][["rowSet"]]))))
    
  }
}

categoriestm <- c(jsonconvteamsTH[["resultSets"]][[1]][["headers"]])

## manually making the list of dataframes to unlist them as before using the unlist for loop

listofteamdfsTH <- list(TH1.1, TH1.2, TH2.1, TH3.1, TH3.2, TH4.1, TH5.1, TH6.1, TH6.2, TH7.1)


for(i in seq_along(listofteamdfsTH)){
  for(j in seq_along(listofteamdfsTH[[1]])){
    listofteamdfsTH[[i]][[j]] <-  as.character(unlist(listofteamdfsTH[[i]][[j]]))
  }
  names(listofteamdfsTH[[i]]) <- c(categoriestm)
  assign(paste0("TOBY", i), listofteamdfsTH[[i]])
}


for(i in 1:7){
  assign(paste0("THszn",i), as.data.frame(listofdfsTH[[i]]))
}

## Everything unlisted (sortable), and nicely iterated, but first we'll find the process it takes to make the full career gamelog


## 2017-18 szn
toby1718.1 <- TOBY1[c(1:50),c(1,5:7)]

toby1718.2 <- TOBY2[c(51:82),c(1,5:7)]


toby1718 <- rbind(toby1718.1, toby1718.2)

toby1718$GAME_NUMBER <- c(1:82)

THszn1$GAME_PLAYED <- c(1:80)

thgamelog1718 <- merge(THszn1, toby1718, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)

## 2016-17 szn
toby1617 <- TOBY3[c(1:82),c(1,5:7)]

toby1617$GAME_NUMBER <- c(1:82)

THszn2$GAME_PLAYED <- c(1:82)

thgamelog1617 <- merge(THszn2, toby1617, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)


## 2015-16 szn

toby1516.1 <- TOBY4[c(1:54),c(1,5:7)]

toby1516.2 <- TOBY5[c(55:82),c(1,5:7)]


toby1516 <- rbind(toby1516.1, toby1516.2)

toby1516$GAME_NUMBER <- c(1:82)

THszn3$GAME_PLAYED <- c(1:76)

thgamelog1516 <- merge(THszn3, toby1516, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)


## 2014-15 szn
toby1415 <- TOBY6[c(1:82),c(1,5:7)]

toby1415$GAME_NUMBER <- c(1:82)

THszn4$GAME_PLAYED <- c(1:68)

thgamelog1415 <- merge(THszn4, toby1415, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)


## 2013-14 szn
toby1314 <- TOBY7[c(1:82),c(1,5:7)]

toby1314$GAME_NUMBER <- c(1:82)

THszn5$GAME_PLAYED <- c(1:61)

thgamelog1314 <- merge(THszn5, toby1314, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)


## 2012-13 szn


toby1213.1 <- TOBY8[c(1:55),c(1,5:7)]

toby1213.2 <- TOBY9[c(56:82),c(1,5:7)]


toby1213 <- rbind(toby1213.1, toby1213.2)

toby1213$GAME_NUMBER <- c(1:82)

THszn6$GAME_PLAYED <- c(1:55)

thgamelog1213 <- merge(THszn6, toby1213, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)



## 2011-12 szn
toby1112 <- TOBY10[c(1:66),c(1,5:7)]

toby1112$GAME_NUMBER <- c(1:66)

THszn7$GAME_PLAYED <- c(1:42)

thgamelog1112 <- merge(THszn7, toby1112, by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE)


thgamelog1718$SEASON_DISPLAY <- as.numeric("2018")
thgamelog1617$SEASON_DISPLAY <- as.numeric("2017")
thgamelog1516$SEASON_DISPLAY <- as.numeric("2016")
thgamelog1415$SEASON_DISPLAY <- as.numeric("2015")
thgamelog1314$SEASON_DISPLAY <- as.numeric("2014")
thgamelog1213$SEASON_DISPLAY <- as.numeric("2013")
thgamelog1112$SEASON_DISPLAY <- as.numeric("2012")

## So to explain what I did just now, i have to explain the issue behind trying to depend each
## player to an 82 game season. Toby has three mid season trades, and it gets ugly when trying 
## to combine two different dataframes representing two different teams' schedules. For instance,
## the LA clippers may have acquired Tobias before they play their 50th game, but at the time
## of this acquisition, Tobias may have already played 52 games with the Detroit Pistons. This
## is a drastic example, and thankfully, there's a gap of inactive dates large enough so that we 
## can say that Tobias missed two games between his last appearance with the Pistons on Jan. 29, 2018 and his
## first appearance with the clippers on Feb. 03, 2018. This is simply for fantasy purposes, so we can know
## exactly how many games he misses in the regular season and when he misses those games.

## So the mid season trades is exactly what spurred me to rbind then merge as I did for Tobias's three mid season trades.

## Unlisting the dataframes is a must for the process I've used, since it is necessary for keeping order in tact

thcareerlog <- rbind(thgamelog1112, thgamelog1213, thgamelog1314, thgamelog1415, thgamelog1516, thgamelog1617, thgamelog1718)

thcareerlognum <- thcareerlog[c(1:558), c(1, 11:65, 69:70)]

for (i in seq_along(thcareerlognum)){
  thcareerlognum[[i]] <- as.numeric(unlist(thcareerlognum[[i]]))
}


th <- group_by(thcareerlognum, SEASON_DISPLAY)


th[is.na(th)] <- 0


yearlycomp <- plot_ly(th, x = ~GAME_NUMBER, frame = ~SEASON_DISPLAY) %>%
  add_lines(y = ~PTS)

th$CAREER_GAMES <- c(1:558)

careerview <- plot_ly(th, x = ~CAREER_GAMES, y = ~PTS, type='scatter', mode='lines')

careerview



jsonconvTH1 <- fromJSON(paste0(readLines("http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=202699&PlusMinus=N&Rank=N&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision=", warn = FALSE)))


json <- "http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerID=202699&PlusMinus=N&Rank=N&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision="

data <- readLines(json)


jsonconv <- fromJSON(paste0(data))

