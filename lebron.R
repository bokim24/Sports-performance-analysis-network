

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

szndisplay <- c("1819", "1718", "1617", "1516", "1415", "1314",
                "1213", "1112", "1011", "0910", "0809",
                "0708", "0607", "0506", "0405", "0304",
                "0203", "0102", "0001", "9900", "9899",
                "9798", "9697")


getyrs <- c("2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14",
            "2012-13", "2011-12", "2010-11", "2009-10", "2008-09",
            "2007-08", "2006-07", "2005-06", "2004-05", "2003-04",
            "2002-03", "2001-02", "2000-01", "1999-00", "1998-99",
            "1997-98", "1996-97")

grouprow <- c("2018", "2017",
              "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007",
              "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998", "1997")

gettingplayerids <- paste0("http://www.nba.com/players/active_players.json")

playeridsjson <- fromJSON(paste(readLines(gettingplayerids, warn = FALSE), collapse = ""))

playerinfodf <- as.data.frame(do.call("rbind", playeridsjson))

## LJ player ID: 2544

parsebysznLJf <- paste0("http://stats.nba.com/stats/playergamelogs?DateFrom=&DateTo",
                        "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                        "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                        "=N&PerMode=Totals&Period=0&PlayerID=2544&PlusMinus",
                        "=N&Rank=N&Season=")

parsebysznl <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange",
                      "=&VsConference=&VsDivision=")

listofURLsLJ <- list()

for(i in 1:23){
  listofURLsLJ[[i]]<- paste0(parsebysznLJf, getyrs[i], parsebysznl)
}


## So I have to warn those who read my code to formulate their own because we may be doing similar
## processes for various desired results. So anyway, this for loop below that You'll see many times
## in my code is what generates the NBA.com advanced gamelogs JSON files. However, for one reason or another
## this combination of fromJSON(paste0(readlines(json url))) likes to go into timeout every once in awhile...
## I haven't figured out why it does this.. I was using seq_along(getyrs) for iteration of i, but I changed that
## 1 in 1:20 for this reason: I thought that the seq_along was causing my code to run in an infinite loop aka 
## causing error and timeout, but that wasn't it. sometimes it doesn't even work when I try to pass a single url 
## to this process, which is what really irks me (might just have to do with the way that the ports are set up, or
## maybe it's something to do with permissions idk). Usually, when I clear environment and restart R, and rerun the
## whole script from the top (aka the libraries) everything runs smoothly.


listofdfsLJ <- list()

for(i in seq_along(getyrs)){
  jsonconvLJ <- fromJSON(paste(readLines(paste0(parsebysznLJf, getyrs[i], parsebysznl), warn = FALSE), collapse = ""))
  if(!length(jsonconvLJ[["resultSets"]][[1]][["rowSet"]]) == 0){
    listofdfsLJ[[i]] <- as.data.frame(do.call("rbind", jsonconvLJ[["resultSets"]][[1]][["rowSet"]]))
    categories <- c(jsonconvLJ[["resultSets"]][[1]][["headers"]])
    names(listofdfsLJ[[i]]) <- c(categories)
    
  }
}

for(i in seq_along(listofdfsLJ)){
  for(j in seq_along(listofdfsLJ[[1]])){
    listofdfsLJ[[i]][[j]] <-  as.character(rev(unlist(listofdfsLJ[[i]][[j]])))
  }
  assign(paste0("LJszn",i), as.data.frame(listofdfsLJ[[i]]))
}

teamsznsdisplay <- list()

for(i in seq_along(listofdfsLJ)){
  teamids <- unique(listofdfsLJ[[i]][[4]])
  teamnames <- unique(listofdfsLJ[[i]][[5]])
  parsebyteamidf <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo",
                           "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                           "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                           "=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=")
  parsebyteamidm <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=")
  parsebyteamidl <- paste0("&VsConference=&VsDivision=")
  if(!length(unique(teamids)) == 1){
    for(j in seq_along(teamids)){
      jsonconvteamsLJ <- fromJSON(paste0(readLines(paste0(parsebyteamidf, getyrs[i], parsebyteamidm, teamids[j], parsebyteamidl), warn = FALSE)))
      assign(paste0("LJ", i,".",j), flip(as.data.frame(do.call("rbind", jsonconvteamsLJ[["resultSets"]][[1]][["rowSet"]]))))
      
    }
  }else{
    jsonconvteamsLJ <- fromJSON(paste0(readLines(paste0(parsebyteamidf, getyrs[i], parsebyteamidm, teamids , parsebyteamidl), warn = FALSE)))
    assign(paste0("LJ", i,".",1), flip(as.data.frame(do.call("rbind", jsonconvteamsLJ[["resultSets"]][[1]][["rowSet"]]))))
    
  }
}

## Tobias played for two different teams in the 17-18 seasons, detroit and LA. lets see if we 
## can ask R to find this out for us

ListofteamURLsLJ <- list()

LALid <- "1610612747"
parsebyteamidf <- paste0("http://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo",
                         "=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType",
                         "=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust",
                         "=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=")
parsebyteamidm <- paste0("&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=")
parsebyteamidl <- paste0("&VsConference=&VsDivision=")



for(i in 1:20){
  ListofteamURLsKB[[i]] <- paste0(parsebyteamidf, getyrs20[i], parsebyteamidm, LALid, parsebyteamidl)
}

ListofteamdfsKB <- list()

for(i in 1:20){
  jsonconvteamsKB <- fromJSON(paste0(readLines(ListofteamURLsKB[[i]], warn = FALSE)))
  ListofteamdfsKB[[i]] <- as.data.frame(do.call("rbind", jsonconvteamsKB[["resultSets"]][[1]][["rowSet"]]))
  categoriestm <- c(jsonconvteamsKB[["resultSets"]][[1]][["headers"]])
  names(ListofteamdfsKB[[i]]) <- c(categoriestm)
}


for(i in seq_along(ListofteamdfsKB)){
  for(j in seq_along(ListofteamdfsKB[[1]])){
    ListofteamdfsKB[[i]][[j]] <-  as.character(rev(unlist(ListofteamdfsKB[[i]][[j]])))
  }
  assign(paste0("KOBE", i), ListofteamdfsKB[[i]])
}


## Everything unlisted (sortable), and nicely iterated, but first we'll find the process it takes to make the full career gamelog

## (first name in caps)(season number*last season first) <- i.e. TOBY10, KOBE5 
##   -> these are the gamelogs of the teams that said player played on.
##      if we have a case like Tobias where he has multiple dataframes for
##      a single season, we cut and rbind (refer to toby code) where appropriate.
##      otherwise, we make (player_initials)gamelog(season) by merging. 





## 2015-16 szn <-- last season first** Kobe's last active season is the 15-16 season.
## Since we have 20 dataframes to merge and match, maybe it's best for us to make a loop/function...
kobemerge <- list()


## possibly the most beautiful for loop I've written thus far.
## code should work for any player who did not have a mid season trade 


for(i in 1:20){
  kobemerge[[i]] <- ListofteamdfsKB[[i]][c(1:nrow(ListofteamdfsKB[[i]])),c(1,5:7)]
  kobemerge[[i]]$GAME_NUMBER <- c(1:nrow(ListofteamdfsKB[[i]]))
  kobemerge[[i]]$SEASON_DISPLAY <- as.numeric(grouprow[[i]])
  listofdfsKB[[i]]$GAMES_PLAYED <- c(1:nrow(listofdfsKB[[i]]))
  assign(paste0("KBgamelogz", szndisplay[[i]]), merge(listofdfsKB[[i]], kobemerge[[i]], by.x = 7, by.y = 2, all.x = TRUE, all.y = TRUE))
}


KBcareerlog <- rbind(KBgamelogz9697, KBgamelogz9798, KBgamelogz9899, KBgamelogz9900, KBgamelogz0001, KBgamelogz0102, KBgamelogz0203,
                     KBgamelogz0304, KBgamelogz0405, KBgamelogz0506, KBgamelogz0607, KBgamelogz0708, KBgamelogz0809, KBgamelogz0910, 
                     KBgamelogz1011, KBgamelogz1112, KBgamelogz1213, KBgamelogz1314, KBgamelogz1415, KBgamelogz1516)

KBcareerlognum <- KBcareerlog[c(1:1592), c(1, 11:65, 69:70)]

for (i in seq_along(KBcareerlognum)){
  KBcareerlognum[[i]] <- as.numeric(unlist(KBcareerlognum[[i]]))
}


KB <- group_by(KBcareerlognum, SEASON_DISPLAY)


KB[is.na(KB)] <- 0


yearlycomp <- plot_ly(KB, x = ~GAME_NUMBER, frame = ~SEASON_DISPLAY) %>%
  add_lines(y = ~PTS)

yearlycomp

KB$CAREER_GAMES <- c(1:1592)

careerview <- plot_ly(KB, x = ~CAREER_GAMES, y = ~PTS, type='scatter', mode='lines')

careerview

