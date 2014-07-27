library(ggplot2)

######### per games and 100 possessions together

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

AgPG <- read.csv("AguirrePerGame.csv", stringsAsFactors = FALSE)
WoPG <- read.csv("WorthyPerGame.csv", , stringsAsFactors = FALSE)
#remove the entries for Aguirre for Detroit and Dallas for the 88-89 season and just go with the total row
AgPG <- AgPG[!(AgPG$Season == "1988-89" & AgPG$Tm != "TOT"),]

AgPG$Player<-"Aguirre"
WoPG$Player<-"Worthy"


perGame<-rbind(AgPG, WoPG)
perGame$Type <- "Per Game"

A100 <- read.csv("Aguirre100Poss.csv", stringsAsFactors = FALSE)
W100 <- read.csv("Worthy100Poss.csv", stringsAsFactors = FALSE)

A100$Player <- "Aguirre"
W100$Player <- "Worthy"
per100 <- rbind(A100,W100)
per100$Type <- "Per 100 possessions"


A36 <- read.csv("AguirrePer36.csv", stringsAsFactors = FALSE)
W36 <- read.csv("WorthyPer36.csv", stringsAsFactors = FALSE)

A36$Player <- "Aguirre"
W36$Player <- "Worthy"
per36 <- rbind(A36,W36)
per36$Type <- "Per 36 minutes"

pba <- rbind(perGame,per100, per36)
pba$Type <- ordered(pba$Type, levels = c("Per Game", "Per 36 minutes", "Per 100 possessions"))
pba$Year<-paste("'",substrRight(pba$Season,2),sep="")

ptsPlot <- ggplot(aes(x = Year, y = PTS, colour = Player), data = pba) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) +xlab("Season") + ggtitle("Mark Aguirre vs James Worthy: Scoring")
boardsPlot <- ggplot(aes(x = Year, y = TRB, colour = Player), data = pba) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) +xlab("Season") + ggtitle("Mark Aguirre vs James Worthy: Rebounding")
dimesPlot <- ggplot(aes(x = Year, y = AST, colour = Player), data = pba) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) +xlab("Season") + ggtitle("Mark Aguirre vs James Worthy: Assists")

###################### same thing for playoffs. Mostly just copy-pasted.

AgPG <- read.csv("AguirrePlayoffPerGame.csv", stringsAsFactors = FALSE)
WoPG <- read.csv("WorthyPlayoffPerGame.csv", , stringsAsFactors = FALSE)
#remove the entries for Aguirre for Detroit and Dallas for the 88-89 season and just go with the total row
AgPG <- AgPG[!(AgPG$Season == "1988-89" & AgPG$Tm != "TOT"),]

AgPG$Player<-"Aguirre"
WoPG$Player<-"Worthy"


pperGame<-rbind(AgPG, WoPG)
pperGame$Type <- "Per Game"

A100 <- read.csv("AguirrePlayoff100Poss.csv", stringsAsFactors = FALSE)
W100 <- read.csv("WorthyPlayoff100Poss.csv", stringsAsFactors = FALSE)

A100$Player <- "Aguirre"
W100$Player <- "Worthy"
pper100 <- rbind(A100,W100)
pper100$Type <- "Per 100 possessions"


A36 <- read.csv("AguirrePlayoffPer36.csv", stringsAsFactors = FALSE)
W36 <- read.csv("WorthyPlayoffPer36.csv", stringsAsFactors = FALSE)

A36$Player <- "Aguirre"
W36$Player <- "Worthy"
pper36 <- rbind(A36,W36)
pper36$Type <- "Per 36 minutes"

pba <- rbind(pperGame,pper100, pper36)
pba$Type <- ordered(pba$Type, levels = c("Per Game", "Per 36 minutes", "Per 100 possessions"))
pba$Year<-paste("'",substrRight(pba$Season,2),sep="")

pptsPlot <- ggplot(aes(x = Year, y = PTS, colour = Player), data = pba) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) + xlab("Season") +ggtitle("Mark Aguirre vs James Worthy: Playoff Scoring")
pboardsPlot <- ggplot(aes(x = Year, y = TRB, colour = Player), data = pba) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) + xlab("Season") +ggtitle("Mark Aguirre vs James Worthy: Playoff Rebounding")
pdimesPlot <- ggplot(aes(x = Year, y = AST, colour = Player), data = pba) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) + xlab("Season") +ggtitle("Mark Aguirre vs James Worthy: Playoff Assists")

########adv stats



AgAdv <- read.csv("AguirreAdv.csv", stringsAsFactors = FALSE)
WoAdv <- read.csv("WorthyAdv.csv", stringsAsFactors = FALSE)
AgAdv$Player <- "Aguirre"
WoAdv$Player <- "Worthy"
advStats <- rbind(AgAdv, WoAdv)
advStats$Type <- "Regular Season"

pAgAdv <- read.csv("AguirrePlayoffAdv.csv", stringsAsFactors = FALSE)
pWoAdv <- read.csv("WorthyPlayoffAdv.csv", stringsAsFactors = FALSE)
pAgAdv$Player <- "Aguirre"
pWoAdv$Player <- "Worthy"
padvStats <- rbind(pAgAdv, pWoAdv)
padvStats$Type <- "Playoffs"

advStats <- rbind(advStats, padvStats)
advStats$Type <- ordered(advStats$Type, levels = c("Regular Season", "Playoffs"))

advStats$Year<-paste("'",substrRight(advStats$Season,2),sep="")



PERPlot <- ggplot(aes(x = Year, y = PER, colour = Player), data = advStats) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) + xlab("Season") + ylab("PER") 
PERPlot <- PERPlot + ggtitle("Mark Aguirre vs James Worthy: PER")

rbpctPlot <- ggplot(aes(x = Year, y = TRB., colour = Player), data = advStats) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) + xlab("Season")  + ylab("Rebound %")
rbpctPlot <- rbpctPlot + ggtitle("Mark Aguirre vs James Worthy: Rebound %")

astpctPlot <- ggplot(aes(x = Year, y = AST., colour = Player), data = advStats) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type) + xlab("Season") + ylab("Assist %")
astpctPlot <- astpctPlot + ggtitle("Mark Aguirre vs James Worthy: Assist %")

tspctPlot <- ggplot(aes(x = Year, y = TS., colour = Player), data = advStats) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type)+ xlab("Season") + ylab("True Shooting %")
tspctPlot <- tspctPlot + ggtitle("Mark Aguirre vs James Worthy: True Shooting %")

wsPlot <- ggplot(aes(x = Year, y = WS, colour = Player), data = advStats) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type)+ xlab("Season") + ylab("Win Shares")
wsPlot <- wsPlot + ggtitle("Mark Aguirre vs James Worthy: Win Shares")

ws48Plot <- ggplot(aes(x = Year, y = WS.48, colour = Player), data = advStats) + geom_point() + geom_line(aes(group = Player)) + theme_bw() + facet_wrap(~Type)+ xlab("Season") + ylab("Win Shares per 48")
ws48Plot <- ws48Plot + ggtitle("Mark Aguirre vs James Worthy: Win Shares per 48 minutes")






