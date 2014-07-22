
   #plots some of the graphs found here: http://www.darynr.com/pointsWinsDraws.htm

    library(ggplot2)
    library(dplyr)
    
    #set directory appropriately
    setwd()
    PL <- read.csv("PremierLeagueHistory.csv", stringsAsFactors = FALSE)
    LL <- read.csv("LaLigaHistory.csv", stringsAsFactors = FALSE)
    #above input files here: https://github.com/banjosupreme/Data/blob/master/LaLigaHistory.csv
    #                        https://github.com/banjosupreme/Data/blob/master/PremierLeagueHistory.csv
    
    
    names(PL)[10] <- "FinalPts"
    names(PL)[11] <- "Pts"
    
    
    PL <- PL[PL$SeasonNum>=firstYear & PL$SeasonNum<=lastYear,]
    LL <- LL[LL$SeasonNum>=firstYear & LL$SeasonNum<=lastYear ,]
    
    
    aggPtsPL <- aggregate(list(Pts = PL$Pts), by = list(Season = PL$Season), sum)
    aggPtsLL <- aggregate(list(Pts = LL$Pts), by = list(Season = LL$Season), sum)
    
    sseason <- unique(PL[PL$SeasonNum==firstYear,]$Season)[1]
    eseason <- unique(PL[PL$SeasonNum==lastYear,]$Season)[1]
    
    
    LLdist<-aggregate(list(LL_Avg = LL$Pts), list(Pos = LL$Pos), mean)
    PLdist<-aggregate(list(PL_Avg = PL$Pts), list(Pos = PL$Pos), mean)
    
    compTable <- merge(PLdist, LLdist)
    names(compTable) <- c("Pos", "Premier League", "La Liga")
    compTable <- melt(compTable, id.vars = c("Pos"))
    
    names(compTable) <- c("Pos", "League", "Points")
    p <- ggplot(aes(x=Pos, y=Points, colour=League), data = compTable) + geom_line(aes(group=League)) + geom_point(size=3) + scale_x_continuous(breaks = 1:20) + theme_bw() + scale_y_continuous(breaks = seq(25,100,by = 5))
    p <- p +  ylab("Points") + xlab("Position") + ggtitle(paste("Comparison of leagues by average points at each ranking ", "(", sseason, " to " ,  eseason, ")", sep="" ))
    
    
    #win vs draw plot here
    
    LLwindraw<-aggregate(list(Wins = LL$W, Draws = LL$D/2), list(Season = LL$End), sum)
    PLwindraw<-aggregate(list(Wins = PL$W, Draws = PL$D/2), list(Season = PL$End), sum)
    
    LLwindraw$League <- "La Liga"
    PLwindraw$League <- "Premier League"
    
    windraw <- rbind(LLwindraw,PLwindraw)
    
    windraw <- melt(windraw, id.vars = c("Season", "League"))
    windraw$Season <- factor(windraw$Season, levels = seq(2005,2014,by = 1), ordered = TRUE)
    names(windraw)[3] <- "Results"
    
    q <- ggplot(aes(x = Season, y = value, fill = Results), data = windraw) + geom_bar(stat='identity') + facet_wrap(~League) +  theme_bw()
    q <- q + ggtitle(paste("Relative Frequencies of Wins vs Draws by League ", "(", sseason, " to " ,  eseason, ")", sep="" )) + ylab("Number of games") + xlab("Season")
   
   
   #same thing as above using Pythagorean wins
   
   LL$GF2 <- (LL$GF*LL$GF)
   LL$GA2 <- (LL$GA*LL$GA)
   LL$Pyth <- LL$GF2/(LL$GF2 + LL$GA2)
   
   PL$GF2 <- (PL$GF*PL$GF)
   PL$GA2 <- (PL$GA*PL$GA)
   PL$Pyth <- PL$GF2/(PL$GF2 + PL$GA2)
   
    LLpyth<-aggregate(list(LL_Avg = LL$Pyth), list(Pos = LL$Pos), mean)
    PLpyth<-aggregate(list(PL_Avg = PL$Pyth), list(Pos = PL$Pos), mean)
    
    pythTable <- merge(PLpyth, LLpyth)
    names(pythTable) <- c("Pos", "Premier League", "La Liga")
    
    pythTable <- melt(pythTable, id.vars = c("Pos"))
    
    names(pythTable) <- c("Pos", "League", "Pyth")
    r <- ggplot(aes(x=Pos, y=Pyth, colour=League), data = pythTable) + geom_line(aes(group=League)) + geom_point(size=3) + scale_x_continuous(breaks = 1:20) + theme_bw() 
    r <- r +  ylab("Pythagorean Win Pct") + xlab("Position") + ggtitle(paste("Comparison of leagues by average Pythagorean Expectation at each ranking ", "(", sseason, " to " ,  eseason, ")", sep="" ))

   
   #same thing as above using GD instead
    
    
    LLdist<-aggregate(list(LL_Avg = LL$GD), list(Pos = LL$Pos), mean)
    PLdist<-aggregate(list(PL_Avg = PL$GD), list(Pos = PL$Pos), mean)
    
    distTable <- merge(PLdist, LLdist)
    names(distTable) <- c("Pos", "Premier League", "La Liga")
    distTable <- melt(distTable, id.vars = c("Pos"))
    names(distTable) <- c("Pos", "League", "GD")
    
    s <- ggplot(aes(x=Pos, y=GD, colour=League), data = distTable) + geom_line(aes(group=League)) + geom_point(size=3) + scale_x_continuous(breaks = 1:20) + theme_bw() 
    s <- s +  ylab("Goal Difference") + xlab("Position") + ggtitle(paste("Comparison of leagues by average Goal Difference at each ranking ", "(", sseason, " to " ,  eseason, ")", sep="" ))
