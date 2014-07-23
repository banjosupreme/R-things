plotGroupTransitions <- function(firstYear=0, lastYear=21)
 {

    #I probably should have made the other function in plotTransitions.r more general but it was pretty easy to modify the 
    #the necessary lines in here to get what I wanted. This script makes the last graph on this page: http://www.darynr.com/leagueTransitions.htm
    


    library(ggplot2)
    library(dplyr)
    
    #following commented line switches to the appropriate directory
    #setwd()
    #files here:  
    #https://github.com/banjosupreme/Data/blob/master/LaLigaHistory.csv
    #https://github.com/banjosupreme/Data/blob/master/PremierLeagueHistory.csv
    
    
    PL <- read.csv("PremierLeagueHistory.csv", stringsAsFactors = FALSE)
    LL <- read.csv("LaLigaHistory.csv", stringsAsFactors = FALSE)
    
    
    PL <- PL[PL$SeasonNum>=firstYear & PL$SeasonNum<=lastYear,]
    LL <- LL[LL$SeasonNum>=firstYear & LL$SeasonNum<=lastYear ,]
    
    sseason <- unique(PL[PL$SeasonNum==firstYear,]$Season)[1]
    eseason <- unique(PL[PL$SeasonNum==lastYear,]$Season)[1]
    
    
    PL2 <- PL
    PL2$SeasonNum <- PL2$SeasonNum -1
    names(PL2)[1]<- "nextPos"

    S1 <- PL[,c("Pos","Team", "SeasonNum")]
    S2 <- PL2[,c("nextPos","Team","SeasonNum")]
    transitions <- merge(S1,S2)
    transitions$Group <- ceiling((transitions$Pos)/4)
    transitions$Group <- ifelse(transitions$Pos==17,4,transitions$Group)  
    
    transitions$nextGroup <- ceiling((transitions$nextPos)/4)
    transitions$nextGroup <- ifelse(transitions$nextPos==17,4,transitions$nextGroup)  
    
    
    transitions$dummy <- 1
    x<-aggregate(transitions$dummy, list(Group = transitions$Group, nextGroup = transitions$nextGroup), sum)
    y<-aggregate(transitions$dummy, list(Group = transitions$Group), sum)
    names(y)[2] <- "denominator"
    names(x)[3] <- "numerator" 
    x$denominator <- y$denominator[match(x$Group,y$Group)]
    x$prob <- x$numerator/x$denominator
    PLtrans <- x
    PLtrans <- PLtrans[PLtrans$Group <= 4,]


    LL2 <- LL
    LL2$SeasonNum <- LL2$SeasonNum -1
    names(LL2)[1]<- "nextPos"

    S1 <- LL[,c("Pos","Team", "SeasonNum")]
    S2 <- LL2[,c("nextPos","Team","SeasonNum")]
    transitions <- merge(S1,S2)
    
    transitions$Group <- ceiling((transitions$Pos)/4)
    transitions$Group <- ifelse(transitions$Pos==17,4,transitions$Group)  
    
    transitions$nextGroup <- ceiling((transitions$nextPos)/4)
    transitions$nextGroup <- ifelse(transitions$nextPos==17,4,transitions$nextGroup)  
    
    transitions$dummy <- 1
    x<-aggregate(transitions$dummy, list(Group = transitions$Group, nextGroup = transitions$nextGroup), sum)
    y<-aggregate(transitions$dummy, list(Group = transitions$Group), sum)
    names(y)[2] <- "denominator"
    names(x)[3] <- "numerator" 
    x$denominator <- y$denominator[match(x$Group,y$Group)]
    x$prob <- x$numerator/x$denominator

    LLtrans <- x
    LLtrans <- LLtrans[LLtrans$Group <= 4,]

    
    from <- 1:4
    to <- 1:5
    league <- c("Premier League", "La Liga")

    bigTrans <- expand.grid(Group = from, nextGroup = to, League = league)

    LLtrans$key <- paste(LLtrans$Group,LLtrans$nextGroup, "La Liga",sep="*")
    PLtrans$key <- paste(PLtrans$Group,PLtrans$nextGroup, "Premier League",sep="*")
    
    PLtrans <- rbind(PLtrans, LLtrans)

    bigTrans$key <- paste(bigTrans$Group,bigTrans$nextGroup, bigTrans$League, sep = "*")
    bigTrans$prob <-  PLtrans$prob[match(bigTrans$key,PLtrans$key)] 
    bigTrans$prob <- ifelse(is.na(bigTrans$prob),0,bigTrans$prob)

    
    
   
    bigTrans$Group <- factor(bigTrans$Group, levels = seq(4,1,by = -1), ordered = TRUE)
    bigTrans$nextGroup <- factor(bigTrans$nextGroup, levels = seq(1,5,by = 1), ordered = TRUE)
    
    p <- ggplot(aes(x = nextGroup, y = Group), data = bigTrans) + geom_tile(aes(fill=prob)) + scale_fill_gradient(low = "blue", high = "red") + facet_wrap(~League)
    p <- p + ggtitle(paste("Transition Probabilities of League placing from one year to the next ", "(", sseason, " to " ,  eseason, ")", sep="" )) + ylab("Initial Year Place") + xlab("Following Year Place") 
    
    return(p)
    
}
    
    
