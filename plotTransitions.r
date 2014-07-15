#this function plots transition probabilities for placing in La Liga and the premier league from one year to the next

plotTransitions <- function(firstYear=0, lastYear=21)
 {

    library(ggplot2)
    library(dplyr)
    #assuming directory is correct and the files are named as below
    PL <- read.csv("PremierLeague.csv", stringsAsFactors = FALSE)
    LL <- read.csv("LaLiga.csv", stringsAsFactors = FALSE)
    
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
    transitions$dummy <- 1
    x<-aggregate(transitions$dummy, list(Pos = transitions$Pos, nextPos = transitions$nextPos), sum)
    y<-aggregate(transitions$dummy, list(Pos = transitions$Pos), sum)
    names(y)[2] <- "denominator"
    names(x)[3] <- "numerator" 
    x$denominator <- y$denominator[match(x$Pos,y$Pos)]
    x$prob <- x$numerator/x$denominator
    PLtrans <- x
    PLtrans <- PLtrans[PLtrans$Pos <= 17,]


    LL <- read.csv("LaLiga.csv", stringsAsFactors = FALSE)
    LL2 <- LL
    LL2$SeasonNum <- LL2$SeasonNum -1
    names(LL2)[1]<- "nextPos"

    S1 <- LL[,c("Pos","Team", "SeasonNum")]
    S2 <- LL2[,c("nextPos","Team","SeasonNum")]
    transitions <- merge(S1,S2)
    transitions$dummy <- 1
    x<-aggregate(transitions$dummy, list(Pos = transitions$Pos, nextPos = transitions$nextPos), sum)
    y<-aggregate(transitions$dummy, list(Pos = transitions$Pos), sum)
    names(y)[2] <- "denominator"
    names(x)[3] <- "numerator" 
    x$denominator <- y$denominator[match(x$Pos,y$Pos)]
    x$prob <- x$numerator/x$denominator

    LLtrans <- x
    LLtrans <- LLtrans[LLtrans$Pos <= 17,]

    
    from <- 1:17
    to <- 1:20
    league <- c("Premier League", "La Liga")

    bigTrans <- expand.grid(Pos = from, nextPos = to, League = league)

    LLtrans$key <- paste(LLtrans$Pos,LLtrans$nextPos, "La Liga",sep="*")
    PLtrans$key <- paste(PLtrans$Pos,PLtrans$nextPos, "Premier League",sep="*")
    
    PLtrans <- rbind(PLtrans, LLtrans)

    bigTrans$key <- paste(bigTrans$Pos,bigTrans$nextPos, bigTrans$League, sep = "*")
    bigTrans$prob <-  PLtrans$prob[match(bigTrans$key,PLtrans$key)] 
    bigTrans$prob <- ifelse(is.na(bigTrans$prob),0,bigTrans$prob)

    

    bigTrans$Pos <- factor(bigTrans$Pos, levels = seq(17,1,by = -1), ordered = TRUE)
    bigTrans$nextPos <- factor(bigTrans$nextPos, levels = seq(1,20,by = 1), ordered = TRUE)
    
    p <- ggplot(aes(x = nextPos, y = Pos), data = bigTrans) + geom_tile(aes(fill=prob)) + scale_fill_gradient(low = "blue", high = "red") + facet_wrap(~League)
    p <- p + ggtitle(paste("Transition Probabilities of League placing from one year to the next ", "(", sseason, " to " ,  eseason, ")", sep="" )) + ylab("Initial Year Place") + xlab("Following Year Place") 
    
    return(p)

}    
