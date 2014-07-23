library(dplyr)


#read the file found here: https://github.com/banjosupreme/Data/blob/master/WCPenaltyShootouts.csv
pkData <- read.csv("WCPenaltyShootouts.csv")
pkData$Goals <- ifelse(pkData$Scored, 1, 0)

pk_countries <- pkData %.%
                group_by(Team) %.%
                summarise(convRate = mean(Goals), n = n()) %.%
                arrange(-convRate, -n)
                
pk_position <- pkData %.%
                group_by(Position) %.%
                summarise(convRate = mean(Goals), n = n()) %.%
                arrange(Position)
                
pk_year <- pkData %.%
                group_by(Year) %.%
                summarise(convRate = mean(Goals), n = n()) %.%
                arrange(-convRate)
