download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")
original <- read.csv("file.csv.bz2", header= TRUE, stringsAsFactors = FALSE, nrows = 1)
str(original)
weather <- read.csv("file.csv.bz2", header= TRUE, stringsAsFactors = FALSE)[ ,c('EVTYPE', 'FATALITIES', 'INJURIES','PROPDMG' )]
dim(weather)
str(weather)
library(dplyr)
weather_case <- mutate_each(weather, funs(toupper))
cols.num <- c('FATALITIES', 'INJURIES','PROPDMG');
weather_case[cols.num] <- sapply(weather_case[cols.num],as.numeric)
sapply(weather_case, class)
str(weather_case)
#aggregate by event type
weather_aggregated <- setNames(aggregate(weather_case$FATALITIES ~ EVTYPE, data = weather_case, sum, na.rm=TRUE),c("Type", "Calc"))
sorted <- tail(weather_aggregated[order(weather_aggregated$Calc),])
aggregated_matrix <- t(sorted[-1])
colnames(aggregated_matrix) <- sorted[,1]
weather_aggregated_injuries <- setNames(aggregate(weather_case$INJURIES ~ EVTYPE, data = weather_case, sum, na.rm=TRUE),c("Type2", "Calc2"))
sorted_injuries <- tail(weather_aggregated_injuries[order(weather_aggregated_injuries$Calc2),])
aggregated_matrix_injuries <- t(sorted_injuries[-1])
colnames(aggregated_matrix_injuries) <- sorted_injuries[,1]
weather_aggregated_damage <- setNames(aggregate(weather_case$PROPDMG ~ EVTYPE, data = weather_case, sum, na.rm=TRUE),c("Type3", "Calc3"))
sorted_damage <- tail(weather_aggregated_damage[order(weather_aggregated_damage$Calc3),])
aggregated_matrix_damage <- t(sorted_damage[-1])
colnames(aggregated_matrix_damage) <- sorted_damage[,1]
sorted
sorted_injuries
sorted_damage
par(mfrow = c(1, 3))
par(cex = 0.55)
plot1 <- barplot(aggregated_matrix, las=2, main="Top 6 Natural Disasters by Total Number of Fatalities, 1950-2011", xlab = "Type", ylab = "Fatalities", cex=0.8, cex.axis=0.8, col="lightgrey", border="lightgrey")
plot2 <- barplot(aggregated_matrix_injuries, las=2, main="Top 6 Natural Disasters by Total Number of Injured, 1950-2011", xlab = "Type", ylab = "Injuries", cex=0.8, cex.axis=0.8, col="lightgrey", border="lightgrey")
plot3 <- barplot(aggregated_matrix_damage, las=2, main="Top 6 Natural Disasters by Property damage, 1950-2011", xlab = "Type", ylab = "USD", cex=0.8, cex.axis=0.8, col="lightgrey", border="lightgrey")