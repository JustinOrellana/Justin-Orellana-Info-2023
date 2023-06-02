library(dplyr)
His <- read.csv("HistoricalEsportData.csv")
Gen <- read.csv("GeneralEsportData.csv")
MergeDF <- full_join(His, Gen, by = "Game")
MergeDF <- na.omit(MergeDF)
greater <- function(Games){
  gameFil <- filter(MergeDF, Game == Games)
  ifelse(gameFil$TotalEarnings >= 500000, TRUE, FALSE)
}

MergeDF$Greater_500000 <- greater(MergeDF$Game)
In_Person <- function(Games){
  gameFil <- filter(MergeDF, Game == Games)
  person <- (gameFil$TotalEarnings - gameFil$OnlineEarnings)
  return(person)
}
MergeDF$In_Person <- In_Person(MergeDF$Game)
Summarized <- data.frame(Date = MergeDF$Date,
                         Game = MergeDF$Game,
                         Genre = MergeDF$Genre,
                         Tournaments = MergeDF$TotalTournaments,
                         Players = MergeDF$TotalPlayers,
                         Earnings = MergeDF$TotalEarnings,
                         Above_Half_Million = MergeDF$Greater_500000)
Summarized <- na.omit(Summarized)
Summarized$Date <- format(as.Date(Summarized$Date, format="%Y-%m-%d"),"%Y")
write.csv(Summarized, "C:\\Users\\justi\\Documents\\GitHub\\Justin-Orellana-Info-2023\\Summarized.csv", row.names=TRUE)
write.csv(MergeDF, "C:\\Users\\justi\\Documents\\GitHub\\Justin-Orellana-Info-2023\\MergeDF.csv", row.names=TRUE)
