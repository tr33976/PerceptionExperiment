#helper library/funcs for console activation and clearing; also setting the location of the file as the working drive
rm(list=ls())
library("rstudioapi")
library(png)
library(ggplot2)
library(tictoc)
setwd(dirname(getActiveDocumentContext()$path))
clr = function(){cat("\014")}

#if a data file already exists load it and create a backup, if it doesn't exist we create a backup directory
# and create the data file
if(file.exists("test_data.csv")){
  testData = read.csv("test_data.csv")
  testData$stim =  factor(testData$stim, levels=c("active", "control", "passive"))
  testData$wait = factor(testData$wait, levels=c(30, 90, 150))
  testData$waitActual = as.numeric(testData$waitActual)
  write.csv(testData, paste("backups/", format(Sys.time(), '%Y-%m-%d%H%M%S'),"test_data.csv",sep=""), row.names = FALSE)
} else {
  dir.create("backups")
  columns = c("pid","stim","wait","waitActual", "indexdatetime") 
  testData = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(testData) = columns
  testData$stim =  factor(testData$stim, levels=c("active", "control", "passive"))
  testData$wait = factor(testData$wait, levels=c("30", "90", "150"))
  testData$waitActual = as.numeric(testData$waitActual)
  write.csv(testData, "test_data.csv", row.names = FALSE)
  write.csv(testData, paste("backups/",format(Sys.time(), '%Y-%m-%d%H%M%S'),"test_data.csv",sep = ""), row.names = FALSE)
}


#console loop to get current participant number input, will check for numeric
#and null input but nothing else so have to be careful
clr
participant = NA
while(is.na(participant)){
  clr()
  rstudioapi::executeCommand('activateConsole')
  participant =
    as.numeric(readline(prompt="Enter Participant ID and press [enter]"))
  clr()
}

#subset the test data to only the current participant
participantData = testData[testData$pid==participant,]
treatmentsCompleted = table(participantData$stim, participantData$wait)

#if no data yet then random sample, otherwise it random samples from the stimuli
#that are under represented
if(sum(treatmentsCompleted)==0){
  stim=sample(levels(testData$stim), 1)
  wait=as.numeric(sample(levels(testData$wait), 1))
} else {
  underSampledPairs=which(treatmentsCompleted==min(as.numeric(treatmentsCompleted)), arr.ind = TRUE)
  choice = underSampledPairs[sample(1:nrow(underSampledPairs),1),]
  stim=row.names(treatmentsCompleted)[choice[1]]
  wait=as.numeric(colnames(treatmentsCompleted)[choice[2]])
}


#experiment body
clr()
cat('You will be waiting', floor(wait/60), 'min &', wait%%60, 'secs')
cat('\n& be experiencing the', stim, 'stimulus')
switch(stim,
       control = {cat("\nPlease stare at the nearest blank wall")},
       active = {
         grid::grid.raster(readPNG("passage.png"))
         cat("\nPlese open link in plot window and read passages until time over")},
       passive = {
         grid::grid.raster(readPNG("news.png"))
         cat("\nPlese open link in plot window and watch the video until time over")})
rstudioapi::executeCommand('activateConsole')
readline(prompt="Press [enter] to begin wait period")
clr()
tic(msg = NULL, quiet = TRUE)
clr()
rstudioapi::executeCommand('activateConsole')
readline(prompt="Press [enter] to end wait period")
clr()
end.timer <- toc(log = FALSE, quiet = TRUE)
clr()
waitActual = as.numeric(end.timer$toc - end.timer$tic)
rm(participantData)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)

# #overwrite the test file and create yet another backup, yes im paranoid lol
testData[nrow(testData) + 1,] = c(participant, stim, as.numeric(wait), as.numeric(waitActual), Sys.time())
testData$waitActual = as.numeric(testData$waitActual)
write.csv(testData, "test_data.csv", row.names = FALSE)

rm(waitActual)
rm(end.timer)
cat("Complete, press source to run experiment again")
