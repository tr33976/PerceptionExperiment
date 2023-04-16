#helper library/funcs for console activation and clearing; also setting the location of the file as the working drive
rm(list=ls())
library("rstudioapi")
library(png)
library(ggplot2)
setwd(dirname(getActiveDocumentContext()$path))
clr = function(){cat("\014")}

#if a data file already exists load it and create a backup, if it doesn't exist we create a backup directory
# and create the data file
if(file.exists("test_data.csv")){
  testData = read.csv("test_data.csv")
  testData$stim =  factor(testData$stim, levels=c("active", "control", "passive"))
  testData$timeDay =  factor(testData$timeDay, levels=c("m", "d", "n"))
  testData$wait = as.numeric(testData$wait)
  testData$waitActual = as.numeric(testData$waitActual)
  write.csv(testData, paste("backups/",format(Sys.time(), '%Y-%m-%d%H%M%S'),"test_data.csv",sep = ""), row.names = FALSE)
} else {
  dir.create("backups")
  columns = c("pid","stim","wait","waitActual", "timeDay", "indexdatetime") 
  testData = data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(testData) = columns
  testData$stim =  factor(testData$stim, levels=c("active", "control", "passive"))
  testData$timeDay =  factor(testData$timeDay, levels=c("m", "d", "n"))
  testData$wait = as.numeric(testData$wait)
  testData$waitActual = as.numeric(testData$waitActual)
  write.csv(testData, "test_data.csv", row.names = FALSE)
  write.csv(testData, paste("backups/",format(Sys.time(), '%Y-%m-%d%H%M%S'),"test_data.csv",sep = ""), row.names = FALSE)
}


clr
participant = NA
tod = ""

#console loop to get current participant number input, will check for numeric
#and null input but nothing else so have to be careful
while(is.na(participant)){
  clr()
  rstudioapi::executeCommand('activateConsole')
  participant =
    as.numeric(readline(prompt="Enter Participant ID and press [enter]"))
  clr()
}
#time of day, wont proceed until d m or n is entered
while(tod=="" | !tod %in% c("d", "m", "n")) {
  clr()
  rstudioapi::executeCommand('activateConsole')
  tod =
    readline(prompt="Enter time of day as morning=m day=d and night=n")
  clr()
}

#subset the test data to only the current participant
participantData = testData[testData$pid==participant,]
participantStims = table(participantData$stim)

#if no data yet then random sample, otherwise it random samples from the stimuli
#that are under represented
if(sum(participantStims)==0 | length(unique(as.numeric(participantStims)))==1){
  stim=sample(levels(testData$stim), 1)
} else {
  stim=sample(names(participantStims[participantStims==min(as.numeric(participantStims))]), 1)
}

# generates minute sample weights to ensure balancing of randomised times
# if not seen all stimulus yet then times are random, after which
# it will identify either those minutes which havent been seen and randomized 
# between them or just inverse sample those which have been seen least/most/
if(length(unique(participantData$stim)) < 3){
  minWeights = rep(1, 5)
  cat('here')
} else {
  stimTable = table(participantData$stim, factor(floor(participantData$wait / 60), levels=c(0:4)))
  stimTable = stimTable[levels(testData$stim)==stim,]
  stimCounts = as.numeric(stimTable)
  seenMin = setdiff(0:4, as.numeric(names(stimTable[stimTable==0])))
  minWeights = rep(0,5)
  
  if(0 %in% stimCounts){
    for (i in 0:length(minWeights)-1){
      if(i %in% seenMin){
        minWeights[i+1]=0
      } else {
        minWeights[i+1]=1
      }
    }  
  } else {
    minWeights = 1/stimCounts
  }
}


#experiment body
clr()
minWeights[1] = minWeights[1]/2
waitMin = sample(0:4,size=1, prob=minWeights)
if(waitMin == 0){
  waitSec = sample(30:59,1)
} else
{
  waitSec = sample(0:59,1)
}
waitTime = waitMin*60 + waitSec
clr()
cat('You will be waiting', floor(waitTime/60), 'min &', waitTime%%60, 'secs')
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
start.timer <- Sys.time()
clr()
rstudioapi::executeCommand('activateConsole')
readline(prompt="Press [enter] to end wait period")
clr()
end.timer <- Sys.time()
clr()
actualTime = as.numeric(end.timer - start.timer)
rm(participantData)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)

#overwrite the test file
testData[nrow(testData) + 1,] = c(participant, stim, as.numeric(waitTime), as.numeric(actualTime), tod, Sys.time())
testData$wait = as.numeric(testData$wait)
testData$waitActual = as.numeric(testData$waitActual)
write.csv(testData, "test_data.csv", row.names = FALSE)

cat("Complete, press source to run experiment again")


