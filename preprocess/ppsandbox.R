charVect <- readLines(paste0(ddir, '../tests/sentenceAnnealTest.txt'))

twtpath <- 'C:/data/Dropbox/sw_dev/projects/PredictNextKBO/twitter_timings.csv'

twtdata <- read.csv(twtpath, colClasses=c('numeric', 'POSIXct'))

cum.time <- append(0, cumsum(as.numeric(diff(twtdata$time), units='mins')))
twtdata$cumTime <- cum.time

logLinearRegion <- 24:109
ln.cum.time <- log(cum.time[logLinearRegion])
ln.per.comp <- log(twtdata$percent.complete[logLinearRegion])
plot(ln.per.comp, ln.cum.time)

twitModel <- lm(ln.cum.time ~ ln.per.comp)
names(twitModel$coefficients) <- c('intercept', 'slope')

getDeltaMins <- function(twitMod=twitModel, twitPer) {
    twitTime <- exp(twitMod$coefficients['intercept'] +
                    (twitMod$coefficients['slope'] * log(twitPer)))
    return(twitTime)
}

getPredTime <- function(twitMod=twitModel, twitPer,
                        startTime='2016-06-16 04:29:55',
                        startPerComp=12.71107) {
    require(lubridate)
    deltaMins <- getDeltaMins(twitModel, twitPer)
    deltaMinsBeyondBase <- deltaMins - startPerComp
    ref.time <- as.POSIXct(startTime)
    return(ref.time + minutes(ceiling(deltaMinsBeyondBase)))
}

# test prediction
getPredTime(twitMod=TwitModel, twitPer=64.61461)

# Need to pretend data starts at the 24th point on
modStartTime <- as.POSIXct('2016-06-16 04:29:55')
modStartPerc <- 12.7110700

# min.time <- as.POSIXct('2016-06-16 04:00:00')
# max.time <- as.POSIXct('2016-06-16 23:00:00')
# plot(twtdata$percent.complete, twtdata$time, xlim=c(0, 100), ylim=c(min.time, max.time))

plot(twtdata$percent.complete, twtdata$cumTime, xlim=c(0, 100), ylim=c(0, 1500))

