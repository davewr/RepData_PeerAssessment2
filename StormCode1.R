# PeerAssess1
# Smartphones
setwd("J:/coursera/DataScience/RepResearch/RepData_PeerAssessment2")
setwd=("~")
getwd()

library("lubridate")
library(dplyr)


sdURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile = "repdata_data_StormData.csv.bz2"
download.file(sdURL, destFile, mode="wb")


# This works
sd <- read.table(destFile, header=T,sep=",", stringsAsFactors = F)
sd2 <-sd[,c(1:31,37)]

# Setup year and month factors...
sd2$BGN_DATE <- mdy_hms(sd2$BGN_DATE) #, "%m/%d/%Y %h:%M:%S", tz=sd2$TIME_ZONE)
sd2$year <- year(sd2$BGN_DATE)
sd2$month <- month(sd2$BGN_DATE)

g1y <- sd2
groupyr <- factor(sd2$year)
g1y <- data.frame(group=groupyr,sd2)
max(g1y$year)

# -------------
groupevt <- factor(sd2$EVTYPE)
gev <- data.frame(group=groupevt,g1y)

sum_fat_yr <- summarise(group_by(g1y,group), fatalities = sum(FATALITIES))
sum_inj_yr <- summarise(group_by(g1y,group), injuries = sum(INJURIES))
sum_propdmg_yr <- summarise(group_by(g1y,group), propdmg = sum(PROPDMG))

yrlabels = as.character(seq(1950,2011, by = 5))
yrlabels2 = as.character(seq(1,62, by = 1))

yrs = seq(1950,2011, by = 5)

plot(sum_fat_yr$fatalities, sum_fat_yr$year,
    main = "Fatalities per Year",
    axes=F, pch=9,
    xlab = "Year", ylab = "Fatalities")
    axis(2)
    axis(1, at= seq(1,62, by=5), labels=yrlabels)
    box()

plot(sum_inj_yr$injuries, sum_inj_yr$year, axes=F,
     main="Injuries per year",
     xlab = "Year", ylab = "Number of Injuries")
     
    axis(1, at= seq(1,62, by=5), labels=yrlabels)
    axis(2)
    box()

par(pch=01)
plot(sum_propdmg_yr$propdmg / 1000, sum_propdmg_yr$year,  main="Property Damage by Year",
    axes=F, pch=9,
    xlab = "Year", ylab = "$$ in Billions")
    axis(2)
    axis(1, at= seq(1,62, by=5), labels=yrlabels)
    box()



boxplot(sum_fat_yr$fatalities, main="Fatalities")
boxplot(sum_inj_yr$injuries, main="Injuries")
boxplot(sum_propdmg_yr$propdmg, main="Property Damage")

ia <- read.csv("InflationAve.csv", header=T,sep=",", stringsAsFactors = F)

corrfactor <- ia[1,2]
ia$Ave <- corrfactor / ia$Ave. 



# -----------------

# ---------------------------
# spdtcm <- summarise(group_by(g1m, group), spd = sum(steps))
# ---------------------------


ue <- unique(sd$EVTYPE)
tail(sd2)

maxpd <- max(sd2$PROPDMG)
maxinj <- max(sd2$INJURIES)
maxfatal <- max(sd2$FATALITIES)

x1 <- sd2[sd2$PROPDMG >= maxpd-1,]
x2 <- sd2[sd2$INJURIES >= maxinj-1,]
x3 <- sd2[sd2$FATALITIES >= maxfatal-1,]

boxplot(g1y$INJURIES)
boxplot(sd2$FATALITIES)
boxplot(sd2$PROPDMG)
#rm("InflationDataB")
# rm("sum_yr")
