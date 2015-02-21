# PeerAssess1
# StormData

setwd("J:/coursera/DataScience/RepResearch/RepData_PeerAssessment2")
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
g1y <- data.frame(year_group=groupyr,sd2)
max(g1y$year)

# -------------
# EVENTS -
# --------------
groupevt <- factor(sd2$EVTYPE)
gev <- data.frame(evt_group=groupevt,sd2)
gev_inj_sum <- summarise(group_by(gev,evt_group), injuries = sum(INJURIES))
gev_fat_sum <- summarise(group_by(gev,evt_group), fatalities = sum(FATALITIES))
gev_pd_sum  <- summarise(group_by(gev,evt_group), propdmg=sum(PROPDMG))

gev_inj_sum <- gev_inj_sum[order(gev_inj_sum$injuries, decreasing=T),]
gev_fat_sum <- gev_fat_sum[order(gev_fat_sum$fatalities, decreasing=T),]
gev_pd_sum <-  gev_pd_sum[order(gev_pd_sum$propdmg, decreasing=T),]

ia <- ia[order(ia$Year),]

topteninj <- gev_inj_sum[1:10,]
topteninj$evt_group <- factor(topteninj$evt_group) 
injlabels <- topteninj$evt_group

toptenfat <- gev_fat_sum[1:10,]

toptenpropdmg <- gev_pd_sum[1:10,]

# ------------------------
evinjbycat <- ggplot(topteninj, aes(y=injuries, x=evt_group )) + 
    geom_bar(stat="identity", fill="red",colour="black") +
    xlab("Event Type") +
    ylab("Injuries") +
    ggtitle("Injuries by Event Type") +
    theme_bw()  +
    theme( axis.text.x = element_text(angle=60, hjust=1),
    panel.grid.major.y= element_line(color="grey60", linetype="solid"),
    panel.grid.minor.y= element_line(color="grey60", linetype="dashed"),
    panel.grid.major.x= element_line(),
    plot.title=element_text(size=rel(2)),
    axis.title.x  = element_text(angle=0, vjust=0.5, size=rel(1.5)),
    axis.title.y  = element_text(angle=90, vjust=0.5, size=rel(1.5))
    )

evinjbycat            

# ---------------------

evfatbycat <- ggplot(toptenfat, aes(y=fatalities, x=evt_group )) + 
    geom_bar(stat="identity", fill="blue",colour="black") +
    scale_fill_brewer(palette = "Blues") +
    xlab("Event Type") +
    ylab("Injuries") +
    ggtitle("Fatalities by Event Type") +
    theme_bw()  +
    theme( axis.text.x = element_text(angle=60, hjust=1, size=rel(1)),
           panel.grid.major.y= element_line(color="grey60", linetype="solid"),
           panel.grid.minor.y= element_line(color="grey60", linetype="dashed"),
           panel.grid.major.x= element_line(),
           plot.title=element_text(size=rel(2)),
           axis.title.x  = element_text(angle=0, vjust=0.5, size=rel(1.5)),
           axis.title.y  = element_text(angle=90, vjust=0.5, size=rel(1.5))
           #plot.xlab =element_text(size=rel(1.5))
           )

evfatbycat 
          

# ----------------------------------------------


evpdbycat <- ggplot(toptenpropdmg, aes(y=propdmg/1000, x=evt_group )) + 
    geom_bar(stat="identity", fill="blue",colour="black") +
    scale_fill_brewer(palette = "Blues") +
    xlab("Event Type") +
    ylab("Damage in Billions $$$") +
    ggtitle("Property Damge by Event Type in Dollars") +
    theme_bw()  +
    theme( axis.text.x = element_text(angle=60, hjust=1, size=rel(1)),
           panel.grid.major.y= element_line(color="grey60", linetype="solid"),
           panel.grid.minor.y= element_line(color="grey60", linetype="dashed"),
           panel.grid.major.x= element_line(),
           plot.title=element_text(size=rel(2)),
           axis.title.x  = element_text(angle=0, vjust=0.5, hjust=0.5, size=rel(1.5)),
           axis.title.y  = element_text(angle=90, hjust=0.6, vjust=0.5, size=rel(1.5))
           #plot.xlab =element_text(size=rel(1.5))
    )

evpdbycat 



# -----------------------------------------------






sum_fat_yr <- summarise(group_by(g1y,year_group), fatalities = sum(FATALITIES))
sum_inj_yr <- summarise(group_by(g1y,year_group), injuries = sum(INJURIES))
sum_propdmg_yr <- summarise(group_by(g1y,year_group), propdmg = sum(PROPDMG))

yrlabels = as.character(seq(1950,2011, by = 5))
yrlabels2 = as.character(seq(1,62, by = 1))

yrs = seq(1950,2011, by = 5)

ia <- read.csv("InflationAve.csv", header=T,sep=",", stringsAsFactors = F)

corrfactor <- ia[1,2]
ia$AveCorrFactor <- corrfactor / ia$Ave. 
names(ia)[2] <- "Avg"

# Order CPI by year Ascending
ia <- ia[order(ia$Year),]
plot(ia$Avg ~ia$Year, main="Consumer Price Index vs Year")

spdyr <- sum_propdmg_yr
#merged <- merge(myIncomplete, spit, by.x="interval", by.y="group", 
 
mergedpd <- merge(spdyr, ia, by.x="year_group", by.y="Year")

mergedpd$pd_cpi <- mergedpd$propdmg *  mergedpd$AveCorrFactor
    

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

evtable <- data.frame(table(sd2$EVTYP, sd2$year))
head (evtable, 20)



library(ggplot2)

# -----------------------------
# Storm Damge $$$ GGPLOT2

pd <- ggplot(mergedpd, aes(y=propdmg  / 1000, x=year_group, group=1))
pd <- pd + geom_line(data=mergedpd, aes(x=year_group, y=pd_cpi/1000, group=1), col="green")
pd  + geom_point() + stat_smooth(method = "loess") +
    labs(title = "Storm Damage per Year -- Dollars") +
    labs(x = "Year", y = "Injuries") +
    scale_x_discrete(breaks=seq(1950, 2011, 5))
# ------------------------------------------

# FATALITIES per Year
pf <- ggplot(sum_fat_yr, aes(y=fatalities, x=year_group, group=1))
pf  + geom_point() + stat_smooth(method = "lm") +
    labs(title = "Storm Fatalities per Year") +
    labs(x = "Year", y = "Fatalities") +
    scale_x_discrete(breaks=seq(1950, 2011, 5))

# Injuries per year
# -----------------
pi <- pi <- ggplot(sum_inj_yr, aes(y=injuries, x=year_group, group=1))
pi  + geom_point() + stat_smooth(method = "lm") +
    labs(title = "Storm Injuries per Year") +
    labs(x = "Year", y = "Injuries") +
    scale_x_discrete(breaks=seq(1950, 2011, 5))

# --------------------------------------------------
