# Tester...

library("lubridate")
library("dplyr")
library("ggplot2")
library("gridExtra")


sdURL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile = "repdata_data_StormData.csv.bz2"
# Remove download for testing only...
#download.file(sdURL, destFile, mode="wb")


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
# max(g1y$year)

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


topteninj <- gev_inj_sum[1:10,]
topteninj$evt_group <- factor(topteninj$evt_group) 
injlabels <- topteninj$evt_group

toptenfat <- gev_fat_sum[1:10,]

toptenpropdmg <- gev_pd_sum[1:10,]


sum_propdmg_yr <- summarise(group_by(g1y,year_group), propdmg = sum(PROPDMG))




spdyr <- sum_propdmg_yr



# ------------------------

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
    ylab("Fatalities") +
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



require(gridExtra)
# -----------------------------
# Storm Damge $$$ GGPLOT2

pd <- ggplot(spdyr, aes(y=propdmg  / 1000, x=year_group, group=1))
pd <- pd+ geom_point() + stat_smooth(method = "loess") +
    labs(title = "Storm Damage per Year -- Dollars") +
    labs(x = "Year", y = "Injuries") +
    scale_x_discrete(breaks=seq(1950, 2011, 5)
    )
# pd
# ------------------------------------------

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

#evpdbycat 

grid.border

grid.arrange(pd, evpdbycat , nrow=2, newpage=TRUE)



 # *************************************

