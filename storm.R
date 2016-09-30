#Course 5/Week 4 - Assignment
#
##   storm.R
##
## September, 2016
## Julie Foster
#

##Title: Your document should have a title that briefly 
##summarizes your data analysis
##Synopsis: Immediately after the title, there should be a 
##synopsis which describes and 
##summarizes your analysis in at most 10 complete sentences.

###Data Processing
# describes (in words and code) how the data were loaded 
# into R and processed for analysis. In particular, your 
# analysis must start from the raw CSV file containing the 
# data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider using the cache = TRUE option for certain code chunks.

##  Loading and preprocessing the data - cache=TRUE

storm <- read.csv("repdata%2Fdata%2FStormData.csv.bz2", header = TRUE, sep = ",")

##  Getting to understand the dataset
str(storm)
names(storm)
head(storm)
summary(storm)


library(dplyr)

## Data Analysis

## Q1.  Across the United States, which types of events 
# (as indicated in the EVTYPE variable) are most harmful 
# with respect to population health?

# 1. Reduce data set to just evtype, fatalities, injuries

subStorm <- select(storm, EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG)
head(subStorm)
str(subStorm)
events <- table(subStorm$EVTYPE)
str(events)

# check for NAs

sum(is.na(subStorm))

# 2. Clean up rows:  The Storm Data Documentation 
# (https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
# lists 48 possible event types.  The levels(subStorm$EVTYPE) shows the 
# EVTYPE column with 985 types and needs to be cleaned up.

# Making all EVTYPE lower case:
# Aggregate the data and subsetting only records 
# with positive values for population health threat or 
# economic loss.


subStorm$EVTYPE = factor(tolower(subStorm$EVTYPE))
subStorm <- aggregate(cbind(FATALITIES,INJURIES,PROPDMG,CROPDMG)~EVTYPE,subStorm,sum)
subStorm <- subStorm[subStorm$FATALITIES>0 | 
                         subStorm$INJURIES>0 | 
                         subStorm$PROPDMG>0 | 
                         subStorm$CROPDMG>0,]

events <- table(subStorm$EVTYPE)
unique(subStorm$EVTYPE[1:20])
events

# add new column "total"
harm <- select(subStorm, EVTYPE, FATALITIES, INJURIES)
harm <- harm[harm$FATALITIES>0 | harm$INJURIES>0, ]
   
harm <- mutate(harm, total=FATALITIES + INJURIES)
harm <- arrange(harm, desc(total))
harm10 <- harm[1:10, ]

str(harm10)


# 4. plot of evtype vs fatalities, evtype vs injuries

par(mfrow=c(1,2), oma=c(0,0,1,0))

barplot(harm10$FATALITIES, names.arg=harm10$EVTYPE, las=2, cex.axis=0.7,
        cex.names=0.8,col="red",
        ylab="Number of Fatalities")
barplot(harm10$INJURIES, names.arg=harm10$EVTYPE, las=2, cex.axis=0.7,
        cex.names=0.8,col="steelblue",
        ylab="Number of Injuries")
title("Population Impact by Storm Event",outer=TRUE)

## Q2. Across the United States, which types of events 
## have the greatest economic consequences?

econ <- select(subStorm, EVTYPE, PROPDMG, CROPDMG)
econ <- econ[econ$PROPDMG>0 | econ$CROPDMG>0, ]

econ <- mutate(econ, total=PROPDMG + CROPDMG)
econ <- arrange(econ, desc(total))
econ10 <- econ[1:10, ]

str(econ10)

par(mfrow=c(1,2), oma=c(0,0,1,0))

barplot(econ10$PROPDMG, names.arg=econ10$EVTYPE, las=2, cex.axis=0.7,
        cex.names=0.8,col="sienna4",
        ylab="Cost of Property Damage")
barplot(econ10$CROPDMG, names.arg=econ10$EVTYPE, las=2, cex.axis=0.7,
        cex.names=0.8,col="darkgreen",
        ylab="Cost of Crop Damage")
title("Economic Impact by Storm Event",outer=TRUE)
### Results
