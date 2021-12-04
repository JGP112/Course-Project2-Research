---
  title: "R Notebook"
output: html_notebook
---
  
  ##TImpact of Weather Events on Personal and Property Damage Using NOAA Storm Database
  #Synopsis
  Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This study explores the criticality of different events based on three aforementioned parameters viz., fatalities, injuries and economic damage. It identifies the top six critical events across each parameter and compares with the others.

The analysis identifies Tornado to be the most destructive on a personal level as it causes the highest number of fatalities and injuries. It also shows that Floods are the major cause for property damage.

Data processing
1. Downloading and reading the data
The data for the analysis was downloaded from the NOAA storm database. Documentation of data is also shared below.

Dataset : Storm Data
Documentation : National Weather Service Storm Data Documentation
The data is available in a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. After the data is downloaded from the website, it is uncompressed and read into R environment

# Dependencies
```{r}
library(dplyr)
library(tidyr)
library(ggplot2)

```



# data

```{r}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              destfile = "repdata_data_StormData.csv.bz2")
```

# Reading data back

```{r}
StormData <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))
```


2. Cleaning the data

Key variables used for the analysis are:
  
  EVTYPE : Type of the event
FATALITIES : Number of fatalities from the event
INJURIES : Nunber of injuries from the event
PROPDMG : Property damage measured
CROPDMG : Crop damage measured
PROPDMGEXP : Property damage exponent (Mns, Bns etc)
CROPDMGEXP : Crop damage exponent (Mns, Bns etc)

Last two variables mentioned above do not have clean data as shown below.

```{r}
unique(StormData$PROPDMGEXP)

unique(StormData$CROPDMGEXP)
```



```{r}
StormData$PROPDMGEXP <- toupper(StormData$PROPDMGEXP)
StormData$PROPDMGEXP[StormData$PROPDMGEXP %in% c("", "+", "-", "?")] <- "0"
StormData$PROPDMGEXP[StormData$PROPDMGEXP %in% c("B")] <- "9"
StormData$PROPDMGEXP[StormData$PROPDMGEXP %in% c("M")] <- "6"
StormData$PROPDMGEXP[StormData$PROPDMGEXP %in% c("K")] <- "3"
StormData$PROPDMGEXP[StormData$PROPDMGEXP %in% c("H")] <- "2"

StormData$CROPDMGEXP <- toupper(StormData$CROPDMGEXP)
StormData$CROPDMGEXP[StormData$CROPDMGEXP %in% c("", "+", "-", "?")] <- "0"
StormData$CROPDMGEXP[StormData$CROPDMGEXP %in% c("B")] <- "9"
StormData$CROPDMGEXP[StormData$CROPDMGEXP %in% c("M")] <- "6"
StormData$CROPDMGEXP[StormData$CROPDMGEXP %in% c("K")] <- "3"
StormData$CROPDMGEXP[StormData$CROPDMGEXP %in% c("H")] <- "2"

```

3. Creating new variables
Final values of property damage, crop damage and total damage are calculated with the help of clean data

```{r}
StormData$PROPDMGTOTAL <- StormData$PROPDMG * (10 ^ as.numeric(StormData$PROPDMGEXP))
StormData$CROPDMGTOTAL <- StormData$CROPDMG * (10 ^ as.numeric(StormData$CROPDMGEXP))
StormData$DMGTOTAL <- StormData$PROPDMGTOTAL + StormData$CROPDMGTOTAL
```


Analyzing critical weather events
Total fatalities, injuries and economic damage are summarized for each weather event across the considered time frame. Few records of the summarized data is shown below.

```{r}
SummStormData <- StormData %>%
  group_by(EVTYPE) %>%
  summarize(SUMFATALITIES = sum(FATALITIES),
            SUMINJURIES = sum(INJURIES),
            SUMPROPDMG = sum(PROPDMGTOTAL),
            SUMCROPDMG = sum(CROPDMGTOTAL),
            TOTALDMG = sum(DMGTOTAL))
```

```{r}
head(SummStormData)

```


1. Identifying events that caused most fatalities
Summarized data is arranged in descending order of fatalities and top six events are retained

```{r}
SummStormDataFatality <- arrange(SummStormData, desc(SUMFATALITIES))
FatalityData <- head(SummStormDataFatality)
```


2. Identifying events that caused most injuries
Summarized data is arranged in descending order of injuries and top six events are retained

```{r}
SummStormDataInjury <- arrange(SummStormData, desc(SUMINJURIES))
InjuryData <- head(SummStormDataInjury)
```


3. Identifying events that caused most damage
Summarized data is arranged in descending order of damage and top six events are retained

```{r}
SummStormDataDamage <- arrange(SummStormData, desc(TOTALDMG))
DamageData <- head(SummStormDataDamage)
```


Results
1. Fatalities
Top fatalities causing events are plotted for better understanding of the magnitude

```{r}

FatalityData$EVTYPE <- with(FatalityData, reorder(EVTYPE, -SUMFATALITIES))
ggplot(FatalityData, aes(EVTYPE, SUMFATALITIES, label = SUMFATALITIES)) +
  geom_bar(stat = "identity") +
  geom_text(nudge_y = 200) +
  xlab("Event Type") +
  ylab("Total Fatalities") +
  ggtitle("Most Fatal Events") +
  theme(plot.title = element_text(hjust = 0.5))

```


It is observed that Tornado causes the most number of fatalities followed by Excessive Heat and Flash Floods.

2. Injuries
Top injuries causing events are plotted for better understanding of the magnitude

```{r}
InjuryData$EVTYPE <- with(InjuryData, reorder(EVTYPE, -SUMINJURIES))
ggplot(InjuryData, aes(EVTYPE, SUMINJURIES, label = SUMINJURIES)) +
  geom_bar(stat = "identity") +
  geom_text(nudge_y = 3000) +
  xlab("Event Type") +
  ylab("Total Injuries") +
  ggtitle("Most Injury Events") +
  theme(plot.title = element_text(hjust = 0.5))
```


Here too Tornado is the leading cause for injuries by a huge margin.

3. Damage
The data contains two types of damage viz, property and crop. The data is tidied to aid in comparing the property and crop damage. Top damage (property + crop) causing events are plotted for better understanding of the magnitude

# Tidying data for plot stacked by type of damage

```{r}
DamageData$EVTYPE <- with(DamageData, reorder(EVTYPE, -TOTALDMG))
DamageDataLong <- DamageData %>%
  gather(key = "Type", value = "TOTALDAMAGE", c("SUMPROPDMG", "SUMCROPDMG")) %>%
  select(EVTYPE, Type, TOTALDAMAGE)
DamageDataLong$Type[DamageDataLong$Type %in% c("SUMPROPDMG")] <- "Property damage"
DamageDataLong$Type[DamageDataLong$Type %in% c("SUMCROPDMG")] <- "Crop damage"
```

# Plot

```{r}
ggplot(DamageDataLong, aes(x = EVTYPE, y = TOTALDAMAGE, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Event Type") +
  ylab("Total Damage") +
  ggtitle("Events with Most Damage") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

```


Floods cause the highest damage to property and crop combined, followed by hurricane, typhoon and tornado.

Conclusion
In all three parameters, it is observed that Tornado and Floods have been the major reasons for fatalities, injuries and economic damage. A better calamity prediction for these events would help saving lives and reducing property damage.