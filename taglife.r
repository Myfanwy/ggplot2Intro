
require(lubridate)
library(dplyr)
library(ggplot2)
require(reshape2)

# setwd("C:/Users/Myfanwy/Dropbox/Ch1/data_clean") PC
#setwd("~/Dropbox/Ch1/data_clean")
# Load Detection Data
d <- read.csv("Detections.csv", stringsAsFactors = F)
d <- tbl_df(d) # convert for easy viewing
d
cols <- c("DateTimeUTC" , "VR2" , "TagID") 
d <- d[ , 1:3] #pare down df
colnames(d) <- cols #rename column headers

class(d$DateTimeUTC)
d$DateTimeUTC <- as.POSIXct(strptime(d$DateTimeUTC, format ="%Y-%m-%d %H:%M:%S", tz="GMT")) #format must match order that data is currently in

d$DateTimeUTC <- ymd_hms(d$DateTimeUTC)  #convert to Lubridate format
d$DateTimePST <- with_tz(d$DateTimeUTC, "America/Los_Angeles")
d <- select(d, VR2:DateTimePST)

d <- d %>%
  arrange(DateTimePST) %>%
  filter(DateTimePST >= "2014-12-19 18:23:00") %>% #make sure all detections are within real range of experiment
  filter(DateTimePST <= "2015-01-13 16:15:00")
class(d$TagID)

tagparts <- colsplit(d$TagID, pattern = "\\-" , names = c('a' , 'b' , 'TagID'))
head(tagparts)
d2 <- tbl_df(data.frame(tagparts$TagID, d$VR2, d$DateTimePST))
head(d2)
colnames(d2) <- c("TagID" , "VR2" , "DateTimePST")

rm(d, tagparts, cols) # clean up workspace
d2

write.csv(d2, "detections_tidy.csv") #write tidy csv for ggplot2 slide processing

# Load tagIDS

tags <- read.csv("tagsheet.csv", header = T, stringsAsFactors = F)


# Load Temperature data
t <- tbl_df(read.csv("hobos_all.csv" , header = T, stringsAsFactors = F))
t$DateTimeGMT <- as.POSIXct(strptime(t$DateTimeGMT, format ="%m/%d/%y %H:%M", tz="GMT")) #format must match order that data is currently in
t$DateTimeGMT <- ymd_hms(t$DateTimeGMT) #convert to Lubridate format
t$DateTimePST <- with_tz(t$DateTimeGMT, "America/Los_Angeles")
t <- select(t, DateTimePST, Hobo, Temp, Max, Min)
t
# Summarize min/max temps per day
library(lubridate)
t2 <- t
t2$Date <- as.Date((strptime(t2$DateTimePST, format = "%Y-%m-%d")))

t_mm <- t2 %>%
  group_by(., Hobo) %>%
  filter(Max !="NA") %>%
  filter(DateTimePST >= "2014-12-18") %>% # this line doesn't work
  filter(DateTimePST <= "2015-01-13") %>% # this line works
  ungroup()

t_mm <- t_mm %>%
  select(Date, Hobo, Max, Min) %>%
  arrange(Date)
t_mm <- t_mm[7:78, ]

tail(t_mm)
means <- t2
  means <- summarise(group_by(means, Hobo, Date), Mean = mean(Temp, na.rm=T))
means

t_m3 <- left_join(t_mm, means) # make df with all temp data
t_m3
write.csv(t_m3, "hobos_tidy.csv") #write tidy csv for ggplot2 slide processing

#---------------------------------#

# Summarize number of detections per day, per tag
dets <- d2 # d2 = detections_tidy
dets$Date <- as.Date(dets$DateTimePST) # add date variable
dets$DayHr <- round_date(dets$DateTimePST, "hour")
Hrdets <- dets %>%
  group_by(TagID, DayHr) %>%
  summarise(., n=n()) %>%
  arrange(n)
Hrdets$TagID <- as.character(Hrdets$TagID)

dets_total <- dets %>%
  group_by(TagID) %>%
  summarise(., n=n())

dets_total <- arrange(dets_total, desc(n))
dets_total
write.csv(dets_total, "total_detections_tidy.csv") #write tidy csv for ggplot2 slide processing
summary(dets_total) # make a pretty table in rmd doc

## Summarize tag life:

dets <- d2
dets <- 
dets %>%
  arrange(TagID, DateTimePST) %>%
  group_by(TagID, id = cumsum(!duplicated(TagID) | c(F, round(diff(DateTimePST)/1440) > 1440))) %>%
             slice(c(1, length(DateTimePST))) %>%
             mutate(Departure = DateTimePST[2]) %>%
             slice(1) %>%
             ungroup
dets <- mutate(dets, Total = (Departure - DateTimePST))
class(dets$Total)

dets$LifeDays <- as.numeric(dets$Total/1440)
dets$LifeHrs <- as.numeric(dets$Total/60)
dets
write.csv(dets, "taglife_tidy.csv") # write tidy csv for ggplot2 slide processing

#--------------------------------#

## Start Plotting ##

# Slider plot for tag life:
# require(colourlovers)
# dets$TagID <- as.character(dets$TagID)
# clpalettes('top')
cols <- c("#FF4E50", "#FC913A", "#F9D423", "#EDE574", "#E1F5C4", "#EFFFCD", "#DCE9BE", "#555152", "#2E2633")

ggplot(dets, aes(x=reorder(TagID, LifeDays), y=LifeDays, width=0.5)) +
  geom_bar(stat="identity", aes(fill=cols)) + 
  labs(x="Tag", y="Days", title="Tag Life is Not Particularly Impressive") +
  scale_y_continuous(breaks=seq(0, 9.5, 0.5), limits=c(0, 9.5), expand=c(0,0)) +
                       coord_flip() + theme(legend.position="none")

# temperature data - verify that means are approximately the same
ggplot(t_m3, aes(x=Date, y=Mean)) + 
  geom_line(aes(color = Hobo)) +
  facet_wrap(~ Hobo, ncol=3, scales = "free_y") +
  ggtitle("Mean Temperature Across Tanks")

# temps: see if max/means are wildly out of synk (must not be, though)

ggplot(t_m3, aes(Date, c(Min))) +
  geom_line(aes(color = Hobo)) +
  facet_wrap(~Hobo, ncol=1, scales = "free_y") + ggtitle("Daily Minimum Temperature is Similar for All Three Tanks")

## Detections: illustrate how different each tag is:
Hrdets$TagID <- as.character(Hrdets$TagID)
# arrange Hrdets so that tagIDs are ranked by total number of detections
ranks <- 
  Hrdets %>%
    group_by(TagID) %>%
    summarise(., sums=sum(n)) %>%
    mutate(Rank = rank(sums)) %>%
    arrange(desc(Rank)) %>%
    ungroup()

Hrdets <- inner_join(Hrdets, ranks)
Hrdets <- ungroup(Hrdets)
Hrdets <- select(Hrdets, TagID, DayHr, n, Rank)
Hrdets <- arrange(Hrdets, -Rank)

r_table <- table(Hrdets$TagID) #index
r_levels <- names(r_table)[order(r_table)] #generate levels
Hrdets$TagID2 <- factor(Hrdets$TagID, levels = r_levels) #add column of levels in order

g <- ggplot(Hrdets, aes(x = TagID2, y=n)) + 
      geom_jitter(alpha = 0.5, aes(color = TagID2), position = position_jitter(width = .2)) + 
      guides(fill=FALSE) + labs(x="", y="Number of Detections", title="V5 Tag Detections Over Two Weeks") +
      geom_violin(alpha = 0.5, color="gray") + coord_flip() + theme(legend.position="none")
g

