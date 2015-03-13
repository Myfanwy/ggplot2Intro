## ggplot demo ##
library(dplyr)
library(lubridate)
library(ggplot2)

# set path/relative paths
setwd("C:/Users/Myfanwy/Dropbox/GitHubRepos/ggplot2Intro/data") #PC


# Load data

temp <- tbl_df(read.csv("hobos_tidy.csv", stringsAsFactors = F, header = T))
temp$Date <- ymd(temp$Date) # important if you're going to put Date on an axis as continous variable
temp$Max
temp <- filter(temp, Max < 20) # get rid of outliers

d <- tbl_df(read.csv("detections_tidy.csv" , stringsAsFactors = F, header = T))
d <- d[,-1] # get rid of weird X column

td <- tbl_df(read.csv("total_detections_tidy.csv", header = T))
td <- td[,-1] 
td

tl <- tbl_df(read.csv("taglife_tidy.csv", stringsAsFactors = F, header = T))
tl <- tl[ , -1]

cols <- c("#FF4E50", "#FC913A", "#F9D423", "#EDE574", "#E1F5C4", "#EFFFCD", "#DCE9BE", "#555152", "#2E2633")

# Plot 1:  All Detections
dets <- d # d2 = detections_tidy
dets$Date <- as.Date(dets$DateTimePST) # add date variable
dets$DateTimePST <- ymd_hms(dets$DateTimePST)
dets$DayHr <- round_date(dets$DateTimePST, "hour")
dets

Hrdets <- dets %>% # creates a table of number of detections by dayHr
  group_by(TagID, DayHr) %>%
  summarise(., N=n()) %>%
  arrange(., N) %>%
  ungroup()

g <- ggplot(Hrdets, aes(x = DayHr, y = N)) + geom_point()
g
g + geom_point(aes(color = TagID, shape = TagID, size = 1.5)) # ugly plot, but helps me figure things out

# Violin/Jitter Plot
g <- ggplot(Hrdets, aes(x = TagID, y=N)) + 
  geom_jitter(alpha = 0.5, aes(color = TagID), position = position_jitter(width = .2)) + 
  guides(fill=FALSE) + labs(x="", y="Number of Detections", title="V5 Tag Detections Over Two Weeks") +
  geom_violin(alpha = 0.5, color="gray") + coord_flip() + theme(legend.position="none")
g

# Plot 2: Average Daily Temperature Across Tanks

p <- ggplot(temp, aes(x = Date, y = Mean, color= Hobo))
p
p <- p + geom_line()
p
p + ggtitle("Average Daily Temperature Was Similar Across Tanks")

# Make lines thicker, different type
p + geom_line(alpha = 0.5, linetype = 6, size = 1.5)

# Make different kind of graph to show the same data: ribbon plot

p <- ggplot(temp, aes(x = Date, y=Mean))
p <- p + geom_ribbon(aes(ymin=Min, ymax = Max, fill = Hobo, alpha = 0.5))
p + stat_summary(geom = "ribbon", fun.data = "median_hilow")
=============================================================
# # Plot 3: Detections vs. Temperature
# 
# Hrdets
# class(Hrdets$DayHr)
# Hrdets$Date <- as.Date(Hrdets$DayHr)
# temp
# a <- filter(temp, Hobo=="13a") # just get the experimental tank
# a$Date <- as.Date(a$Date)
# r <- left_join(a, Hrdets)
# r
# ggplot(r, aes(x = Mean, y = N)) + geom_point()
==============================================================
  
# Plot 3: Total Number of Detections Per Tag
td
g <- ggplot(td, aes(x = TagID, y  = n)) + geom_bar(stat = "identity")
g
g + scale_y_log10() # these data are best suited for a table, not a graph

# to change the order, have to set factor levels to be in the right order from the getgo, or map
# to a re-ordered x-axis value (need something else in the dataframe with which to order it, though:

g <- ggplot(td, aes(x = reorder(TagID,n), y  = n)) + geom_bar(aes(fill = cols, alpha = 0.5) , stat = "identity")
g

# How to get rid of legend:
g + theme(legend.position = "none")


# Plot 4: putting it all together to get a sense of tag life
tl
ggplot(tl, aes(x=reorder(TagID, LifeDays), y=LifeDays, width=0.5)) + # data, mapping to aes
  
    geom_bar(stat="identity", aes(fill=cols, alpha = 0.5)) + # geom
  
        scale_y_continuous(breaks=seq(0, 9.5, 0.5), limits=c(0, 9.5), expand=c(0,0)) + # scale
  
            coord_flip() + theme(legend.position="none") + # coord
  
              labs(x="Tag", y="Days", title="Tag Life is Not Particularly Impressive 
                After A Year on the Shelf") #labels to polish


##