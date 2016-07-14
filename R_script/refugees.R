library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(ggthemes)

### initialize folder

home <- "/Users/xing"
project <- paste0(home, "/Desktop/Clean_R/Data")

dat1 <- read.csv(paste0(project, "/refugees.csv"), stringsAsFactors = F)

##### Tasks #######
# rename columns
names(dat1) <- c("country", "2011", "2012", "2013", "2014", "2015", "2015_Jan-Mar", "2016_Jan-Mar", "percent_change")

# delete last column and column is NA
dat1$X.8 <- NULL
dat1[10] <- NULL

# delete unecessary rows
dat1 <- dat1[-c(1:2),]
dat1 <- dat1[-c(14:nrow(dat1)),]

# change names of countries to China and Somalia
dat1$country <- ifelse(dat1$country == "China People's Republic of", "China", dat1$country)
dat1$country <- ifelse(dat1$country == "Somalia Democratic Republic of", "Somalia", dat1$country)

# remove % sign after number is %_change column and commas
trim <- function(x) gsub("%", "", x)
dat1$percent_change <- trim(dat1$percent_change)

trim_com <- function(x) gsub(",", "", x)

for (i in names(dat1)) {
    dat1[,i] <- trim_com(dat1[,i])
}

# change year column data into numeric
for (i in 2:9) {
    dat1[,i] <- as.numeric(dat1[,i])
}

# make line graph showing number of refugees by country over time 

mc <- melt(dat1, id.vars = "country")

#remove rows associated with 2015_Jan-Mar, 2016_Jan-Mar, percent_change & total and total top 10
mc <- mc[-c(66:nrow(mc)),]
mc <- subset(mc, country!="Total")
mc <- subset(mc, country!="Total Top 10")
mc <- subset(mc, country!="Other*")

ggplot(mc, aes(variable, value, group = country, col = country)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Number of Claims") +
  ggtitle("Number of Refugee Status Claims at Immigration Visa Offices in Canada") +
  scale_color_discrete(name = "Country of Origin") 

ggplot(mc, aes(variable, value, fill = country)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Number of Claims") +
  ggtitle("Number of Refugee Status Claims at Immigration Visa Offices in Canada") +
  scale_fill_brewer(palette = "Spectral") +
  scale_fill_discrete(name = "Country of Origin") 
