library(dplyr)
library(ggplot2)
library(tidyr) 
library (reshape2)
library (ggthemes)

## initialize folder

home <- "/Users/xing"
project <- paste0(home, "/Desktop/canada_R_code/Data")

fcr14 <- read.csv(paste0(project, "/2014_Fuel_Consumption_Ratings.csv"), stringsAsFactors = F)
fcr15 <- read.csv(paste0(project, "/2015_Fuel_Consumption_Ratings.csv"), stringsAsFactors = F)
fcr16 <- read.csv(paste0(project, "/2016_Fuel_Consumption_Ratings.csv"), stringsAsFactors = F)

# clean up dataframes
fcr14$X.3 <- NULL
fcr16$X.3 <- NULL
fcr16$CO2 <- NULL

# rename columns
names(fcr14) <- c("year", "make", "model", "class", "engine_size", "cylinders", "transmission",
                  "fuel_type", "fuel_con_city", "fuel_con_hwy", "fuel_con_comb", "fuel_con_mpg","co2_emissions")

names(fcr15) <- c("year", "make", "model", "class", "engine_size", "cylinders", "transmission",
                  "fuel_type", "fuel_con_city", "fuel_con_hwy", "fuel_con_comb", "fuel_con_mpg","co2_emissions")

names(fcr16) <- c("year", "make", "model", "class", "engine_size", "cylinders", "transmission",
                  "fuel_type", "fuel_con_city", "fuel_con_hwy", "fuel_con_comb", "fuel_con_mpg","co2_emissions")

# remove rows with NA
fcr14 <- fcr14[complete.cases(fcr14),]
fcr15 <- fcr15[complete.cases(fcr15),]
fcr16 <- fcr16[complete.cases(fcr16),]

# remove unecessary whitespace

trim <- function(x) gsub("[[:space:]]", "", x)
fcr14$class <- trim(fcr14$class)
fcr15$class <- trim(fcr15$class)
fcr16$class <- trim(fcr16$class)

# adjust column classes

fcr14$make <- as.factor(fcr14$make)

# remove information about pickup trucks and cargo vans
fcr14 <- subset(fcr14, class !="PICKUPTRUCK-SMALL" & class !="PICKUPTRUCK-STANDARD" 
                & class !="SPECIALPURPOSEVEHICLE" & class !="VAN-CARGO")

fcr15 <- subset(fcr15, class !="PICKUPTRUCK-SMALL" & class !="PICKUPTRUCK-STANDARD" 
                & class !="SPECIALPURPOSEVEHICLE" & class !="VAN-CARGO")

fcr16 <- subset(fcr16, class !="PICKUPTRUCK-SMALL" & class !="PICKUPTRUCK-STANDARD" 
                & class !="SPECIALPURPOSEVEHICLE" & class !="VAN-CARGO")

# change appropriate columns into numeric / factor

for (i in 9:13) {
  fcr14[,i] <- as.numeric(fcr14[,i])
}
for (i in 9:13) {
  fcr15[,i] <- as.numeric(fcr15[,i])
}
for (i in 9:13) {
  fcr16[,i] <- as.numeric(fcr16[,i])
}

i <- 5

# group vehicles into larger categories
fcr14$class <- ifelse(fcr14$class == "VAN-PASSENGER", "MINIVAN", fcr14$class)
fcr15$class <- ifelse(fcr15$class == "VAN-PASSENGER", "MINIVAN", fcr15$class)
fcr16$class <- ifelse(fcr16$class == "VAN-PASSENGER", "MINIVAN", fcr16$class)

fcr14$class <- ifelse(fcr14$class == "STATIONWAGON-MID-SIZE" | fcr14$class == "STATIONWAGON-SMALL",
                      "STATIONWAGON", fcr14$class)
fcr15$class <- ifelse(fcr15$class == "STATIONWAGON-MID-SIZE" | fcr15$class == "STATIONWAGON-SMALL",
                      "STATIONWAGON", fcr15$class)
fcr16$class <- ifelse(fcr16$class == "STATIONWAGON-MID-SIZE" | fcr16$class == "STATIONWAGON-SMALL",
                      "STATIONWAGON", fcr16$class)

fcr14$class <- as.factor(fcr14$class)
fcr15$class <- as.factor(fcr15$class)
fcr16$class <- as.factor(fcr16$class)
  
#### explore data, make some plots and other cool shit #####
hist(fcr14$co2_emissions)
plot(fcr14$fuel_con_mpg, fcr14$c02_emissions)
boxplot(fcr14$fuel_con_city ~ fcr14$fuel_type)

##### average co2 level by class #####
co2_by_class14 <- fcr14 %>%
  group_by(class) %>%
  summarise(mean_co2 = mean(co2_emissions))

co2_by_class15 <- fcr15 %>%
  group_by(class) %>%
  summarise(mean_co2 = mean(co2_emissions))

co2_by_class16 <- fcr16 %>%
  group_by(class) %>%
  summarise(mean_co2 = mean(co2_emissions))

co2_merge <- merge(co2_by_class14, co2_by_class15, by = "class")
co2_merge <- merge(co2_merge, co2_by_class16, by = "class")

# rename columns
names(co2_merge) <- c("class", "mean_co2_14", "mean_co2_15", "mean_co2_16")

# melt by class variable
co2_melt <- melt(co2_merge, id.vars = "class")

# make bar graph of this data

ggplot(co2_melt, aes(class, value, group = variable, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Type of Car") +
  ylab("Average CO2 Emissions") +
  ggtitle("Average CO2 Emission from 2014-2016") +
  theme_economist()

ggplot(co2_melt, aes(variable, value, group = class, col = class)) +
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Average CO2 Emissions") +
  ggtitle("Average CO2 Emissions by Type of Car 2014-2016")
  

