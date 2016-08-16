# initiate folder ##

library(dplyr)
library(ggplot2)
library(tidyr) 
library (reshape2)
library (ggthemes)

## initialize folder

home <- "/Users/xing"
project <- paste0(home, "/Desktop/canada_R_code/Data")

ttc <- read.csv(paste0(project, "/TTC.csv"), stringsAsFactors = F)

# remove first column
ttc <- ttc[,-1]

# remove unecessary rows
ttc <- ttc[-c(1:4),]
ttc <- ttc[-c(50:nrow(ttc)),]

# change column header names
colnames(ttc) = ttc[1,]
ttc <- ttc[-1,]

# remove data for years before 2005
ttc <- ttc[,-c(13:32)]

# remove all rows with all blanks
ttc <- ttc[!apply(ttc == "", 1, all),]

# change names of first two cols
colnames(ttc) [1:2] <- c("fare_type", "2015")

#remove all blank spaces
trim <- function(x) gsub("[[:space:]]", "", x)
ttc$fare_type <- trim(ttc$fare_type)


# remove other unecessary rows
ttc <- subset(ttc, ttc$fare_type != "TWIN-GOPASS" & ttc$fare_type != "TWO-FARE" & ttc$fare_type != "TROLLEYCOACH"
            & ttc$fare_type != "POSTALCARRIERS" & ttc$fare_type != "BUS" & ttc$fare_type != "RAIL" & ttc$fare_type != "ADULT" &
              ttc$fare_type != "SENIOR/STUDENT" & ttc$fare_type != "CHILDREN")

# change N/A into NA
for (i in 2:ncol(ttc)) {
ttc[,i] <- ifelse(ttc[,i] == "N/A", NA, ttc[,i])
}

ttc$'2015' <- ifelse(ttc$`2015` == "N/A", NA, ttc$'2015')
ttc$'2014' <- ifelse(ttc$`2014` == "N/A", NA, ttc$'2014')
ttc$'2013' <- ifelse(ttc$`2013` == "N/A", NA, ttc$'2013')
ttc$'2012' <- ifelse(ttc$`2012` == "N/A", NA, ttc$'2012')
ttc$'2011' <- ifelse(ttc$`2011` == "N/A", NA, ttc$'2011')
ttc$'2010' <- ifelse(ttc$`2010` == "N/A", NA, ttc$'2010')
ttc$'2009' <- ifelse(ttc$`2009` == "N/A", NA, ttc$'2009')
ttc$'2008' <- ifelse(ttc$`2008` == "N/A", NA, ttc$'2008')
ttc$'2007' <- ifelse(ttc$`2007` == "N/A", NA, ttc$'2007')
ttc$'2006' <- ifelse(ttc$`2006` == "N/A", NA, ttc$'2006')
ttc$'2005' <- ifelse(ttc$`2005` == "N/A", NA, ttc$'2005')

ttc[1:8,1] <- c("adult_tokens", "adult_tickets", "adult_presto", "adult_reg_month_pass", "adult_student_month_pass", "adult_week_pass",
                "adult_cash", "adult_subtotal")

ttc[9:14,1] <- c("student_month_pass", "stud_week_pass", "stud_tickets", "stud_presto", "stud_cash", "stud_subtotal")

ttc[15:19,1] <- c("child_free", "child_ticket", "child_presto", "child_cash", "child_subtotal")

ttc[20:33,1] <- c("day_visit_etc", "blind_waramps", "premium", "gta_pass", "system_total1", "bus", "subway", "srt", 
                  "streetcar", "rail_total", "system_total2", "weekday", "weekend_holiday", "system_total")

# remove system_total duplicates
ttc <- subset(ttc, ttc$fare_type != "system_total1" & ttc$fare_type != "system_total2")

# trim white spaces in all columns
for (i in 2:ncol(ttc)) {
  ttc[,i] <- trim(ttc[,i])
}

# remove commas from numbers
rm.comma <- function(x) gsub(",", "", x)
for (i in 2:ncol(ttc)) {
  ttc[,i] <- rm.comma(ttc[,i])
}

# change 0 into NA
ttc$'2010' <- ifelse(ttc$'2010' == 0, NA, ttc$'2010')
ttc$'2009' <- ifelse(ttc$'2009' == 0, NA, ttc$'2009')

# make numeric
for (i in 2:ncol(ttc)) {
  ttc[,i] <- as.numeric(ttc[,i])
}

# change order of columns

ttc2 <- ttc[,c("fare_type", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")]

# make line graph of system_total by year

ggplot(ttc2, aes())
