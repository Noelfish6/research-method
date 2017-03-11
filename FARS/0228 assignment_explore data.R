data2015<- read.csv("2015accident.csv", header=TRUE, sep="\t", stringsAsFactors=TRUE)
data2015 <- read.csv("2015accident.csv", header = T, sep = ",")
str(data2015)
names(data2015)


# Libraries
library(readr)
library(dplyr)
library(ggplot2)

########################################################################################################################

#dplyr

data3 <- read.csv("2015accident.csv",
          header=TRUE,
          sep=",",
          fill=T)

head(data3)
class(data3)
View(data3)

##tbl data format
data3_tbl = tbl_df(data3)
class(data3_tbl)
head(data3_tbl)

data3_tbl
View(data3_tbl)

##filter
filter(data3_tbl, STATE=="25") #STATE == 25
filter(data3_tbl, STATE %in% c(24,25)) #STATE == 24,25

filter(data3_tbl, STATE=="25" & FATALS >= 1) ->filter_data3
View(filter_data3)

##select - keep the fields you choose
select(data3_tbl, STATE, FATALS) ->select_data3
View(select_data3)

##arange - order 
arrange(data3_tbl, FATALS) ->arrange_data3 #small to large
View(arrange_data3)
arrange(data3_tbl, desc(FATALS)) -> arrange_desc_data3#large to small
View(arrange_desc_data3)

##mutate - create new field based on original field
mutate(data3_tbl, double = 2*FATALS)

##summarise - e.g. sum, mean, min, max, sd, median, IQR, n, n_distinct(x), first(x), last(x), nth(x,n)
summarise(data3_tbl, total = sum(DAY_WEEK)) -> sum_data3
View(sum_data3)

##join - e.g. left_join, inner_join, semi_join, anti_join

##group_by
by_dayWeek <- group_by(data3_tbl, DAY_WEEK)
sum_fatals_by_weekday = summarise(by_dayWeek, total=sum(FATALS), average_fatals = mean(FATALS))
View(sum_fatals_by_weekday)

by_hour <- group_by(data3_tbl, HOUR)
sum_fatals_by_hour = summarise(by_hour, count = n())
View(sum_fatals_by_hour)

##管道函數 %>%
data3_tbl %>% group_by(HOUR) %>% summarise(count = n())

##do(data,fun(.)) e.g. 篩選最大的前兩條數據

##colwise - based on "plyr" package, e.g. use same function on multiple fields

########################################################################################################################


#basic intro
##overall year fatals
##season fatals
##month fatals
##week fatals
##day fatals
##4 time range fatals
##specific hour fatals



##States fatal accidents
View(data3)
by_state <- group_by(data3, STATE)
sum_fatals_by_state = summarise(by_state, total=sum(FATALS), average_fatals = mean(FATALS))
View(sum_fatals_by_state)

ggplot(sum_fatals_by_state) + geom_bar( aes(STATE) )

barplot(sum_fatals_by_state$total,xlab="States")

ggplot(sum_fatals_by_state, aes(STATE,total)) + geom_histogram(binwidth = 1) + facet_grid(data3$DAY_WEEK)



ggplot(data2, aes(x = HOUR, fill = FATAL)) + geom_histogram(binwidth = 1) + facet_grid(data2DAY_WEEK)


n <- names(data2015)
edit(n)

data2 <- data2015[,c("COUNTY","CITY","DAY","MONTH","YEAR","DAY_WEEK","HOUR","LATITUDE","LONGITUD", "WEATHER1", "WEATHER2", "WEATHER","FATALS","DRUNK_DR")]
madata<-subset(data2, COUNTY==25)

#scatter plot
ggplot(data2, aes(x=HOUR, y=FATALS)) + geom_point()
ggplot(data2, aes(x=MONTH, y=COUNTY)) + geom_point()

#line chart
ggplot(data2, aes(x=MONTH+DAY+HOUR, y=COUNTY)) + geom_line() + geom_point()

str(madata)


#basic math
( sum(madata$FATALS) / sum(data2$FATALS) ) * 100 #MA is pretty safe

