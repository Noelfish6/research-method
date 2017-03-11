library(readr)
library(dplyr)
library(ggplot2)


fars <- read.csv("2015accident.csv")

fars_hour_clean <- subset(fars, !(HOUR == 99))
View(fars)


View(accidents)

#fatals decrease by day
group <- group_by(fars, DAY)
accidentsByDate <- summarize(group, FATALS = n())

ggplot(accidentsByDate, aes(DAY, FATALS)) +
  geom_point(color = 'red') +
  labs(x = "Day", y = "Number of fatalities") +
  geom_smooth()

#States fatal accidents
group_state <- group_by(fars, STATE)
accidentsByState <- summarize(group_state, FATALS = n())


View(accidentsByState)

accidentsByState <- c()

ggplot(accidentsByState, aes(STATE, FATALS)) + geom_bar(stat = 'identity')


ggplot(accidentsByState, aes(x = factor(STATE), y = FATALS)) + geom_bar(stat = 'identity')

class(accidentsByState)
head(accidentsByState)


#Week days fatalities
ggplot(fars_hour_clean, aes(x = HOUR, fill = FATALS)) + geom_histogram(binwidth = 1) + facet_grid(DAY_WEEK~.)


