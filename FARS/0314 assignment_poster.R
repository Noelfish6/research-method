library(ggplot2)
library(ggmap)
library(dplyr)
library(plotly)


accident <- read.csv("2015accident.csv", header = T, sep = ",")

#map

  us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
  map <- get_stamenmap(us, zoom = 6, maptype = "toner-lite")
  USAMap = ggmap(map)

  #dealing data
  mydata = read.csv("2015accident.csv")
  mydata$State <- as.character(mydata$STATE)
  
  mv_num_fatal = data.frame(mydata$FATALS, mydata$LONGITUD, mydata$LATITUDE)
  colnames(mv_num_fatal) = c('fatal','lon','lat')
  
  usa_center = as.numeric(geocode("United States"))
  
  USAMap + geom_point(aes(x=lon, y=lat), data=mv_num_fatal, col="#FAEE1C", alpha=0.1, size=mv_num_fatal$fatal*1.5) + scale_size_continuous(range=range(mv_num_fatal$fatal))
  
  
  #other map
  usa <- map_data("usa")
  ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
    coord_fixed(1.3)
  
  map_usa + geom_point(aes(x=lon, y=lat), data=mv_num_fatal, col="#FAEE1C", alpha=0.1, size=mv_num_fatal$fatal*1.5) + scale_size_continuous(range=range(mv_num_fatal$fatal))
  
#statistic description
  summary(fars$FATALS)
  
   
#rectangle
  
  #by day
  accident$yearDay <- paste(accident$YEAR,accident$MONTH,accident$DAY)
  accident$yearDay <- factor(accident$yearDay,levels=rev(unique(accident$yearDay)))
  
  ggplot(accident, aes(STATE, yearDay)) +
    geom_raster(aes(fill = FATALS))
  
  #by month
  accident$yearMonth <- paste(accident$YEAR,accident$MONTH)
  accident$yearMonth <- factor(accident$yearMonth,levels=rev(unique(accident$yearMonth)))
  
  by_Month <- group_by(accident, STATE, yearMonth)
  sum_fatals_by_month = summarise(by_Month, total=sum(FATALS))
  View(sum_fatals_by_month)
  
  ggplot(sum_fatals_by_month, aes(STATE, yearMonth)) +
    geom_raster(aes(fill = total)) + scale_fill_gradientn(colours=c("#F6F5F5","#FAEE1C","#F02A71"))
  ggplotly()

  
#3 states' fatals in hours
  fars <- subset(fars, !(HOUR == 99))
  
  group <- group_by(fars, STATE, HOUR)
  accidentsByHour <- summarize(group, FATALS = n())
  accidentsByHour3State <- filter(accidentsByHour, STATE == 6 | STATE == 12 | STATE == 48)
  
  
  ggplot(accidentsByHour3State, aes(HOUR, FATALS)) +
    geom_point(color = "#FAEE1C" ) +
    labs(x = "Hour", y = "Number of fatalities") +
    geom_smooth(color = "#F02A71" ,fill = "#d7d7d8") +
    facet_grid(STATE~.)+
    theme_bw(base_family = "Helvetica")
  
#weather factor resulting in accidents
  fars <- subset(fars, !(WEATHER == 98 | WEATHER == 99))
  
  groupWeather <- group_by(fars, STATE, WEATHER)
  accidentsByWeather <- summarize(groupWeather, FATALS = n())
  accidentsByWeather3State <- filter(accidentsByWeather, STATE == 6 | STATE == 12 | STATE == 48)
  
  ggplot(accidentsByWeather3State, aes(STATE,FATALS, fill = WEATHER)) + 
    geom_bar(position = "fill", stat = 'identity') +
    scale_fill_gradientn(colours=c("#FAEE1C","#F6F5F5","#DBA51E",
                                   "#7B0C1B","#990B19","#D41D40","#F05A7D","#F02A71"))
    
  
  ggplot(accidentsByWeather, aes(STATE,FATALS, fill = WEATHER)) + geom_bar(position = "fill", stat = 'identity')
  ggplot(accidentsByWeather, aes(STATE,FATALS)) + geom_bar(aes(fill = accidentsByWeather$WEATHER), position = "fill", stat = 'identity')
  ggplot(accidentsByWeather, aes(STATE,FATALS)) + geom_bar(aes(fill = accidentsByWeather$WEATHER), stat = 'identity')
  