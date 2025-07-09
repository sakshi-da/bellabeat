install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("rmarkdown")

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(skimr)
library(janitor)

#activity
daily_activity<- read.csv("C:/Users/saksh/Desktop/case study 2/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
#daily_activity$date<- mdy(daily_activity$ActivityDate)
#daily_activity1<- daily_activity[-c(2)] 
daily_activity$ActivityDate=as.POSIXct(daily_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
#daily_activity$date <- format(daily_activity$ActivityDate, format = "%m/%d/%y")
daily_activity$ActivityDate=as.Date(daily_activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
#daily_activity$date=as.Date(daily_activity$date, format="%m/%d/%Y")
glimpse(daily_activity)
head(daily_activity)

daily_activity %>%  
  select(TotalSteps,TotalDistance,SedentaryMinutes, Calories) %>%
  summary()


#heart rate
heartrate<- read.csv("C:/Users/saksh/Desktop/case study 2/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
head(heartrate)


#intensities
intensities<- read.csv("C:/Users/saksh/Desktop/case study 2/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
intensities$ActivityDay=as.Date(intensities$ActivityDay, format="%m/%d/%Y", tz=Sys.timezone())
head(intensities)

intensities %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()



#calories
calories<-read.csv("C:/Users/saksh/Desktop/case study 2/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
head(calories)
calories%>%
  select(Calories)%>%
  summary()


#sleep
sleep <- read.csv("C:/Users/saksh/Desktop/case study 2/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
sleep<-sleep[!duplicated(sleep), ]
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
#sleep$date<- mdy(sleep$SleepDay)
#sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
#sleep$date=as.Date(sleep$date, "% m/% d/% y")
head(sleep)
sleep%>%
  select(TotalSleepRecords,TotalMinutesAsleep,TotalTimeInBed)%>%
  summary()



#weight
weight<- read.csv("C:/Users/saksh/Desktop/case study 2/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
weight<- weight[-c(5)]
head(weight)

# total number of participants :

daily_activity%>%
  summarise(total_participants = n_distinct(daily_activity$Id))

n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(heartrate$Id) #wont include heart rate any further because of bias
n_distinct(sleep$Id)
n_distinct(weight$Id) #wont include weight because data is not given by number of participants unable to make prediction depending on it

#for most active time
active <- merge(weight, intensities, by="Id", all=TRUE)
active$time <- format(active$Date, format = "%H:%M:%S")



ggplot(active)+
  geom_line(mapping = aes(x=time, y=VeryActiveMinutes,color="red"))+
  theme(axis.text.x = element_text(angle=90) )+
  labs(title="Total very Active Intensity vs. Time ")

ggplot(sleep)+
  geom_point(mapping= aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep , y= TotalTimeInBed))+
  labs(title = "Total time in bed vs asleep")
 

ggplot(daily_activity)+
  geom_point(mapping = aes(x=TotalSteps, y= SedentaryMinutes))+
  geom_smooth(mapping = aes(x=TotalSteps, y= SedentaryMinutes))+
  labs(title = "Total steps vs sedentary minutes")

ggplot(daily_activity)+
  geom_point(mapping = aes(y=Calories, x=TotalSteps))+
  geom_smooth(mapping = aes(y=Calories, x= TotalSteps))+
  labs(title = "Total steps vs Calories")

