#Homework 1 - Gustavo Mendonca

#Load libraries
library(nycflights13)
library(dplyr)

flights %>% 
  filter(arr_delay >= 120) %>% 
  filter(dest %in% c("IAH","HOU")) %>% 
  filter(carrier %in% c("UA","AA","DL")) %>% 
  filter(month %in% c(7,8,9)) %>% 
  filter(dep_time - sched_dep_time <= dep_delay)


flights %>% 
  filter(arr_delay >= 120) %>% 
  filter(dep_delay>0)

flights %>% 
  filter(is.na(dep_time)==TRUE) %>% 
  count(month,sort=TRUE) %>% 
  mutate(percentTotal = n/sum(n)) %>% 
  arrange(percentTotal) %>% 
  slice(c(which.min(percentTotal),which.max(percentTotal)))




mostFlights = flights %>%
  filter(is.na(tailnum)==FALSE) %>% 
  group_by(tailnum) %>% 
  count(tailnum,sort=TRUE)
left_join(mostFlights,planes,by="tailnum")


average_temperature=weather %>% 
  filter(is.na(temp)==FALSE) %>% 
  filter(month==07) %>% 
  filter(year==2013) %>%
  group_by(day) %>% 
  summarize(avgTemp=mean(temp))

ggplot(average_temperature, aes(x = avgTemp))+
  geom_histogram()

weather %>% 
  slice(c(which.min(wind_speed),which.max(wind_speed)))
  
weather_stats = weather %>% 
  filter(is.na(temp)==FALSE) %>% 
  filter(month==07) %>% 
  filter(year==2013) %>%
  group_by(day)

ggplot(weather_stats, aes(x=dewp, y = humid))+
  geom_point()
ggplot(weather_stats, aes(x=precip, y = visib))+
  geom_point()

#Question 5
planes %>% 
  filter(is.na(year)==TRUE) %>% 
  count(year,sort=TRUE)
  
common_manufacturers = planes %>% 
  filter(is.na(manufacturer)==FALSE) %>% 
  group_by(manufacturer) %>% 
  count(manufacturer,sort=TRUE)
most_common = data.frame(common_manufacturers[1:5,],stringsAsFactors = FALSE)
most_common

flights_manuf = left_join(flights,planes,by="tailnum")
flights_trends = flights_manuf %>%
  filter(is.na(manufacturer)==FALSE) %>% 
  group_by(month) %>% 
  count(manufacturer,sort=TRUE) %>% 
  arrange(month) %>% 
  filter(manufacturer %in% most_common[,1])

ggplot(flights_trends, aes(x=month, y = n,color=manufacturer))+
  geom_line()+
  labs(title="Trends in manufacturers",
       x ="Months of 2013", y = "Number of flights per month")

#Question 6
flights_manuf = left_join(planes,flights,by="tailnum")
flights_manuf %>% 
  slice(which.min(year.x))

flights_manuf = left_join(flights,planes,by="tailnum")
mostFlights = flights %>% 
  filter(is.na(tailnum)==FALSE) %>% 
  group_by(tailnum) %>% 
  count(tailnum,sort=TRUE)

sum(mostFlights$tailnum %in% planes$tailnum)

#Question 7
flights %>% 
  filter(is.na(arr_delay)==FALSE) %>% 
  group_by(month,origin) %>% 
  summarize(medianDelay = median(arr_delay))

median_EWR = flights %>% 
  filter(is.na(arr_delay)==FALSE) %>%
  filter(origin=="EWR") %>% 
  group_by(month,carrier) %>% 
  summarize(medianDelay = median(arr_delay))

ggplot(median_EWR, aes(x=month, y = medianDelay,color=carrier))+
  geom_line()+
  labs(title="Median monthly arrival delays at EWR",
       x ="Months of 2013", y = "Median delay time (minutes)")

median_JFK = flights %>% 
  filter(is.na(arr_delay)==FALSE) %>%
  filter(origin=="JFK") %>% 
  group_by(month,carrier) %>% 
  summarize(medianDelay = median(arr_delay))

ggplot(median_JFK, aes(x=month, y = medianDelay,color=carrier))+
  geom_line()+
  labs(title="Median monthly arrival delays at JFK",
       x ="Months of 2013", y = "Median delay time (minutes)")

media_LGA = flights %>% 
  filter(is.na(arr_delay)==FALSE) %>%
  filter(origin=="LGA") %>% 
  group_by(month,carrier) %>% 
  summarize(medianDelay = median(arr_delay))

ggplot(median_LGA, aes(x=month, y = medianDelay,color=carrier))+
  geom_line()+
  labs(title="Median monthly arrival delays at LGA",
       x ="Months of 2013", y = "Median delay time (minutes)")

#Question 8
flights_airlines = left_join(flights,airlines,by="carrier")
flights_sfo = flights_airlines %>% 
  filter(dest=="SFO") %>%
  group_by(name) %>% 
  count(name,sort=TRUE,name="SFO")

flights_total = flights_airlines %>% 
  filter(is.na(dest)==FALSE) %>%
  group_by(name) %>% 
  count(name,sort=TRUE,name="total_flights")

left_join(flights_sfo,flights_total,by="name") %>% 
  mutate(percent_flights = SFO/total_flights)
  
#Question 10
ageDifference = age_gaps %>% 
  group_by(age_difference) %>% 
  count(age_difference,sort=TRUE)

ggplot(ageDifference, aes(x = age_difference,y=n))+
  geom_col()

ageDifference %>% 
  slice(which.max(age_difference))

sevenRule = age_gaps %>% 
  mutate(LB = actor_1_age>(actor_2_age/2+7)) %>% 
  mutate(UB = (actor_2_age-7)*2 > actor_1_age)
dim(sevenRule)[1]*2 - sum(sevenRule$LB) - sum(sevenRule$UB)

#Actors with the most love interests
age_gaps %>%
  count(actor_1_name,sort=TRUE,name="count") %>% 
  slice(which.max(count))

#Movies with most love interests
age_gaps %>%
  count(movie_name,sort=TRUE,name="count") %>% 
  slice(which.max(count))

#Changes in trends of age difference
age_trends = age_gaps %>% 
  group_by(release_year) %>% 
  summarize(meanDifference = mean(age_difference),medianDifference = median(age_difference))

ggplot(age_trends, aes(x = release_year))+
  
  geom_line(aes(y = meanDifference, color="Mean annual age difference"))+
  geom_line(aes(y = medianDifference, color="Median annual age difference"))

#Same gender couple
sameGender = age_gaps %>% 
  mutate(SG = character_1_gender == character_2_gender)
#Number of occasions
sum(sameGender$SG)


