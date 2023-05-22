#Q1
cases_year=mass_shootings %>% 
  group_by(year) %>% 
  count(year,sort=TRUE)

#Q2
cases_race = mass_shootings %>% 
  group_by(race) %>% 
  count(race,sort=TRUE)

ggplot(cases_race, aes(x=race, y = n))+
  geom_col()+
  labs(title="Mass shootings by race",
       x ="Race", y = "Occurrences")+
  geom_text(aes(label = n), vjust = -0.5)

#Q3
cases_location = mass_shootings %>% 
  group_by(location_type) 

ggplot(cases_location, aes(x=location_type, y = total_victims))+
  geom_boxplot()

#Q4
cases_location_vegas = mass_shootings %>%
  filter(case!="Las Vegas Strip massacre") %>% 
  group_by(location_type)

ggplot(cases_location_vegas, aes(x=location_type, y = total_victims))+
  geom_boxplot()

#Q5
white_males = mass_shootings %>% 
  filter(year>2000) %>% 
  filter(prior_mental_illness=="Yes") %>% 
  filter(race=="White") %>% 
  count(prior_mental_illness)

#Q6
months = data.frame(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                    c(1,2,3,4,5,6,7,8,9,10,11,12))
colnames(months) = c("month","Number")
cases_month = mass_shootings %>% 
  group_by(month) %>% 
  count(month)

cases_month = left_join(cases_month,months,by="month")

cases_month = cases_month %>% 
  arrange(Number,by_group="Number")

ggplot(cases_month, aes(x=Number, y = n))+
  geom_col()+
  labs(title="Mass shootings by month",
       x ="Month of the year", y = "Occurrences")+
  geom_text(aes(label = n), vjust = -0.5)

#Q7
cases_distrib = mass_shootings %>%
  filter(race %in% c("White","Black","Latino")) %>% 
  group_by(race)

ggplot(cases_distrib,aes(x=fatalities,fill=race))+
  geom_histogram(alpha=1,position="dodge")+
  labs(title="Distribution of fatalies by race",
       x ="Fatalities", y = "Occurence")

#Q8
mental_distrib = mass_shootings %>% 
  filter(is.na(prior_mental_illness)==FALSE) %>% 
  group_by(prior_mental_illness)
  
mental_location = mass_shootings %>%
  filter(is.na(prior_mental_illness)==FALSE) %>% 
  group_by(prior_mental_illness) %>% 
  count(location_type)
mental_race = mass_shootings %>%
  filter(is.na(prior_mental_illness)==FALSE) %>% 
  filter(is.na(race)==FALSE) %>% 
  group_by(prior_mental_illness) %>% 
  count(race)

ggplot(mental_location,aes(x=location_type,y=n,fill=prior_mental_illness))+
  geom_col(position = "dodge")
ggplot(mental_race,aes(x=race,y=n,fill=prior_mental_illness))+
  geom_col(position = "dodge")
ggplot(mental_distrib,aes(x=age_of_shooter,fill=prior_mental_illness))+
  geom_histogram(alpha=1,position="dodge")


#Q9
#Relationship Mental Illness vs Victims
ggplot(mental_distrib,aes(x=total_victims,fill=prior_mental_illness))+
  geom_histogram(alpha=1,position="dodge")

#Relationship Location type vs Mental Illness
ggplot(mental_location,aes(x=location_type,y=n,fill=prior_mental_illness))+
  geom_col(position = "dodge")

#Relationship between the 3 variables
ggplot(mental_distrib,aes(x=location_type,y=total_victims,color=prior_mental_illness))+
  geom_point()

#Q10
total_transactions = card_fraud %>% 
  group_by(trans_year) %>%
  count(trans_year,name="transactions")
total_frauds = card_fraud %>%
  filter(is_fraud==1) %>% 
  group_by(trans_year) %>%
  count(is_fraud,name="frauds")
total_frauds = left_join(total_frauds,total_transactions,by="trans_year")
total_frauds %>% 
  mutate(fraud_rate = frauds/transactions)

#Q11
card_fraud %>%
  group_by(trans_year,is_fraud) %>% 
  summarize(count = sum(amt))%>%
  ungroup() %>% 
  mutate(is_fraud = ifelse(is_fraud == 1, "fraud", "valid")) %>% 
  pivot_wider(names_from = "is_fraud",values_from = "count") %>% 
  mutate(ratio=fraud/(valid+fraud))

#Q12
fraud_hist = card_fraud %>%
  mutate(is_fraud = ifelse(is_fraud == 1, "fraud", "valid"))
  
ggplot(fraud_hist,aes(x=amt,fill=is_fraud))+
  scale_y_continuous(limits=c(0,500))+
  scale_x_continuous(limits=c(0,7000))+
  geom_histogram(alpha=1,position="dodge",binwidth = 50)+
  labs(title="Distribution of fraud amounts",
       x ="Amount in USD", y = "Occurrences")

fraud_hist %>% 
  group_by(is_fraud) %>% 
  summarize(countAmt = n(),meanAmount=mean(amt),medianAmount=median(amt),sdAmt=sd(amt),minAmt=min(amt),maxAmt=max(amt))

#Q13
card_fraud %>% 
  group_by(category) %>% 
  count(is_fraud) %>% 
  ungroup() %>% 
  mutate(is_fraud = ifelse(is_fraud == 1, "fraud", "valid")) %>%
  pivot_wider(names_from = "is_fraud",values_from = "n") %>% 
  mutate(ratio=fraud/(valid+fraud)) %>% 
  arrange(desc(ratio))

#Q14
fraud_time = card_fraud %>% 
  mutate(
    date_only = lubridate::date(trans_date_trans_time),
    month_name = lubridate::month(trans_date_trans_time, label=TRUE),
    hour = lubridate::hour(trans_date_trans_time),
    weekday = lubridate::wday(trans_date_trans_time, label = TRUE)
  )
#Most common day
fraud_time %>% 
  group_by(weekday) %>% 
  count(weekday,sort=TRUE)
#Most common hour
fraud_time %>% 
  group_by(hour) %>% 
  count(hour,sort=TRUE)
#Most common month
fraud_time %>% 
  group_by(month_name) %>% 
  count(month_name,sort=TRUE)

#Q15
fraud_age = card_fraud %>%
  mutate(
    age = round(interval(dob, trans_date_trans_time) / years(1)),-1) %>%
  group_by(age) %>% 
  count(is_fraud) %>%
  ungroup() %>% 
  mutate(is_fraud = ifelse(is_fraud == 1, "fraud", "valid")) %>%
  pivot_wider(names_from = "is_fraud",values_from = "n") %>% 
  mutate(ratio=fraud/(valid+fraud))
  

ggplot(fraud_age,aes(x=age,y=ratio))+
  geom_col()

#Q16
card_fraud$distance_km=round(card_fraud$distance_km,0)
fraud_distance = card_fraud %>%
  group_by(distance_km) %>% 
  count(is_fraud) %>%
  ungroup() %>% 
  mutate(is_fraud = ifelse(is_fraud == 1, "fraud", "valid")) %>%
  pivot_wider(names_from = "is_fraud",values_from = "n") %>% 
  mutate(fraud = ifelse(is.na(fraud)==TRUE, 0, fraud)) %>% 
  mutate(ratio=fraud/(valid+fraud))

ggplot(fraud_distance,aes(x=distance_km,y=ratio))+
  geom_col()

#Q17
#Turning energy into tidy, long format
energy %>% 
  pivot_longer(cols=4:18,names_to = "characteristics",values_to="values")

#Merging the tables
#Selecting the relevant info
co2_percap = co2_percap %>% 
  select(iso3c,year,co2percap)
gdp_percap = gdp_percap %>% 
  select(iso3c,year,GDPpercap)
energy_select = energy %>% 
  select(year,iso_code,energy_per_capita)

#Relationship GDP vs CO2 per capita
joint_table = left_join(gdp_percap,co2_percap,by=c("iso3c"="iso3c","year"="year"))
ggplot(joint_table,aes(x=GDPpercap,y=co2percap))+
  geom_point()

#Relationship GDP vs Energy consumption per capita
energy_gdp = left_join(gdp_percap,energy_select,by=c("iso3c"="iso_code","year"="year"))
ggplot(energy_gdp,aes(x=GDPpercap,y=energy_per_capita))+
  geom_point()


                        