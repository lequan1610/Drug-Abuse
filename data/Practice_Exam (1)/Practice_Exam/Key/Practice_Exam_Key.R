################
##### INIT #####
################





#NOTE: This is how I would've (probably) coded to solve this practice exam. You 
#don't always have to use my exact code or style to get to the results. So long 
#as you get to the same answers, feel free to code using your own style.

#Set working directory
setwd("~/Desktop/PhD Stuff/Teaching/P4E/Practice Exam/Practice_Exam")

#Clear environment
rm(list = ls())

#Load necessary packages
library(readxl)
library(tidyverse)





################################
##### PART 1: CITY BUDGET ######
################################





#Load dataset
city_budget = read_xlsx("Datasets/city_budget.xlsx")
#Convert from tibble to data.frame
city_budget = as.data.frame(city_budget)



### Q1 ###



#Compute mean
mean(city_budget$total_budget) #513653
#Compute median
median(city_budget$total_budget) #517450



### Q2 ###



#Compute education percent of the budget
city_budget = mutate(city_budget, education_percent = (education_budget/total_budget)*100)

#Compute means by region
mean(city_budget[city_budget$region == "North", "education_percent"]) #31.3%
mean(city_budget[city_budget$region == "South", "education_percent"]) #29.1%
mean(city_budget[city_budget$region == "East", "education_percent"]) #29.7%
mean(city_budget[city_budget$region == "West", "education_percent"]) #31.1%

#Compute medians by region
median(city_budget[city_budget$region == "North", "education_percent"]) #29.7%
median(city_budget[city_budget$region == "South", "education_percent"]) #27.6%
median(city_budget[city_budget$region == "East", "education_percent"]) #29.7%
median(city_budget[city_budget$region == "West", "education_percent"]) #31.1%



### Q3 ###



#Create budget per capita variable
city_budget = mutate(city_budget, budget_per_capita = total_budget/population)

#slice_max to get top five cities by budget_per_capita
city_budget %>%
  slice_max(budget_per_capita, n = 5)
#3, 17, 28, 77, 88



### Q4 ###



ggplot(city_budget, aes(x = total_budget, y = service_quality_index)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(breaks = c(1:8)*100000 + 50000, lim = c(150000, 850000)) +
  scale_y_continuous(breaks = c(3:10)*10, lim = c(30, 100))





#########################################
##### PART 2: PUBLIC TRANSPORTATION #####
#########################################





#Load dataset
public_transport_survey = read_xlsx("Datasets/public_transport_survey.xlsx")
#Convert from tibble to data.frame
public_transport_survey = as.data.frame(public_transport_survey)



### Q1 ###



#See unique values of num_trips_week
sort(unique(public_transport_survey$num_trips_week)) 
#Not possible to take negative trips
100*length(which(public_transport_survey$num_trips_week < 0))/nrow(public_transport_survey)
#2%

#See unique values of satisfaction
sort(unique(public_transport_survey$satisfaction))
#Scale must range from 1-5
100*length(which(public_transport_survey$satisfaction < 1 | public_transport_survey$satisfaction > 5))/nrow(public_transport_survey)
#3.3%

#See unique values of punctuality_rating
sort(unique(public_transport_survey$punctuality_rating))
#Scale must range from 1-5
100*length(which(public_transport_survey$punctuality_rating < 1 | public_transport_survey$punctuality_rating > 5))/nrow(public_transport_survey)
#2%



### Interlude: Replacing errors with NAs ###



public_transport_survey[public_transport_survey$num_trips_week < 0, "num_trips_week"] = NA
public_transport_survey[public_transport_survey$satisfaction < 1 | public_transport_survey$satisfaction > 5, "satisfaction"] = NA
public_transport_survey[public_transport_survey$punctuality_rating < 1 | public_transport_survey$punctuality_rating > 5, "punctuality_rating"] = NA



### Q2 ###



#Create variables for all-weekday commuters and high-satisfaction commuters
public_transport_survey = mutate(public_transport_survey,
                                 all_weekday_commuter = ifelse(num_trips_week >= 10, 1, 0),
                                 high_satisfaction = ifelse(satisfaction >= 4, 1, 0))

#Compute means
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Bus", "high_satisfaction"], na.rm = TRUE) #46.9%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Metro", "high_satisfaction"], na.rm = TRUE) #88.5%          
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Train", "high_satisfaction"], na.rm = TRUE) #100%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Tram", "high_satisfaction"], na.rm = TRUE) #100%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Bus", "high_satisfaction"], na.rm = TRUE) #59.3%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Metro", "high_satisfaction"], na.rm = TRUE) #80%          
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Train", "high_satisfaction"], na.rm = TRUE) #74.2%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Tram", "high_satisfaction"], na.rm = TRUE) #81.3%



### Q3 ###


#Compute means
mean(public_transport_survey[public_transport_survey$transport_mode == "Bus", "punctuality_rating"], na.rm = TRUE) #2.9
mean(public_transport_survey[public_transport_survey$transport_mode == "Metro", "punctuality_rating"], na.rm = TRUE) #4
mean(public_transport_survey[public_transport_survey$transport_mode == "Train", "punctuality_rating"], na.rm = TRUE) #3.3
mean(public_transport_survey[public_transport_survey$transport_mode == "Tram", "punctuality_rating"], na.rm = TRUE) #3.7

#Compute medians
median(public_transport_survey[public_transport_survey$transport_mode == "Bus", "punctuality_rating"], na.rm = TRUE) #3
median(public_transport_survey[public_transport_survey$transport_mode == "Metro", "punctuality_rating"], na.rm = TRUE) #4
median(public_transport_survey[public_transport_survey$transport_mode == "Train", "punctuality_rating"], na.rm = TRUE) #3
median(public_transport_survey[public_transport_survey$transport_mode == "Tram", "punctuality_rating"], na.rm = TRUE) #4



### Q4 ###



ggplot(public_transport_survey, aes(x = as.factor(transport_mode), y = num_trips_week)) +
  stat_summary(fun.y = mean, geom = "col") +
  scale_y_continuous(breaks = c(0:6)*2, lim = c(0, 12))





#############################
##### PART 3: HOSPITALS #####
#############################





#Load datasets
hospital_data = read_xlsx("Datasets/hospital_data.xlsx")
readmission_rates = read_xlsx("Datasets/readmission_rates.xlsx")
#Convert from tibble to data.frame
hospital_data = as.data.frame(hospital_data)
readmission_rates = as.data.frame(readmission_rates)



### Q1 ###



#Compute means
mean(hospital_data[hospital_data$region == "North", "avg_wait_time"]) #44.3
mean(hospital_data[hospital_data$region == "East", "avg_wait_time"]) #44.4
mean(hospital_data[hospital_data$region == "South", "avg_wait_time"]) #46.7
mean(hospital_data[hospital_data$region == "West", "avg_wait_time"]) #44.7



### Q2 ###



#Compute means
mean(hospital_data[hospital_data$patient_satisfaction >= 4, "avg_wait_time"]) #42.1
mean(hospital_data[hospital_data$patient_satisfaction < 4, "avg_wait_time"]) #46.7
mean(hospital_data[hospital_data$patient_satisfaction >= 4, "mortality_rate"]) #1.71
mean(hospital_data[hospital_data$patient_satisfaction < 4, "mortality_rate"]) #1.66



### Q3 ###



#Merge the data
merged_data = full_join(hospital_data, readmission_rates, by = c("hospital_id"))

#Sort the data by mortality rate
merged_data = merged_data[order(merged_data$mortality_rate), ]

#Create mortality rate terciles
merged_data[1:50, "Tercile"] = "Lowest"
merged_data[51:100, "Tercile"] = "Middle"
merged_data[101:150, "Tercile"] = "Highest"

#Compute means
mean(merged_data[merged_data$Tercile == "Lowest", "thirty_day_readmit_rate"]) #13.7
mean(merged_data[merged_data$Tercile == "Lowest", "heart_failure_readmit"]) #13.7
mean(merged_data[merged_data$Tercile == "Lowest", "pneumonia_readmit"]) #13.6
mean(merged_data[merged_data$Tercile == "Middle", "thirty_day_readmit_rate"]) #14
mean(merged_data[merged_data$Tercile == "Middle", "heart_failure_readmit"]) #14
mean(merged_data[merged_data$Tercile == "Middle", "pneumonia_readmit"]) #13.9
mean(merged_data[merged_data$Tercile == "Highest", "thirty_day_readmit_rate"]) #14.4
mean(merged_data[merged_data$Tercile == "Highest", "heart_failure_readmit"]) #14.5
mean(merged_data[merged_data$Tercile == "Highest", "pneumonia_readmit"]) #14.6



### Q4 ###



merged_data = mutate(merged_data, staff_bed_ratio = staff_count/bed_count)

ggplot(merged_data, aes(x = as.factor(Tercile), y = staff_bed_ratio)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(7:13)*0.1 - 0.05, lim = c(0.65, 1.25)) +
  labs(x = "Terciles of Mortality Rate")



###########################
##### PART 4: HOUSING #####
###########################




#Load dataset
housing_survey = read_xlsx("Datasets/housing_survey.xlsx")
#Convert from tibble to data.frame
housing_survey = as.data.frame(housing_survey)



### Q1 ###



#Obtain percentages
100*length(which(is.na(housing_survey$monthly_income)))/nrow(housing_survey) #15%
100*length(which(is.na(housing_survey$satisfaction_score)))/nrow(housing_survey) #27.5%
100*length(which(is.na(housing_survey$utility_costs)))/nrow(housing_survey) #10%



### Q2 ###



#Plot histogram
ggplot(housing_survey, aes(x = household_size)) +
  geom_histogram(bins = 8) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = (0:7)*10, lim = c(0, 70))



### Q3 ###



#Compute rent percentage of income
housing_survey = mutate(housing_survey, rent_percent = 100*monthly_rent/monthly_income)

#For each unique value of household size...
for (i in 1:8) {
  
  #Print the average percent of income spent on rent for households of that size
  print(mean(housing_survey[housing_survey$household_size == i, "rent_percent"], na.rm = TRUE))
  
}
#31.3%, 26.7%, 20.4%, 19.7%, 15.4%, 14.1%, 10.2%, 7.4%



### Q4 ###



#Compute means
mean(housing_survey[housing_survey$region_type == "Rural", "monthly_income"], na.rm = TRUE) # 3570.9
mean(housing_survey[housing_survey$region_type == "Rural", "satisfaction_score"], na.rm = TRUE) # 9.3
mean(housing_survey[housing_survey$region_type == "Rural", "utility_costs"], na.rm = TRUE) # 152.5
mean(housing_survey[housing_survey$region_type == "Urban", "monthly_income"], na.rm = TRUE) # 3118.2
mean(housing_survey[housing_survey$region_type == "Urban", "satisfaction_score"], na.rm = TRUE) #8.7
mean(housing_survey[housing_survey$region_type == "Urban", "utility_costs"], na.rm = TRUE) #140.4




