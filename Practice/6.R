# Working with dplyr

library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)
# Creating a local copy my_flights
my_flights <- flights
print(my_flights)
#Filtering out missing values in dep_delay and arr_delay using the filter function and selecting only the required values using the select function
my_flights <- my_flights %>% filter(!is.na(dep_delay)&!is.na(arr_delay)) %>% select(time_hour,origin,dest,carrier,dep_delay,arr_delay,air_time,distance)
print(my_flights)

# Adding new columns Month, DayOfWeek and HourOfDay using the mutate function, We use lubridate library functions to get these new data 
my_flights = my_flights %>% mutate(DayOfWeek=wday(my_flights$time_hour,label=TRUE),HourOfDay=hour(my_flights$time_hour),Month=month(my_flights$time_hour,label=TRUE)) %>%
  select(time_hour,Month,DayOfWeek,HourOfDay,everything())

print(my_flights)

#Displaying the average departure delay statistics based on hour of day using group_by and summarise functions and ordering it based on the AvrDepDelay using the arrange function
print(summarise(group_by(my_flights,HourOfDay),AvrDepDelay=mean(dep_delay)) %>% arrange(desc(AvrDepDelay)))
#Displaying the average departure delay statistics based on month using group_by and summarise functions and ordering it based on the AvrDepDelay using the arrange function
print(summarise(group_by(my_flights,Month),AvrDepDelay=mean(dep_delay)) %>% arrange(desc(AvrDepDelay)))
#Displaying the average departure delay statistics based on carrier using group_by and summarise functions and ordering it based on the AvrDepDelay using the arrange function
print(summarise(group_by(my_flights,carrier),AvrDepDelay=mean(dep_delay)) %>% arrange(desc(AvrDepDelay)))
#Displaying the average departure delay statistics based on airport,month using group_by and summarise functions and ordering it based on the AvrDepDelay using the arrange function
print(summarise(group_by(my_flights,origin,Month),AvrDepDelay=mean(dep_delay)) %>% arrange(desc(AvrDepDelay)))

# Adding a new variable DaySection based on the given conditions using case_when() function and mutate function and using select function to order in the required way
my_flights <- my_flights %>% mutate(DaySection=case_when(my_flights$HourOfDay>=5&my_flights$HourOfDay<12 ~ "Morning",
                                            my_flights$HourOfDay>=12&my_flights$HourOfDay<18 ~ "Afternoon",
                                            my_flights$HourOfDay>=18 ~ "Evening")) %>% select(time_hour,DaySection,everything())

print(my_flights)

# Creating a boxplot to plot departure delays(<60) against DayofWeek. Using ggplot for this, subset is used to restrict the plot for departure delays(<60), color is used to plot on the basis of DaySection. geom_boxplot is used to plot the graph
print(ggplot(subset(my_flights,dep_delay<60),aes(x=DayOfWeek,y=dep_delay,color=DaySection)) + geom_boxplot())
