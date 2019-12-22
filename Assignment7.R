#Loading all the required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(date)
library(stringr)
library(ggplot2)
library(corrplot)
library(readxl)

#Reading the DublinInward(flights coming into Dublin) details from the Excel file
d_in <- read_excel("DublinAirport.xlsx",2)
#Displaying the 10X10 Dublin inward tibble
print(d_in[1:10,1:10])
#Reading the DublinOutward(flights leaving Dublin) details from the Excel file
d_out <- read_excel("DublinAirport.xlsx",3)
#Displaying the 10X10 Dublin outward tibble
print(d_out[1:10,1:10])

#Using the gather function to convert the untidy data(d_in) to tidy_data(d_in_tidy) creating a new variable "Passengers" 
d_in_tidy <- gather(d_in,key=Airport,value=Passengers,	"Aberdeen (ABZ),Great Britain":"Dubrovnik (DBV),Croatia",convert = TRUE)
#Converting the Passengers variable to integer type as it is needed in integer format (convert = TRUE did not convert it to integer)
d_in_tidy$Passengers <- as.integer(d_in_tidy$Passengers)
#The first column is converted to one of the data formats supported by lubridate to convert it to date
d_in_tidy[,1] <- mutate_if(d_in_tidy[,1], 
                           is.character, 
                           str_replace_all, pattern = "M", replacement = "01")
#Providing the name to gthe first column a Date
colnames(d_in_tidy)[1]<-c("Date")
#Converting the date to the required format using lubridate
d_in_tidy[,1] <- ydm(d_in_tidy$Date)
#Using the mutate function to to add new variable Year, Month and MonthName and Direction(set to Inbound as it is Inbond Flights data) and using select function to order the data in the required manner
d_in_tidy <- d_in_tidy %>% mutate(Year=as.character(year(Date)),Month=format(Date,"%m"),MonthName = as.factor(month.abb[month(Date)]),Direction="Inbound") %>% select(Year,Month,Airport,Passengers,Direction,MonthName,Date)
print(d_in_tidy)

#We do the same operations on d_out to generate d_out_tidy. Only difference is that Direction is changed to Outbound
d_out_tidy <- gather(d_out,key=Airport,value=Passengers,	"Aberdeen (ABZ),Great Britain":"Dubrovnik (DBV),Croatia",convert = TRUE)
d_out_tidy$Passengers <- as.integer(d_out_tidy$Passengers)  
d_out_tidy[,1] <- mutate_if(d_out_tidy[,1], 
                            is.character, 
                            str_replace_all, pattern = "M", replacement = "01")

colnames(d_out_tidy)[1]<-c("Date")
d_out_tidy[,1] <- ydm(d_out_tidy$Date)
d_out_tidy <- d_out_tidy %>% mutate(Year=as.character(year(Date)),Month=format(Date,"%m"),MonthName = as.factor(month.abb[month(Date)]),Direction="Outbound") %>% select(Year,Month,Airport,Passengers,Direction,MonthName,Date)
print(d_out_tidy)

#Combining d_in_tidy and d_out_tidy to full_tidy
full_tidy = rbind(d_in_tidy,d_out_tidy)
print(full_tidy)

#Using summarise and group_by to group based on Airport and getting the number of Passengers and 
#arranging it in descending order using the arrange function and then selecting all the Airports using select function
top_ten = (summarise(group_by(full_tidy,Airport),Passengers1 = sum(Passengers,na.rm = T)) %>%
             arrange(desc(Passengers1)) %>% select(Airport))

#Getting the top ten airports as a vector
top_ten <- as.vector(top_ten$Airport[1:10])
print(top_ten)

#Using summarise and group_by to group based on Yearly passenger values based on Direction and calculating sum 
summ_year = (summarise(group_by(full_tidy,Year,Direction),Total = sum(Passengers,na.rm = T)) %>% select(Year,Direction,Total))
print(summ_year)

#Using summarise and group_by to group based on monthly passenger values based on Direction and calculating mean
avr_month <- summarise(group_by(full_tidy,Year,Date,Month,Direction),Total = mean(Passengers, na.rm = TRUE)) %>% select(Year,Month,Direction,Total,Date)
print(avr_month)

#Using summarise and group_by to group based on monthly passenger values based on Direction and calculating sum and filtering for the Year 2019
Total_pass <- summarise(group_by(d_out_tidy,Year,Month),Total = sum(Passengers,na.rm = T)) %>% filter(Year!="2019")
print(Total_pass)

#Creating a heat map for the Total_pass based on the Month and Year using ggplot, geom_tile and scal_fill_gradient
print(ggplot(Total_pass, aes(Year, Month)) + geom_tile(aes(fill = Total)) + scale_fill_gradient(low = "blue", high = "red"))

#Creating the correlation matrix using the cor() function
M <- d_out %>% select(-Date) %>% cor()
#Using gregexpr changing the column and row names to their respective acronym
cnames <- regmatches(colnames(M), gregexpr("(?<=\\().*?(?=\\))", colnames(M), perl=T)) 
colnames(M) <- cnames
rownames(M) <- cnames

#Building a corrplot using the correlation matrix in the function corrplot
corrplot(M, type="upper", method = "circle")
