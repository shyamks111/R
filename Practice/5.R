# Working with ggplot2


library(readxl)
library(ggplot2)

orig_list <- data.frame(readxl::read_excel("titanic3_assignment.xls"))
plist <- orig_list
print(dim(plist))
print(summary(plist))

#Converting the survived variable to Logical values using ifelse
plist[,"survived"]<-ifelse(plist[,"survived"]==0,FALSE,TRUE)
print(summary(plist))

#Changing class variable to character using the sapply function
plist$pclass <- sapply(plist$pclass,function(x){if(x==1){x="First"}else if(x==2){x="Second"}else if(x==3){x="Third"}else{x}})
print(summary(plist))
print(unique(plist$pclass))

#Setting the NA values in age variable with the mean of the age using ifelse
plist$age <- ifelse(is.na(plist$age),mean(plist$age,na.rm=TRUE),plist$age)
print(summary(plist))

#Setting the NA values in fare variable with the mean of the fare using ifelse
plist$fare <- ifelse(is.na(plist$fare),mean(plist$fare,na.rm=TRUE),plist$fare)
print(summary(plist))

#Setting the seed to 99
set.seed(99)

#Setting the NA values in embarked variable by randomly selecting the values of remaining samples in the embarked variable
plist$embarked <- (sapply(plist$embarked,function(x){if(is.na(x)){x=unlist(sample(plist$embarked[!is.na(plist$embarked)],1,replace = TRUE))}else{x=x}}))

#Converting the embarked variable to a vector
plist$embarked <- as.vector(plist$embarked)
print(summary(plist))
print(unique(plist$embarked))

#Creating the new category age_cohort depending on the age of the person
plist$age_cohort <- sapply(plist$age,function(x){if(x<16){x="Child"}else if(x>=16&&x<60){x="Adult"}else {x="Elderly"}})
print(summary(plist))

#Naming the embarked variable with the full name of the city
plist$embarked <- sapply(plist$embarked, function(x){if(x=="S"){x="Southampton"}else if(x=="C"){x="Cherbourg"} else if(x=="Q"){x="Cobh"} else{x}})
print(summary(plist))
print(plist$embarked)

#Checking the dataset
print(head(plist))
print(dim(plist))
print(table(plist$survived))
print(table(plist$survived,plist$age_cohort))
print(table(plist$survived,plist$sex))
print(table(plist$survived,plist$pclass))
print(table(plist$survived,plist$embarked))


#Plots

#Plot1 to Plot4: Using ggplot to plot stacked bar charts of number of people who survived based on Travel Class, Age Cohort, Gennder and Embarkation Location
#                In addition to geom bar xlab,ylab,labs and theme functions are used to achieve this

#Plot1
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=pclass))+xlab("Survived")+ylab("Number")+labs(title="Survival Numbers by Travel Class")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot2
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=sex))+xlab("Survived")+ylab("Number")+labs(title="Survival Numbers by Gender")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot3
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=age_cohort))+xlab("Survived")+ylab("Number")+labs(title="Survival Numbers by Age Cohort")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot4
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=embarked))+xlab("Survived")+ylab("Number")+labs(title="Survival Numbers by Embarkation Location")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot5 to Plot8: Using ggplot to plot stacked bar charts of proportion of people who survived based on Travel Class, Age Cohort, Gennder and Embarkation Location
#                In addition to geom bar(position = "fill" is used for obtaining proportions ) xlab,ylab,labs and theme functions are used to achieve this

#Plot5
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=pclass),position="fill")+xlab("Survived")+ylab("Proportion")+labs(title="Survival Proportions by Embarkation Location")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot6
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=sex),position="fill")+xlab("Survived")+ylab("Proportion")+labs(title="Survival Proportions by Gender")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot7
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=age_cohort),position="fill")+xlab("Survived")+ylab("Proportion")+labs(title="Survival Proportions by Age Cohort")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot8
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=embarked),position="fill")+xlab("Survived")+ylab("Proportion")+labs(title="Survival Proportions by place of Embarkation")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot9: Using ggplot to plot stacked bar charts of number of people who survived based on Travel Class and Age Cohort
#       In addition to geom bar facet_grid,xlab,ylab,labs and theme functions are used to achieve this
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=age_cohort))+facet_grid(~pclass)+xlab("Survived")+ylab("Number")+labs(title="Survival Numbers by Cohort and Travel Class")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot10: Using ggplot to plot stacked bar charts of number of people who survived based on Travel Class and Gender
#       In addition to geom bar facet_grid,xlab,ylab,labs and theme functions are used to achieve this
print(ggplot(data=plist)+geom_bar(aes(x=survived,fill=sex))+facet_grid(~pclass)+xlab("Survived")+ylab("Number")+labs(title="Survival Numbers by Gender and Travel Class")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot11: Using ggplot to plot scatterplot of age vs fare based on Place of Embarkment
#       In addition to geom point xlab,ylab,labs and theme functions are used to achieve this
print(ggplot(data=plist)+geom_point(aes(x=age,y=fare,colour=embarked))+xlab("Age")+ylab("Fare")+labs(title="Age v Fare by Place of Embarkation")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot12: Using ggplot to plot scatterplot of age vs fare. Using the geom_smooth with method=lm to get the linear model
#       In addition to geom point xlab,ylab,labs and theme functions are used to achieve this
print(ggplot(data=plist)+geom_point(aes(x=age,y=fare))+geom_smooth(method="lm",aes(x=age,y=fare))+xlab("Age")+ylab("Fare")+labs(title="Age v Fare with Linear Model")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot13: Using ggplot to plot scatterplot of age vs fare based on Survival info
#       In addition to geom point xlab,ylab,labs and theme functions are used to achieve this
print(ggplot(data=plist)+geom_point(aes(x=age,y=fare,colour=survived))+xlab("Age")+ylab("Fare")+labs(title="Age v Fare with Survival Info")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm')))

#Plot14: Using ggplot to plot scatterplot of age vs fare based on Travel class and Point of Departure
#       In addition to geom point facet_grid,xlab,ylab,labs and theme functions are used to achieve this
ggplot(data=plist)+geom_point(aes(x=age,y=fare,colour=embarked))+facet_grid(~pclass)+xlab("Age")+ylab("Fare")+labs(title="Age v Fare By Travel Class and Point of Departure")+theme(legend.title=element_blank(),legend.position="top", legend.spacing.x = unit(0.1, 'mm'))

