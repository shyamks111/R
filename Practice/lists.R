# Lists in R

# Linear Model as a list
mod<-lm(eruptions ~ waiting, data=faithful)

# Function to get the model for the given variable
get_data<-function(list_mod,var){
  list_mod$model[[var]]
}

d<-get_data(mod,"waiting")
print(d[1:20])

# Function to get the model for the given variable
get_coefficient<-function(list_mod,coeff){
  list_mod$coefficients[coeff]
}

c<-get_coefficient(mod,"waiting")
print(c)

# Creating a new summary function for the given list with the below mentioned variables
mod_summary<-function(list_mod){
  a<-list(call=list_mod$call,coeff=list_mod$coefficients,DataSize=
            c("Rows" = nrow(list_mod$model),"Cols" = ncol(list_mod$model)))
}

s<-mod_summary(mod)
print(s)
