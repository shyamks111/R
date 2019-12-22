# Creating a new function rep_lm which takes the required inputs and gives an output which is of the class "rep_lm" which inherits from the "lm" class
rep_lm <- function(model_name, equation, df){
  # Using the lm function to get the linear model
  m <- lm(as.formula(equation),get(df))
  #Adding the new field Information with the specified List of Data
  m$Information <- list("Name" = model_name,
                        "DateRun" = date(),
                        "LinearModel" = equation,
                        "DataSource" = df,
                        "Columns" = colnames(m$model),
                        "Observations" = nrow(m$model))
  #Setting m as the object  of "rep_lm" which inherits from "lm"
  structure(m,class=c("rep_lm","lm"))
}

#Calling the rep_lm function with the given data
m <- rep_lm("My Model","eruptions~waiting","faithful")

#Printing the structure of the object m
cat(str(m))
cat("\n")

# Creating a new summary function for the objects of the class rep_lm
summary.rep_lm <- function(m1){
  # Adding the necessary items as specified and displaying it using the cat function
  cat("(1) rep_lm class summary \n\n")
  cat("Model Name:", m1$Information$Name, "\t", "Date of Run:", m1$Information$DateRun,"\n\n")
  cat("Linear Model:", m1$Information$LinearModel, "  ", "DataSource:", m1$Information$DataSource,"\n\n")
  cat(paste("Columns:", m1$Information$Columns),sep="\n")
  cat("\n")
  cat("Observations:" , m1$Information$Observations)
  cat("\n\n")
  
  cat("(2) lm class summary \n")
  print(summary.lm(m1))
  
} 

#Printing the summary
summary(m)
