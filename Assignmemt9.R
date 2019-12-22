# Importing the tidyverse library
library(tidyverse)
# Creating function my_timer which returns a list of functions
my_timer<-function(){
  # Initialising the variables
  name=NULL
  start_time=NULL
  finish_time=NULL 
  duration=NULL  
  all_times = tibble(Name=character(0),StartTime=character(0),FinishTime=character(0),Duration=double(0))
  
  # Function start sets the name to the input name and also the start_time to the time of execution
  start<-function(sname="Unknown"){
    name<<-sname
    start_time<<-Sys.time()
  }
  # Function archive updates the tibble all_times with the required values 
  archive<-function(){
    all_times <<- add_row(all_times,Name=name,
                          StartTime = as.character(start_time),
                          FinishTime = as.character(finish_time),
                          Duration = duration)
  }
  # Function finish sets the finish_time and also calculates the duration and also calls the archive function
  finish<-function(){
    finish_time<<-Sys.time()
    duration <<- as.double(difftime(finish_time,start_time))
    archive()
  }
  # Function get_time gets the difference between finish and start time
  get_time<-function(){(difftime(finish_time,start_time))}
  # Function summary gives the summary as a list of Name, StartTime and FinishTime
  summary<-function(){
    list(Name=name, StartTime=start_time, FinishTime=finish_time)
  }
  # Function get_all_times returns the all_times tibble
  get_all_times<-function(){
    (all_times)
  }
  # my_timer returns the list of all the above defined functions
  list(start=start,finish=finish,get_time=get_time,summary=summary,archive=archive,get_all_times=get_all_times)
}

# Calling the function my_timer which returns a list of functions which is assigned to t
t <- my_timer()
# Printing the structure of t
cat(str(t))
# Calling the start function of t with name="Person1"
t$start("Person1")
# Sleep for 3 seconds
Sys.sleep(3)
# Calling the finish function of t
t$finish()
# Calling the get_time function of t
print(t$get_time())
# Calling the get_time function of t
print(t$summary())
# Repeating the above process for "Person2"
t$start("Person2")
Sys.sleep(2)
t$finish()
print(t$get_time())
print(t$summary())
# Print all the data
print(t$get_all_times())
