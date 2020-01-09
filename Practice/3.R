
set.seed(100)

# Creating a normal distributions for 20 students for 5 subjects 
CX101 <- rnorm(20,45,8)
CX102 <- rnorm(20,65,8)
CX103 <- rnorm(20,85,10)
CX104 <- rnorm(20,45,10)
CX105 <- rnorm(20,60,5)

# Creating matrix for different students v different subjects 
res<-matrix(c(CX101,CX102,CX103,CX104,CX105),nrow=20)
# Setting the column names
colnames(res) <- c("CX101","CX102","CX103","CX104","CX105")
# Setting the rownames as Student_1,Student_2... and so on
students<-sprintf("Student_%s", seq(1:20))
rownames(res) <- c(students)

# Displaying the whole matrix
print(summary(res))
cat("\n")

# Displaying marks of students 1,2 and 20 in all the subjects
print(res[c(1,2,20),])
cat("\n")

# Displaying marks of all the students in the subject CX103
print(res[res[,"CX103"] > 100,])
cat("\n")

# Using apply function to repopulate the outliers as NA
res<-apply(res,2,function(m){ifelse(m>100,NA,m)})
res<-apply(res,2,function(m){ifelse(m<0,NA,m)})


print(res)
cat("\n")

# Replacing NA's with mean of the score of that subject
res<-apply(res,2,function(m){ifelse(is.na(m),mean(m,na.rm=T),m)})
print(res)
cat("\n")

# Creating columns Mean and Range to add them to the matrix with the Mean and Range
Mean<-apply(res,1,mean)
student_range<-apply(res,1,range)
Range<-apply(student_range,2,diff)
res<-cbind(res,Mean,Range)
print(res)
cat("\n")

# Displaying the marks of the students with the maximum mean
a<-res[,"Mean"]
b<-max(a)
c<-res[res[,"Mean"]==b,,drop=F]
print(c)
