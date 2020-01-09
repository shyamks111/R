# Understanding Dataframes

set.seed(100)
a<-ggplot2::mpg
f<-ggplot2::mpg   # Needed finally to compare with 'a'
sel_rows = sample(nrow(a),10) # Selecting 10 rows at random
print (sel_rows)
a=as.data.frame(a)
a[sel_rows[1:10],"cty"]<--999 # Setting the "cty" of selected rows to -999
print(a[sel_rows,]) 
a=lapply(a, function(x) {ifelse(x==-999,NA,x)}) # Using lapply to set all -999 attributes to NA
a=as.data.frame(a)
print(a[sel_rows,])
#Using aggregate function to calculate the mean and storing it in 'class_cty'
class_cty=aggregate(a$cty,list(a$class),mean, na.rm=TRUE)
print(class_cty)
#Using match function to get the indices in 'class_cty' where cty is NA in dataframe 'a' 
indices = match(a$class[is.na(a$cty)],class_cty$Group.1)
# Replacing the NA values in cty with the means('x') based on the indices 
a$cty[is.na(a$cty)] <- class_cty$x[indices]
print(a[sel_rows,])
print(summary(a))
print(mean(f$cty))
print(mean(a$cty))




