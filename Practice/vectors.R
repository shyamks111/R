# Atomic Vectors

set.seed(1000)

# Random 1000 selections for the dice 1 using sample
dice1<-sample(1:6,1000,repl=T)
print(table(dice1))

# Random 1000 selections for the dice 2 using sample
dice2<-sample(1:6,1000,repl=T)
print(table(dice2))

# Sum of outcomes from both the dice
total<-vector(mode="integer",length=1000)
total<-dice1+dice2
print(table(total))

# Calculating the number of times dice totals have appeared(2 to 12)
ans1<-vector(mode="integer",length=12)
for(i in total){
  ans1[i] <- ans1[i]+1
}
# As sum can never be 1
ans<-ans1[-1]
names(ans) = c(2:12)
print(ans)

# Prints the number of times each total(2:12) have appeared
print(names(ans))

# Prints the number of times even totals have appeared
evens=ans[c(2:12)%%2==0]
print(evens)

# Prints the number of times odd totals have appeared
odds=ans[c("3","5","7","9","11")]
print(odds)
