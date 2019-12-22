set.seed(1000)


dice1<-sample(1:6,1000,repl=T)
print(table(dice1))

dice2<-sample(1:6,1000,repl=T)
print(table(dice2))


total<-vector(mode="integer",length=1000)
total<-dice1+dice2
print(table(total))


ans1<-vector(mode="integer",length=12)
for(i in total){
  ans1[i] <- ans1[i]+1
}
ans<-ans1[-1]
names(ans) = c(2:12)
print(ans)

print(names(ans))


evens=ans[c(2:12)%%2==0]
print(evens)


odds=ans[c("3","5","7","9","11")]
print(odds)
