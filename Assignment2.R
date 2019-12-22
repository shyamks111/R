mod<-lm(eruptions ~ waiting, data=faithful)
get_data<-function(list_mod,var){
  list_mod$model[[var]]
}
d<-get_data(mod,"waiting")
print(d[1:20])
get_coefficient<-function(list_mod,coeff){
  list_mod$coefficients[coeff]
}
c<-get_coefficient(mod,"waiting")
print(c)
mod_summary<-function(list_mod){
  ##b<-c("Rows" = nrow(list_mod$model),"Cols" = ncol(list_mod$model))
  ##names(b) <- c("Rows","Cols")
  a<-list(call=list_mod$call,coeff=list_mod$coefficients,DataSize=
            c("Rows" = nrow(list_mod$model),"Cols" = ncol(list_mod$model)))
}
s<-mod_summary(mod)
print(s)