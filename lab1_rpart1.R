require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

rpart.plot(Swiss_rpart,branch=1,type=2, fallen.leaves=T,cex=0.8)
text(Swiss_rpart,all = T,use.n = T)

