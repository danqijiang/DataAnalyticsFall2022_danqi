help("read.csv")
#load data_EPI_2010
data_2010EPI<-read.csv(file.choose(), header = TRUE)
data_2010EPI
View(data_2010EPI)
attach(data_2010EPI)
fix(data_2010EPI)
