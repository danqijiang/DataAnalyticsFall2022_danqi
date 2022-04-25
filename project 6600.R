data1<-read.csv(file.choose(), header = TRUE)

#check missing value data1
missing_data=data.frame(lapply(data1,function(x)sum(is.na(x))))
missing_data=t(missing_data);missing_data
sum(data1=="", na.rm=TRUE)
sum(data1=="N/A", na.rm=TRUE)
sum(data1=="NA", na.rm=TRUE)

dim(data1)
#drop miising value data1
data3 <- na.omit(data1)
missing_data=data.frame(lapply(data2,function(x)sum(is.na(x))))
missing_data=t(missing_data);missing_data

dim(data3)


#checking value 
summary(data3)
fivenum(data3$value_2000)


#checking value by using boxplot AND #outlier
boxplot(data3$value_2000,data3$value_2001,data3$value_2002,data3$value_2003,data3$value_2004,data3$value_2005,data3$value_2006,data3$value_2007,data3$value_2008,data3$value_2009,data3$value_2010,data3$value_2011,data3$value_2012,data3$value_2013,data3$value_2014,data3$value_2015,data3$value_2016,data3$value_2017)

Q1value_2000 <- quantile(data3$value_2000, 0.25)
Q3value_2000 <- quantile(data3$value_2000, 0.75)
IQRvalue_2000<-IQR(data3$value_2000)

Q1value_2001 <- quantile(data3$value_2001, 0.25)
Q3value_2001 <- quantile(data3$value_2001, 0.75)
IQRvalue_2001<-IQR(data3$value_2001)

Q1value_2002 <- quantile(data3$value_2002, 0.25)
Q3value_2002 <- quantile(data3$value_2002, 0.75)
IQRvalue_2002<-IQR(data3$value_2002)

Q1value_2003 <- quantile(data3$value_2003, 0.25)
Q3value_2003 <- quantile(data3$value_2003, 0.75)
IQRvalue_2003<-IQR(data3$value_2003)

Q1value_2004 <- quantile(data3$value_2004, 0.25)
Q3value_2004 <- quantile(data3$value_2004, 0.75)
IQRvalue_2004<-IQR(data3$value_2004)

Q1value_2005 <- quantile(data3$value_2005, 0.25)
Q3value_2005 <- quantile(data3$value_2005, 0.75)
IQRvalue_2005<-IQR(data3$value_2005)

Q1value_2006 <- quantile(data3$value_2006, 0.25)
Q3value_2006 <- quantile(data3$value_2006, 0.75)
IQRvalue_2006<-IQR(data3$value_2006)

Q1value_2007 <- quantile(data3$value_2007, 0.25)
Q3value_2007 <- quantile(data3$value_2007, 0.75)
IQRvalue_2007<-IQR(data3$value_2007)

Q1value_2008 <- quantile(data3$value_2008, 0.25)
Q3value_2008 <- quantile(data3$value_2008, 0.75)
IQRvalue_2008<-IQR(data3$value_2008)

Q1value_2009 <- quantile(data3$value_2009, 0.25)
Q3value_2009 <- quantile(data3$value_2009, 0.75)
IQRvalue_2009<-IQR(data3$value_2009)

Q1value_2010 <- quantile(data3$value_2010, 0.25)
Q3value_2010 <- quantile(data3$value_2010, 0.75)
IQRvalue_2010<-IQR(data3$value_2010)

Q1value_2011 <- quantile(data3$value_2011, 0.25)
Q3value_2011 <- quantile(data3$value_2011, 0.75)
IQRvalue_2011<-IQR(data3$value_2011)

Q1value_2012 <- quantile(data3$value_2012, 0.25)
Q3value_2012 <- quantile(data3$value_2012, 0.75)
IQRvalue_2012<-IQR(data3$value_2012)

Q1value_2013 <- quantile(data3$value_2013, 0.25)
Q3value_2013 <- quantile(data3$value_2013, 0.75)
IQRvalue_2013<-IQR(data3$value_2013)

Q1value_2014 <- quantile(data3$value_2014, 0.25)
Q3value_2014 <- quantile(data3$value_2014, 0.75)
IQRvalue_2014<-IQR(data3$value_2014)

Q1value_2015 <- quantile(data3$value_2015, 0.25)
Q3value_2015 <- quantile(data3$value_2015, 0.75)
IQRvalue_2015<-IQR(data3$value_2015)

Q1value_2016 <- quantile(data3$value_2016, 0.25)
Q3value_2016 <- quantile(data3$value_2016, 0.75)
IQRvalue_2016<-IQR(data3$value_2016)

Q1value_2017 <- quantile(data3$value_2017, 0.25)
Q3value_2017 <- quantile(data3$value_2013, 0.75)
IQRvalue_2017<-IQR(data3$value_2013)



dim(data3)
eliminated <- subset(data3, data3$value_2000 > (Q1value_2000 - 1.5*IQRvalue_2000)
                     & data3$value_2000 < (Q3value_2000 + 1.5*IQRvalue_2000))

eliminated1 <- subset(eliminated, eliminated $value_2001 > (Q1value_2001 - 1.5*IQRvalue_2001)
                     & eliminated $value_2001 < (Q3value_2001 + 1.5*IQRvalue_2001))

eliminated2 <- subset(eliminated1,eliminated1$value_2002 > (Q1value_2002 - 1.5*IQRvalue_2002)
                     & eliminated1$value_2002 < (Q3value_2002 + 1.5*IQRvalue_2002))

eliminated3 <- subset(eliminated2, eliminated2$value_2003 > (Q1value_2003 - 1.5*IQRvalue_2003)
                     & eliminated2$value_2003 < (Q3value_2003 + 1.5*IQRvalue_2003))

eliminated4 <- subset(eliminated3, eliminated3$value_2004 > (Q1value_2004 - 1.5*IQRvalue_2004)
                     & eliminated3$value_2004 < (Q3value_2004 + 1.5*IQRvalue_2004))

eliminated5 <- subset(eliminated4, eliminated4$value_2005 > (Q1value_2005 - 1.5*IQRvalue_2005)
                     & eliminated4$value_2005 < (Q3value_2005 + 1.5*IQRvalue_2005))

eliminated6 <- subset(eliminated5, eliminated5$value_2006 > (Q1value_2006 - 1.5*IQRvalue_2006)
                     & eliminated5$value_2006 < (Q3value_2006 + 1.5*IQRvalue_2006))

eliminated7 <- subset(eliminated6, eliminated6$value_2007 > (Q1value_2007 - 1.5*IQRvalue_2007)
                     & eliminated6$value_2007 < (Q3value_2007 + 1.5*IQRvalue_2007))

eliminated8 <- subset(eliminated7, eliminated7$value_2008 > (Q1value_2008 - 1.5*IQRvalue_2008)
                     & eliminated7$value_2008 < (Q3value_2008 + 1.5*IQRvalue_2008))

eliminated9 <- subset(eliminated8, eliminated8$value_2009 > (Q1value_2009 - 1.5*IQRvalue_2009)
                     & eliminated8$value_2009 < (Q3value_2009 + 1.5*IQRvalue_2009))

eliminated10 <- subset(eliminated9, eliminated9$value_2010 > (Q1value_2010 - 1.5*IQRvalue_2010)
                     & eliminated9$value_2010 < (Q3value_2010 + 1.5*IQRvalue_2010))

eliminated11 <- subset(eliminated10, eliminated10$value_2011 > (Q1value_2011 - 1.5*IQRvalue_2011)
                     & eliminated10$value_2011 < (Q3value_2011 + 1.5*IQRvalue_2011))

eliminated12 <- subset(eliminated11, eliminated11$value_2012 > (Q1value_2012 - 1.5*IQRvalue_2012)
                     & eliminated11$value_2012 < (Q3value_2012 + 1.5*IQRvalue_2012))

eliminated13 <- subset(eliminated12, eliminated12$value_2013 > (Q1value_2013 - 1.5*IQRvalue_2013)
                     & eliminated12$value_2013 < (Q3value_2013 + 1.5*IQRvalue_2013))

eliminated14 <- subset(eliminated13, eliminated13$value_2014 > (Q1value_2014 - 1.5*IQRvalue_2014)
                     & eliminated13$value_2014 < (Q3value_2014 + 1.5*IQRvalue_2014))

eliminated15 <- subset(eliminated14, eliminated14$value_2015 > (Q1value_2015 - 1.5*IQRvalue_2015)
                     & eliminated14$value_2015 < (Q3value_2015 + 1.5*IQRvalue_2015))

eliminated16 <- subset(eliminated15, eliminated15$value_2016 > (Q1value_2016 - 1.5*IQRvalue_2016)
                       & eliminated15$value_2016 < (Q3value_2016 + 1.5*IQRvalue_2016))

eliminated17 <- subset(eliminated16, eliminated16$value_2017 > (Q1value_2017 - 1.5*IQRvalue_2017)
                       & eliminated16$value_2017 < (Q3value_2017 + 1.5*IQRvalue_2017))
dim(eliminated17)

boxplot(eliminated17$value_2000,eliminated17$value_2001,eliminated17$value_2002,eliminated17$value_2003)


attach(mtcars) 
par(mfrow=c(1,2)) 
boxplot(data3$value_2000,data3$value_2001,data3$value_2002,data3$value_2003,data3$value_2004,data3$value_2005,data3$value_2006,data3$value_2007,data3$value_2008,data3$value_2009,data3$value_2010,data3$value_2011,data3$value_2012,data3$value_2013,data3$value_2014,data3$value_2015,data3$value_2016,data3$value_2017)
boxplot(eliminated17$value_2000,eliminated17$value_2001,eliminated17$value_2002,eliminated17$value_2003,eliminated17$value_2004,
        eliminated17$value_2005,eliminated17$value_2006,eliminated17$value_2007,eliminated17$value_2008,eliminated17$value_2009,
        eliminated17$value_2010,eliminated17$value_2011,eliminated17$value_2012,eliminated17$value_2013,eliminated17$value_2014,
        eliminated17$value_2015,eliminated17$value_2016,eliminated17$value_2017)
#boxplot(data2$value_2000,data2$value_2001,data2$value_2002,data2$value_2003,data2$value_2004,data2$value_2005,data2$value_2006,data2$value_2007,data2$value_2008,data2$value_2009,data2$value_2010,data2$value_2011,data2$value_2012,data2$value_2013,data2$value_2014,data2$value_2015,data2$value_2016,data2$value_2017, plot=FALSE)$out
#dim(data2$value_2000)
#outliers_value <- boxplot(data2$value_2000,data2$value_2001,data2$value_2002,data2$value_2003,data2$value_2004,data2$value_2005,data2$value_2006,data2$value_2007,data2$value_2008,data2$value_2009,data2$value_2010,data2$value_2011,data2$value_2012,data2$value_2013,data2$value_2014,data2$value_2015,data2$value_2016,data2$value_2017, plot=FALSE)$out
#x<-data2
##x<- x[-which(x$breaks %in% outliers_value),]
#boxplot(data2$value_2000,data2$value_2001,data2$value_2002,data2$value_2003,data2$value_2004,data2$value_2005,data2$value_2006,data2$value_2007,data2$value_2008,data2$value_2009,data2$value_2010,data2$value_2011,data2$value_2012,data2$value_2013,data2$value_2014,data2$value_2015,data2$value_2016,data2$value_2017)
#boxplot(data2$value_2000,data2$value_2001,data2$value_2002,data2$value_2003,data2$value_2004,data2$value_2005,data2$value_2006,data2$value_2007,data2$value_2008,data2$value_2009,data2$value_2010,data2$value_2011,data2$value_2012,data2$value_2013,data2$value_2014,data2$value_2015,data2$value_2016,data2$value_2017, outline = F)
#Q1 <- quantile(data2$value_2000, .25)
#3 <- quantile(data2$value_2000, .75)
#IQR <- IQR(data2$value_2000)
#no_outliers <- subset(data2, data2$value_2000 > (Q1 - 1.5*IQR) & data2$value_2000 < (Q3 + 1.5*IQR))
#im(data2$value_2000)

boxplot(eliminated17$G2000,eliminated17$G2001,eliminated17$G2002,eliminated17$G2003,eliminated17$G2004,eliminated17$G2005,
        eliminated17$G2006,eliminated17$G2007,eliminated17$G2008,eliminated17$G2009,eliminated17$G2010,eliminated17$G2011,
        eliminated17$G2012,eliminated17$G2013,eliminated17$G2014,eliminated17$G2015,eliminated17$G2016,eliminated17$G2017 )


Q1G2000 <- quantile(eliminated17$G2000, 0.25)
Q3G2000 <- quantile(eliminated17$G2000, 0.75)
IQRG2000<-IQR(eliminated17$G2000)

Q1G2001 <- quantile(eliminated17$G2001, 0.25)
Q3G2001 <- quantile(eliminated17$G2001, 0.75)
IQRG2001<-IQR(eliminated17$G2001)

Q1G2002 <- quantile(eliminated17$G2002, 0.25)
Q3G2002 <- quantile(eliminated17$G2002, 0.75)
IQRG2002<-IQR(eliminated17$G2002)

Q1G2003 <- quantile(eliminated17$G2003, 0.25)
Q3G2003 <- quantile(eliminated17$G2003, 0.75)
IQRG2003<-IQR(eliminated17$G2003)

Q1G2004 <- quantile(eliminated17$G2004, 0.25)
Q3G2004 <- quantile(eliminated17$G2004, 0.75)
IQRG2004<-IQR(eliminated17$G2004)

Q1G2005 <- quantile(eliminated17$G2005, 0.25)
Q3G2005 <- quantile(eliminated17$G2005, 0.75)
IQRG2005<-IQR(eliminated17$G2005)

Q1G2006 <- quantile(eliminated17$G2006, 0.25)
Q3G2006 <- quantile(eliminated17$G2006, 0.75)
IQRG2006<-IQR(eliminated17$G2006)

Q1G2007 <- quantile(eliminated17$G2007, 0.25)
Q3G2007 <- quantile(eliminated17$G2007, 0.75)
IQRG2007<-IQR(eliminated17$G2007)

Q1G2008 <- quantile(eliminated17$G2008, 0.25)
Q3G2008 <- quantile(eliminated17$G2008, 0.75)
IQRG2008<-IQR(eliminated17$G2008)

Q1G2009 <- quantile(eliminated17$G2009, 0.25)
Q3G2009 <- quantile(eliminated17$G2009, 0.75)
IQRG2009<-IQR(eliminated17$G2009)

Q1G2010 <- quantile(eliminated17$G2010, 0.25)
Q3G2010 <- quantile(eliminated17$G2010, 0.75)
IQRG2010<-IQR(eliminated17$G2010)

Q1G2011 <- quantile(eliminated17$G2011, 0.25)
Q3G2011 <- quantile(eliminated17$G2011, 0.75)
IQRG2011<-IQR(eliminated17$G2011)

Q1G2012 <- quantile(eliminated17$G2012, 0.25)
Q3G2012 <- quantile(eliminated17$G2012, 0.75)
IQRG2012<-IQR(eliminated17$G2012)

Q1G2013 <- quantile(eliminated17$G2013, 0.25)
Q3G2013 <- quantile(eliminated17$G2013, 0.75)
IQRG2013<-IQR(eliminated17$G2013)

Q1G2014 <- quantile(eliminated17$G2014, 0.25)
Q3G2014 <- quantile(eliminated17$G2014, 0.75)
IQRG2014<-IQR(eliminated17$G2014)

Q1G2015 <- quantile(eliminated17$G2015, 0.25)
Q3G2015 <- quantile(eliminated17$G2015, 0.75)
IQRG2015<-IQR(eliminated17$G2015)

Q1G2016 <- quantile(eliminated17$G2016, 0.25)
Q3G2016 <- quantile(eliminated17$G2016, 0.75)
IQRG2016<-IQR(eliminated17$G2016)

Q1G2017 <- quantile(eliminated17$G2017, 0.25)
Q3G2017 <- quantile(eliminated17$G2017, 0.75)
IQRG2017<-IQR(eliminated17$G2017)

eliminatedG1 <- subset(eliminated17, eliminated17$G2000 > (Q1G2000 - 1.5*IQRG2000)
                       & eliminated17$G2000 < (Q3G2000+ 1.5*IQRG2000))
eliminatedG2 <- subset(eliminatedG1, eliminatedG1$G2001 > (Q1G2001 - 1.5*IQRG2001)
                       & eliminatedG1$G2001 < (Q3G2001+ 1.5*IQRG2001))

eliminatedG3 <- subset(eliminatedG2, eliminatedG2$G2002 > (Q1G2002 - 1.5*IQRG2002)
                       & eliminatedG2$G2002 < (Q3G2002+ 1.5*IQRG2002))

eliminatedG4 <- subset(eliminatedG3, eliminatedG3$G2003 > (Q1G2003 - 1.5*IQRG2003)
                       & eliminatedG3$G2003 < (Q3G2003+ 1.5*IQRG2003))

eliminatedG5 <- subset(eliminatedG4, eliminatedG4$G2004 > (Q1G2004 - 1.5*IQRG2004)
                       & eliminatedG4$G2004 < (Q3G2004+ 1.5*IQRG2004))

eliminatedG6 <- subset(eliminatedG5, eliminatedG5$G2005 > (Q1G2005 - 1.5*IQRG2005)
                       & eliminatedG5$G2005 < (Q3G2005+ 1.5*IQRG2005))

eliminatedG7 <- subset(eliminatedG6, eliminatedG6$G2001 > (Q1G2006 - 1.5*IQRG2006)
                       & eliminatedG6$G2006 < (Q3G2006+ 1.5*IQRG2006))

eliminatedG8 <- subset(eliminatedG7, eliminatedG7$G2007 > (Q1G2007 - 1.5*IQRG2007)
                       & eliminatedG7$G2007 < (Q3G2007+ 1.5*IQRG2007))
eliminatedG9 <- subset(eliminatedG8, eliminatedG8$G2008 > (Q1G2008 - 1.5*IQRG2008)
                       & eliminatedG8$G2008 < (Q3G2008+ 1.5*IQRG2008))


eliminatedG10 <- subset(eliminatedG9, eliminatedG9$G2009 > (Q1G2009 - 1.5*IQRG2009)
                       & eliminatedG9$G2009 < (Q3G2009+ 1.5*IQRG2009))
eliminatedG11 <- subset(eliminatedG10, eliminatedG10$G2010 > (Q1G2010 - 1.5*IQRG2010)
                       & eliminatedG10$G2010 < (Q3G2010+ 1.5*IQRG2010))
eliminatedG12 <- subset(eliminatedG11, eliminatedG11$G2011 > (Q1G2011 - 1.5*IQRG2011)
                       & eliminatedG11$G2011 < (Q3G2011+ 1.5*IQRG2011))
eliminatedG13 <- subset(eliminatedG12, eliminatedG12$G2012 > (Q1G2012 - 1.5*IQRG2012)
                       & eliminatedG12$G2012 < (Q3G2012+ 1.5*IQRG2012))
eliminatedG14 <- subset(eliminatedG13, eliminatedG13$G2013 > (Q1G2013 - 1.5*IQRG2013)
                       & eliminatedG13$G2013 < (Q3G2013+ 1.5*IQRG2013))
eliminatedG15 <- subset(eliminatedG14, eliminatedG14$G2001 > (Q1G2014 - 1.5*IQRG2014)
                       & eliminatedG14$G2014 < (Q3G2014+ 1.5*IQRG2014))
eliminatedG16 <- subset(eliminatedG15, eliminatedG15$G2015 > (Q1G2015 - 1.5*IQRG2015)
                        & eliminatedG15$G2015 < (Q3G2015+ 1.5*IQRG2015))
eliminatedG17 <- subset(eliminatedG16, eliminatedG16$G2016 > (Q1G2016 - 1.5*IQRG2016)
                        & eliminatedG16$G2016 < (Q3G2016+ 1.5*IQRG2016))
eliminatedG18 <- subset(eliminatedG17, eliminatedG17$G2017 > (Q1G2017 - 1.5*IQRG2017)
                        & eliminatedG17$G2017 < (Q3G2017+ 1.5*IQRG2017))
dim(eliminatedG18)
boxplot(eliminatedG18$G2000,eliminatedG18$G2001,eliminatedG18$G2002,eliminatedG18$G2003,eliminatedG18$G2004,eliminatedG18$G2005,
        eliminatedG18$G2006,eliminatedG18$G2007,eliminatedG18$G2008,eliminatedG18$G2009,eliminatedG18$G2010,eliminatedG18$G2011,eliminatedG18$G2012,
        eliminatedG18$G2013,eliminatedG18$G2014,eliminatedG18$G2015,eliminatedG18$G2016,eliminatedG18$G2017)
      

boxplot(eliminatedG18$M2000,eliminatedG18$M2001,eliminatedG18$M2002,eliminatedG18$M2003,eliminatedG18$M2004,
        eliminatedG18$M2005,eliminatedG18$M2006,eliminatedG18$M2007,eliminatedG18$M2008,eliminatedG18$M2009
        ,eliminatedG18$M2010,eliminatedG18$M2011,eliminatedG18$M2012,eliminatedG18$M2013,eliminatedG18$M2014,
        eliminatedG18$M2015,eliminatedG18$M2016,eliminatedG18$M2017)

Q1M2000 <- quantile(eliminatedG18$M2000, 0.25)
Q3M2000 <- quantile(eliminatedG18$M2000, 0.75)
IQRM2000<-IQR(eliminatedG18$M2000)



Q1M2001 <- quantile(eliminatedG18$M2001, 0.25)
Q3M2001 <- quantile(eliminatedG18$M2001, 0.75)
IQRM2001<-IQR(eliminatedG18$M2001)


Q1M2002 <- quantile(eliminatedG18$M2002, 0.25)
Q3M2002 <- quantile(eliminatedG18$M2002, 0.75)
IQRM2002<-IQR(eliminatedG18$M2002)


Q1M2003 <- quantile(eliminatedG18$M2003, 0.25)
Q3M2003 <- quantile(eliminatedG18$M2003, 0.75)
IQRM2003<-IQR(eliminatedG18$M2003)


Q1M2004 <- quantile(eliminatedG18$M2004, 0.25)
Q3M2004 <- quantile(eliminatedG18$M2004, 0.75)
IQRM2004<-IQR(eliminatedG18$M2004)


Q1M2005 <- quantile(eliminatedG18$M2005, 0.25)
Q3M2005 <- quantile(eliminatedG18$M2005, 0.75)
IQRM2005<-IQR(eliminatedG18$M2005)


Q1M2006 <- quantile(eliminatedG18$M2006, 0.25)
Q3M2006 <- quantile(eliminatedG18$M2006, 0.75)
IQRM2006<-IQR(eliminatedG18$M2006)


Q1M2007 <- quantile(eliminatedG18$M2007, 0.25)
Q3M2007 <- quantile(eliminatedG18$M2007, 0.75)
IQRM2007<-IQR(eliminatedG18$M2007)


Q1M2008 <- quantile(eliminatedG18$M2008, 0.25)
Q3M2008 <- quantile(eliminatedG18$M2008, 0.75)
IQRM2008<-IQR(eliminatedG18$M2008)


Q1M2009 <- quantile(eliminatedG18$M2009, 0.25)
Q3M2009 <- quantile(eliminatedG18$M2009, 0.75)
IQRM2009<-IQR(eliminatedG18$M2009)


Q1M2010 <- quantile(eliminatedG18$M2010, 0.25)
Q3M2010 <- quantile(eliminatedG18$M2010, 0.75)
IQRM2010<-IQR(eliminatedG18$M2010)


Q1M2011 <- quantile(eliminatedG18$M2011, 0.25)
Q3M2011 <- quantile(eliminatedG18$M2011, 0.75)
IQRM2011<-IQR(eliminatedG18$M2011)


Q1M2012 <- quantile(eliminatedG18$M2012, 0.25)
Q3M2012 <- quantile(eliminatedG18$M2012, 0.75)
IQRM2012<-IQR(eliminatedG18$M2012)

Q1M2013 <- quantile(eliminatedG18$M2013, 0.25)
Q3M2013 <- quantile(eliminatedG18$M2013, 0.75)
IQRM2013<-IQR(eliminatedG18$M2013)

Q1M2014 <- quantile(eliminatedG18$M2014, 0.25)
Q3M2014 <- quantile(eliminatedG18$M2014, 0.75)
IQRM2014<-IQR(eliminatedG18$M2014)

Q1M2015 <- quantile(eliminatedG18$M2015, 0.25)
Q3M2015 <- quantile(eliminatedG18$M2015, 0.75)
IQRM2015<-IQR(eliminatedG18$M2015)

Q1M2016 <- quantile(eliminatedG18$M2016, 0.25)
Q3M2016 <- quantile(eliminatedG18$M2016, 0.75)
IQRM2016<-IQR(eliminatedG18$M2016)

Q1M2017 <- quantile(eliminatedG18$M2017, 0.25)
Q3M2017 <- quantile(eliminatedG18$M2017, 0.75)
IQRM2017<-IQR(eliminatedG18$M2017)

eliminatedM1 <- subset(eliminatedG18, eliminatedG18$M2000 > (Q1M2000 - 1.5*IQRM2000)
                       & eliminatedG18$M2000 < (Q3M2000+ 1.5*IQRM2000))
eliminatedM2 <- subset(eliminatedM1, eliminatedM1$M2001 > (Q1M2001 - 1.5*IQRM2001)
                       & eliminatedM1$M2001 < (Q3M2001+ 1.5*IQRM2001))

eliminatedM3 <- subset(eliminatedM2, eliminatedM2$M2002 > (Q1M2002 - 1.5*IQRM2002)
                       & eliminatedM2$M2002 < (Q3M2002+ 1.5*IQRM2002))

eliminatedM4 <- subset(eliminatedM3, eliminatedM3$M2003 > (Q1M2003 - 1.5*IQRM2003)
                       & eliminatedM3$M2003 < (Q3M2003+ 1.5*IQRM2003))
eliminatedM5 <- subset(eliminatedM4, eliminatedM4$M2004 > (Q1M2004 - 1.5*IQRM2004)
                       & eliminatedM4$M2004 < (Q3M2004+ 1.5*IQRM2004))
eliminatedM6 <- subset(eliminatedM5, eliminatedM5$M2005 > (Q1M2005 - 1.5*IQRM2005)
                       & eliminatedM5$M2005 < (Q3M2005+ 1.5*IQRM2005))
eliminatedM7 <- subset(eliminatedM6, eliminatedM6$M2006 > (Q1M2006 - 1.5*IQRM2006)
                       & eliminatedM6$M2006 < (Q3M2006+ 1.5*IQRM2006))
eliminatedM8 <- subset(eliminatedM7, eliminatedM7$M2007 > (Q1M2007 - 1.5*IQRM2007)
                       & eliminatedM7$M2007 < (Q3M2007+ 1.5*IQRM2007))
eliminatedM9 <- subset(eliminatedM8, eliminatedM8$M2008 > (Q1M2008 - 1.5*IQRM2008)
                       & eliminatedM8$M2008 < (Q3M2008+ 1.5*IQRM2008))
eliminatedM10 <- subset(eliminatedM9, eliminatedM9$M2009 > (Q1M2009 - 1.5*IQRM2009)
                       & eliminatedM9$M2009 < (Q3M2009+ 1.5*IQRM2009))
eliminatedM11 <- subset(eliminatedM10, eliminatedM10$M2010 > (Q1M2010 - 1.5*IQRM2010)
                        & eliminatedM10$M2010 < (Q3M2010+ 1.5*IQRM2010))
eliminatedM12<- subset(eliminatedM11, eliminatedM11$M2011 > (Q1M2011 - 1.5*IQRM2011)
                       & eliminatedM11$M2011 < (Q3M2011+ 1.5*IQRM2011))
eliminatedM13 <- subset(eliminatedM12, eliminatedM12$M2012 > (Q1M2012 - 1.5*IQRM2012)
                       & eliminatedM12$M2012 < (Q3M2012+ 1.5*IQRM2012))
eliminatedM14 <- subset(eliminatedM13, eliminatedM13$M2013 > (Q1M2013 - 1.5*IQRM2013)
                       & eliminatedM13$M2013 < (Q3M2013+ 1.5*IQRM2013))
eliminatedM15 <- subset(eliminatedM14, eliminatedM14$M2014 > (Q1M2014 - 1.5*IQRM2014)
                       & eliminatedM14$M2014 < (Q3M2014+ 1.5*IQRM2014))
eliminatedM16<- subset(eliminatedM15, eliminatedM15$M2015 > (Q1M2015 - 1.5*IQRM2015)
                       & eliminatedM15$M2015 < (Q3M2015+ 1.5*IQRM2015))

eliminatedM17<- subset(eliminatedM16, eliminatedM16$M2016 > (Q1M2016 - 1.5*IQRM2016)
                       & eliminatedM16$M2016 < (Q3M2016+ 1.5*IQRM2016))

data2<- subset(eliminatedM17, eliminatedM17$M2017 > (Q1M2017 - 1.5*IQRM2017)
                       & eliminatedM17$M2017 < (Q3M2017+ 1.5*IQRM2017))
dim(data2)
boxplot(data2$M2000,data2$M2001,data2$M2002,data2$M2003,data2$M2004,data2$M2005,data2$M2006,data2$M2007,data2$M2008,
        data2$M2009,data2$M2010,data2$M2011,data2$M2012,data2$M2013,data2$M2014,data2$M2015,data2$M2016,data2$M2017)
#checking value by using histogram 
attach(mtcars) 
par(mfrow=c(3,2)) 

hist(data2$value_2000)
hist(data2$value_2001)
hist(data2$value_2002)
hist(data2$value_2003)
hist(data2$value_2004)
hist(data2$value_2005)
hist(data2$value_2006)
hist(data2$value_2007)
hist(data2$value_2008)
hist(data2$value_2009)
hist(data2$value_2010)
hist(data2$value_2011)
hist(data2$value_2012)
hist(data2$value_2013)
hist(data2$value_2014)
hist(data2$value_2015)
hist(data2$value_2016)
hist(data2$value_2017)


#checking value by using histogram 
attach(mtcars) 
par(mfrow=c(3,2)) 

hist(data2$M2000)
hist(data2$M2001)
hist(data2$M2002)
hist(data2$M2003)
hist(data2$M2004)
hist(data2$M2005)
hist(data2$M2006)
hist(data2$M2007)
hist(data2$M2008)
hist(data2$M2009)
hist(data2$M2010)
hist(data2$M2011)
hist(data2$M2012)
hist(data2$M2013)
hist(data2$M2014)
hist(data2$M2015)
hist(data2$M2016)
hist(data2$M2017)
#checking value by using histogram 
hist(data2$G2000)
hist(data2$G2001)
hist(data2$G2002)
hist(data2$G2003)
hist(data2$G2004)
hist(data2$G2005)
hist(data2$G2006)
hist(data2$G2007)
hist(data2$G2008)
hist(data2$G2009)
hist(data2$G2010)
hist(data2$G2011)
hist(data2$G2012)
hist(data2$G2013)
hist(data2$G2014)
hist(data2$G2015)
hist(data2$G2016)
hist(data2$G2017)

#checking value by using ecdf
plot(ecdf(data2$value_2000), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2001), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2002), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2003), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2004), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2005), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2006), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2007), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2008), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2009), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2010), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2011), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2012), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2013), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2014), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2015), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2016), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$value_2017), do.points=FALSE, verticals=TRUE)

#checking value by using ecdf
plot(ecdf(data2$M2000), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2001), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2002), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2003), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2004), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2005), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2006), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2007), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2008), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2009), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2010), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2011), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2012), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2013), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2014), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2015), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2016), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$M2017), do.points=FALSE, verticals=TRUE)

#checking value by using ecdf
par(mfrow=c(1,1))
plot(ecdf(data2$G2000), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2001), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2002), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2003), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2004), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2005), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2006), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2007), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2008), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2009), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2010), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2012), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2013), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2014), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2015), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2016), do.points=FALSE, verticals=TRUE)
plot(ecdf(data2$G2017), do.points=FALSE, verticals=TRUE)

#checking value by using qqplot
par(pty="s") 
qqnorm(data2$value_2000); qqline(data2$value_2000)
qqnorm(data2$value_2001); qqline(data2$value_2001)
qqnorm(data2$value_2002); qqline(data2$value_2002)
qqnorm(data2$value_2003); qqline(data2$value_2003)
qqnorm(data2$value_2004); qqline(data2$value_2004)
qqnorm(data2$value_2005); qqline(data2$value_2005)
qqnorm(data2$value_2006); qqline(data2$value_2006)
qqnorm(data2$value_2007); qqline(data2$value_2007)
qqnorm(data2$value_2008); qqline(data2$value_2008)
qqnorm(data2$value_2009); qqline(data2$value_2009)
qqnorm(data2$value_2010); qqline(data2$value_2010)
qqnorm(data2$value_2011); qqline(data2$value_2011)
qqnorm(data2$value_2012); qqline(data2$value_2012)
qqnorm(data2$value_2013); qqline(data2$value_2013)
qqnorm(data2$value_2014); qqline(data2$value_2014)
qqnorm(data2$value_2015); qqline(data2$value_2015)
qqnorm(data2$value_2016); qqline(data2$value_2016)
qqnorm(data2$value_2017); qqline(data2$value_2017)


#checking value by using qqplot
qqnorm(data2$M2000); qqline(data2$M2000)
qqnorm(data2$M2001); qqline(data2$M2001)
qqnorm(data2$M2002); qqline(data2$M2002)
qqnorm(data2$M2003); qqline(data2$M2003)
qqnorm(data2$M2004); qqline(data2$M2004)
qqnorm(data2$M2005); qqline(data2$M2005)
qqnorm(data2$M2006); qqline(data2$M2006)
qqnorm(data2$M2007); qqline(data2$M2007)
qqnorm(data2$M2008); qqline(data2$M2008)
qqnorm(data2$M2009); qqline(data2$M2009)
qqnorm(data2$M2010); qqline(data2$M2010)
qqnorm(data2$M2011); qqline(data2$M2011)
qqnorm(data2$M2012); qqline(data2$M2012)
qqnorm(data2$M2013); qqline(data2$M2013)
qqnorm(data2$M2014); qqline(data2$M2014)
qqnorm(data2$M2015); qqline(data2$M2015)
qqnorm(data2$M2016); qqline(data2$M2016)
qqnorm(data2$M2017); qqline(data2$M2017)

#checking value by using qqplot
par(pty="s") 
qqnorm(data2$G2000); qqline(data2$G2000)
qqnorm(data2$G2001); qqline(data2$G2001)
qqnorm(data2$G2002); qqline(data2$G2002)
qqnorm(data2$G2003); qqline(data2$G2003)
qqnorm(data2$G2004); qqline(data2$G2004)
qqnorm(data2$G2005); qqline(data2$G2005)
qqnorm(data2$G2006); qqline(data2$G2006)
qqnorm(data2$G2007); qqline(data2$G2007)
qqnorm(data2$G2008); qqline(data2$G2008)
qqnorm(data2$G2009); qqline(data2$G2009)
qqnorm(data2$G2010); qqline(data2$G2010)
qqnorm(data2$G2011); qqline(data2$G2011)
qqnorm(data2$G2012); qqline(data2$G2012)
qqnorm(data2$G2013); qqline(data2$G2013)
qqnorm(data2$G2014); qqline(data2$G2014)
qqnorm(data2$G2015); qqline(data2$G2015)
qqnorm(data2$G2016); qqline(data2$G2016)
qqnorm(data2$G2017); qqline(data2$G2017)



#ggplot
library(ggplot2)
ggplot(data = data2) + geom_bar(mapping = aes(x = data2$value_2000))


#PREDICT 2018
cor.test(data2[,'G2017'],data2[,'M2017'],method = "pearson")
cor.test(data2[,'G2017'],data2[,'value_2017'],method = "pearson")
cor.test(data2[,'M2017'],data2[,'value_2017'],method = "pearson")



install.packages("corrplot")
library(corrplot)
data_value=subset(data2,select=c(value_2008,value_2009,value_2010,value_2011,value_2012,value_2013,value_2014,value_2015,value_2016,value_2017,
                                 M2008,M2009,M2010,M2011,M2012,M2013,M2014,M2015,M2016,M2017,
                                 G2008,G2009,G2010,G2011,G2012,G2013,G2014,G2015,G2016,G2017))
cor_value <- cor(data_value)
corrplot(cor_value,method = "shade",shade.col = NA,t1.col= "black",t1.srt=45, order = "AOE")


#model
install.packages("caret")
library(caret);library(rpart);library(dplyr);library(randomForest)
set.seed(617)
split=sample(x=1:nrow(data2),size=0.8*nrow(data2));
data2_train <-data2[split,]
data2_test <-data2[-split,]
dim(data2_train )
dim(data2_test)
set.seed(617)

lmvalue<-lm(data2$G2017~ data2$value_2017+data2$value_2016+data2$value_2015+data2$value_2014+data2$value_2013
            +data2$value_2012+data2$value_2011+data2$value_2010+data2$M2010+data2$M2011+data2$M2012+data2$M2013+data2$M2014+data2$M2015+data2$M2016+data2$M2017
             )
lmvalue
summary(lmvalue)
coevalue<-coef(lmEvalue)
pred = predict(lmvalue,data=data2_test)
pred
rmse_ln = sqrt(mean((pred-data2_test$value_2017)^2)); rmse_ln
plot(lmEvalue)


lmEvalue<-lm(data2$G2017~ data2$value_2017+data2$value_2016+data2$value_2015+data2$value_2014+data2$value_2013
             +data2$value_2012+data2$value_2011+data2$value_2010+data2$value_2009+data2$value_2008
             +data2$value_2007+data2$value_2006+data2$value_2005+data2$value_2004+data2$value_2003+
               data2$value_2002+data2$value_2001+data2$value_2000+data2$M2000+data2$M2001++data2$M2002
             +data2$M2003+data2$M2004+data2$M2005+data2$M2006+data2$M2007+data2$M2008+data2$M2009
             +data2$M2010+data2$M2011+data2$M2012+data2$M2013+data2$M2014++data2$M2015+data2$M2016+data2$M2017
)
summary(lmEvalue)
coevalue<-coef(lmEvalue)
pred = predict(lmEvalue,data=data2_test)
pred
rmse_ln = sqrt(mean((pred-data2_test$value_2017)^2)); rmse_ln
plot(lmEvalue)
forest <- randomForest(data2$G2017~ data2$value_2016+data2$value_2015+data2$value_2014+data2$value_2013
                       +data2$value_2012+data2$value_2011+data2$value_2010+data2$value_2009+data2$value_2008
                       +data2$value_2007+data2$value_2006+data2$value_2005+data2$value_2004+data2$value_2003+
                         data2$value_2002+data2$value_2001+data2$value_2000+data2$M2000+data2$M2001++data2$M2002
                       +data2$M2003+data2$M2004+data2$M2005+data2$M2006+data2$M2007+data2$M2008+data2$M2009
                       +data2$M2010+data2$M2011+data2$M2012+data2$M2013+data2$M2014+data2$M2015+data2$M2016
                       ,data=data2_train,ntree = 1000,na.action = na.omit)
predforest = predict(forest,newdata1=data2_test)
summary(forest)
forest
rmse_forest = sqrt(mean((predforest-data2_test$value_2017)^2)); rmse_forest
plot(forest)

forest1 <- randomForest(data2$G2017~ data2$value_2016+data2$value_2015+data2$value_2014+data2$value_2013
                       +data2$value_2012+data2$value_2011+data2$value_2010+data2$value_2009+data2$value_2008
                       +data2$value_2007+data2$value_2006+data2$value_2005+data2$value_2004+data2$value_2003+
                         data2$value_2002+data2$value_2001+data2$value_2000+data2$M2000+data2$M2001++data2$M2002
                       +data2$M2003+data2$M2004+data2$M2005+data2$M2006+data2$M2007+data2$M2008+data2$M2009
                       +data2$M2010+data2$M2011+data2$M2012+data2$M2013+data2$M2014+data2$M2015+data2$M2016
                       ,data=data2_train,ntree =100,na.action = na.omit)

predforest1 = predict(forest1,newdata1=data2_test)
rmse_forest1 = sqrt(mean((predforest1-data2_test$value_2017)^2)); rmse_forest1
plot(forest1)
forest1

install.packages("xgboost")

#############################################################################################
head(data2)

##############33
install.packages("vtreat")
library(vtreat)
library(xgboost); library(caret)
set.seed(617)
trt = designTreatmentsZ(dframe = data2_train,
                        varlist = names(data2_train)[4:ncol(data2)-1])

newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']
train_input = prepare(treatmentplan = trt,
                      dframe =  data2_train,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt,
                     dframe = data2_test,
                     varRestriction = newvars)
data_input=prepare(treatmentplan = trt,
                   dframe = data2,
                   varRestriction = newvars)
head(train_input)

##################cross validatin to choose tuned parameter
set.seed(617)
tune_nrounds = xgb.cv(data=as.matrix(train_input), 
                      label = data2_train$G2017,
                      nrounds=250,
                      nfold = 5,
                      verbose = 0)

ggplot(data=tune_nrounds$evaluation_log, aes(x=iter, y=test_rmse_mean))+
  geom_point(size=0.4, color='sienna')+
  geom_line(size=0.1, alpha=0.1)+
  theme_bw()

which.min(tune_nrounds$evaluation_log$test_rmse_mean)

#use tuned parameter to implement xgboost

xgboost2= xgboost(data=as.matrix(train_input), 
                  label = data2_train$G2017,
                  nrounds=250,
                  verbose = 0)
pred = predict(xgboost2, 
               newdata=as.matrix(test_input))
rmse_xgboost = sqrt(mean((pred - data2_test$G2017)^2)); rmse_xgboost
xgboost2
#   xgboost rmse   0.0001322443
################################################################################

###############################################################################
dectionTreeModel<- rpart(data2$G2017~ data2$value_2017+data2$value_2016+data2$value_2015+data2$value_2014+data2$value_2013
                         +data2$value_2012+data2$value_2011+data2$value_2010+data2$value_2009+data2$value_2008
                         +data2$value_2007+data2$value_2006+data2$value_2005+data2$value_2004+data2$value_2003+
                           data2$value_2002+data2$value_2001+data2$value_2000+data2$M2000+data2$M2001++data2$M2002
                         +data2$M2003+data2$M2004+data2$M2005+data2$M2006+data2$M2007+data2$M2008+data2$M2009
                         +data2$M2010+data2$M2011+data2$M2012+data2$M2013+data2$M2014+data2$M2015+data2$M2016+data2$M2017
                         ,data2_train,method = "class")
pred = predict(dectionTreeModel,data=data2_test)
rmse_tree = sqrt(mean((pred-data2_test$value_2017)^2)); rmse_tree
summary(dectionTreeModel)
plot(dectionTreeModel)



