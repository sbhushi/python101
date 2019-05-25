######################################################################
#Setting up the directory and loading the required packages
getwd()
######################################################################
#Change the working directory to the path where file is present
# setwd("chalmers/users/bhushi")

library("stringr")
library("stringi")
library("plyr")
library("reshape2")
library("rgl")
library("ggplot2")
library("ggcorrplot")

######################################################################
#Reading the dataset
hiab_data_1=read.table(file = "H166_2017-07-06.csv",sep = ",",header = TRUE,stringsAsFactors = FALSE)

# hiab_data_2=read.table(file = "H166_2017-07-04.csv",sep = ",",fill = TRUE, header=TRUE, 
#                        stringsAsFactors = FALSE)

# hiab_data_3=read.table(file = "H166_LC_2017-07-07(mod).csv",sep = ",",fill=TRUE,header = TRUE, 
#                        stringsAsFactors = FALSE)

#hiab_data_1=rbind.fill(hiab_data_1,hiab_data_2)

#hiab_data_3=hiab_data_3[,c(1:10)]
######################################################################
#To make sNom uniform and remove outliers from the dataset-applied on lift counter
hiab_data_1$sNom=0.32
hiab_data_1=subset(hiab_data_1,hiab_data_1$lift_ctr<500000)


######################################################################
#Reamoving the factory data
hiab_data_1=subset(hiab_data_1,
                   !(grepl(pattern = "factory",x = hiab_data_1$sac_file,ignore.case = TRUE)))


######################################################################
#to get the data type of all the variables and their summary (OPTIONAL!!)
View(as.data.frame(sapply(X = hiab_data_1,FUN = typeof)))
summary(hiab_data_1)

######################################################################
#Operation to be performed on numeric only dataset REASON: since the three variables
#considered below were not of numeric type
hiab_data_1$calender_days=as.numeric(hiab_data_1$calender_days)
hiab_data_1$lift_ctr=as.numeric(hiab_data_1$lift_ctr)
######################################################################
#consider days where the field usage is >1 day
hiab_data_1=subset(hiab_data_1,hiab_data_1$calender_days>=0)
######################################################################
#Data cleaning and applying limit to the variables considered

hiab_data_1=subset(hiab_data_1,hiab_data_1$IBLCf..s_current<1)
dummy_data=subset(hiab_data_1,!(hiab_data_1$lift_ctr==0))
dummy_data=subset(dummy_data,dummy_data$IBLCp..s_current<1)
dummy_data=subset(dummy_data,!(dummy_data$IBLCf..s_current==0))
dummy_data_corrIBLCFs=dummy_data

# dummy_data=subset(dummy_data,!(dummy_data$IBLCp..s_current==0))
# dummy_data_corrCP.values=dummy_data

######################################################################
#plotting the required graphs (TRIAL)


plot(dummy_data$calender_days,dummy_data$IBLCf..s_current)
######################################################################
#Figure 2
ggplot(data = dummy_data,mapping = aes(x =lift_days ,y = IBLCf..s_current)
       ,color="steelblue")+ geom_point() + geom_smooth()

#figure 3
plot3d(x = dummy_data$calender_days, y = dummy_data$lift_ctr, z = dummy_data$IBLCf..s_current
       ,xlab = "CALENDAR DAYS",ylab = "lift counter", zlab = "CF current")


dummy_data=subset(x = dummy_data,dummy_data$IBLCp..s_current<1)
plot3d(x = dummy_data$lift_days ,y = dummy_data$lift_ctr, z = dummy_data$IBLCp..s_current
       ,xlab = "LIFT DAYS",ylab = "lift counter", zlab = "CP current")

dev.off()
ggplot(data = dummy_data,mapping = aes(x =lift_days  ,y = IBLCp..s_current)
       ,color="steelblue")+ geom_point() + geom_smooth()

######################################################################
#To get the same result (in case of random number generation, the number generated must be close enough)
set.seed(100)

######################################################################

#WARNING/OBS!: THE BELOW COMMAND TAKES A CONSIDERABLE AMOUNT OF TIME!!!
#Figure 1
pairs(dummy_data[20:40])

######################################################################
#Another couple of plots

plot3d(x = dummy_data$calender_days, y = dummy_data$lift_ctr, z = dummy_data$IBLCf..s_current
       ,xlab = "CALENDAR DAYS",ylab = "lift counter", zlab = "CF current")
plot3d(x = dummy_data$lift_ctr, y = dummy_data$IBLCf..s_current, z = dummy_data$IBLCp..s_current
       ,xlab = "Lift counter",ylab = "IBLCF S_current", zlab = "IBLCP S_current")
plot3d(x = dummy_data$lift_ctr, y = dummy_data$IBLCf..s_current, z = dummy_data$IBLCp..s_current
       ,xlab = "Lift counter",ylab = "IBLCF S_current", zlab = "IBLCP S_current")
######################################################################
#Extracting a correlation plot in a csv file

View(as.data.frame(cor(dummy_data[6:47])))
all_corr=as.data.frame(cor(dummy_data[6:47]))
write.csv(x = all_corr,file = "correlation file")
######################################################################
#Working on 2016 and 2017 file (UNNECESSARY BUT MAY BE USEFUL IN FUTURE!
#CAN BE USED FOR PREDICTIVE MODELLING AS WELL!!)

data16=subset(dummy_data, 
              (grepl(pattern = "2016", x =dummy_data$current_date,ignore.case = TRUE)))
data17=subset(dummy_data, 
              (grepl(pattern = "2017", x =dummy_data$current_date,ignore.case = TRUE)))

######################################################################
#sorting dataset as per serial number and lift counter AND CONVERTING IT INTO A DATE TYPE FORMAT 
#can be forgone 

or_data = dummy_data[order(dummy_data$crane_ser_no,- dummy_data$lift_ctr),]
or_data$current_date=as.Date(x = or_data$current_date, format="%Y-%m-%d")

######################################################################
#Company steps analysis (considering only the lastest values)
#Considering only the unique values (of crane serial)
or_data_new=or_data[!duplicated(x = or_data[,'crane_ser_no']),]
pairs(or_data_new[20:40])
View(as.data.frame(cor(or_data_new[6:47])))

######################################################################
#to get the data type of all the variables and their summary
View(as.data.frame(sapply(X = hiab_data_1,FUN = typeof)))
summary(hiab_data_1)

######################################################################
#try by creating training and test data set
set.seed(100)

sample_DAT = sample(n = nrow(hiab_data_1), size = floor(.75*nrow(hiab_data_1)), replace = FALSE)

train = hiab_data_1[sample_DAT, ]
test  = hiab_data_1[-sample_DAT, ]

needed_slewtime=subset(hiab_data_1,hiab_data_1$Time.for.Jib....<100)
needed_slewtime=subset(hiab_data_1,!(hiab_data_1$Time.for.Jib....==0))

######################################################################
#glm model on s current and the total time 
#not needed as the dataset eill only have 335 readings

model1=lm(needed_slewtime$IBLCf..s_current~needed_slewtime$IBLCp..D_tot_time,data = dummy_data)
summary(model1)
abline(model1)
rm(needed_slewtime)

# check=predict(model1,newdata = test, type = "response")
# summary(check)
# auc(test$IBLCf..s_current,check)

######################################################################
#crane force aggregation dataset and arranging it in a decreasing order
crane_force=aggregate(IBLCf..s_current~crane_type,data = hiab_data_1,FUN = mean)
crane_force=crane_force[order(crane_force$IBLCf..s_current),]
######################################################################
#checking for correlation between the time for ## variables (AGAIN)
View(as.data.frame(cor(dummy_data[6:47])))


######################################################################
#Desired plots and finding the correlation
plot(hiab_data_1$lift_ctr,hiab_data_1$tot_time..hour.)
cor(hiab_data_1$lift_ctr,hiab_data_1$tot_time..hour.)

######################################################################
#cor(hiab_data_1$calender_days,hiab_data_1$lift_days)
cor(hiab_data_1[,unlist(lapply(hiab_data_1, is.double))])
dummy_data_corrCP.values=subset(dummy_data_corrCP.values,dummy_data_corrCP.values$lift_days!=0)                
plot(dummy_data_corrCP.values$calender_days,dummy_data_corrCP.values$lift_days)
ggplot(data = hiab_data_1,mapping = aes(x = calender_days,y = lift_days),color="steelblue")+ geom_point() + geom_smooth()
ggplot(data = dummy_data,mapping = aes(x =calender_days ,y = IBLCf..s_current)
       ,color="steelblue")+ geom_point() + geom_smooth()

######################################################################
#Selecting the required amount of data
cranemore200d=subset(or_data,or_data$calender_days>2000)
cranemore200d=cranemore200d[,c("calender_days","IBLCf..s_current")]

######################################################################
#Classification of number of calendar days into different years
cranemore200d$group=ifelse(cranemore200d$calender_days<2500,"Group 1",
                           ifelse(cranemore200d$calender_days<3000,"Group 2",
                                  ifelse(cranemore200d$calender_days<3500,"Group 3",
                                         ifelse(cranemore200d$calender_days<4000,"Group 4","Group 5"))))


######################################################################
#Aggregating the mean of damage and comparing it wiht the yearly basis and plotting it
agg_DB=aggregate(formula=IBLCf..s_current~group,data=cranemore200d,FUN ="mean" )

boxplot(IBLCf..s_current~group,data=cranemore200d, main="Crane Data", 
        xlab="Calendar days ", ylab="IBCLf S_current")
plot(cranemore200d$IBLCf..s_current~cranemore200d$calender_days)
lines(lowess(cranemore200d$IBLCf..s_current~cranemore200d$calender_days))

#box plots of 3 GROUPS. line cannot be plotted with single obs!?!? IMPPPPPPPPPPPPP
ggplot(cranemore200d, aes(x=group, y=IBLCf..s_current)) +  
  #geom_line(data = cranemore200d,aes(x=group, y=mean(IBLCf..s_current)))+geom_boxplot()
  geom_boxplot((aes(fill = factor(group)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))



# lo = loess(formula =IBLCf..s_current~group ,data =agg_DB ,na.action = TRUE,family = "gaussian")
# plot(cranemore200d$IBLCf..s_current,cranemore200d$group)
# lines(predict(lo), col='red', lwd=2)
# geom_line(data=cranemore200d, aes(x=group, y=log10(IBLCf..s_current)))
#cranemore200d$group=ifelse(test = cranemore200d$calender_days<3000,"Group 1","unknown")

######################################################################
#multiple plots

plot(hiab_data_1$calender_days,hiab_data_1$lift_days)
colnames(hiab_data)
View(hiab_data)
as.matrix(cor(hiab_data_3[9:10]))
plot(hiab_data_1$Pref,hiab_data_1$lift_days)
plot(hiab_data_1$Time.for.Slew....,hiab_data_1$Time.for.Jib....)
plot(hiab_data_1$sNom,hiab_data_1$IBLCf..s_current)
# barplot(length(hiab_data_1$crane_type))
ggplot(data=hiab_data_1, aes(x=calender_days, y=IBLCf..s_current, group=crane_type, colour=crane_type)) +
  geom_line() +
  geom_point()


#Plotting multiple graphs
ggplot(data=hiab_data_1, aes(x=lift_ctr, y=IBLCf..s_current, colour="steelblue")) +
  geom_line() +
  geom_point()
ggplot(data = hiab_data_1,mapping = aes(x =lift_ctr ,y = IBLCf..s_current)
       ,color="steelblue")+ geom_point() + geom_smooth()

ggplot(data=dummy_data, aes(x=lift_ctr, y=IBLCf..s_current)) +
  geom_point() +
  geom_smooth()
ggplot(data=data16, aes(x=lift_ctr, y=IBLCf..s_current, legend="Datapoints")) +
  geom_point() +
  geom_smooth()
ggplot(data=data17, aes(x=lift_ctr, y=IBLCf..s_current)) +
  geom_point() +
  geom_smooth()

ggplot(data=or_data_new, aes(x=lift_ctr, y=IBLCf..s_current)) +
  geom_point() +
  geom_smooth()

#IBLCfs crrent v/s calendar days
ggplot(data=dummy_data, aes(x=calender_days, y=IBLCf..s_current)) +
  geom_point() +
  geom_smooth()
ggplot(data=dummy_data, aes(x=lift_ctr, y=IBLCp..s_current)) +
  geom_point() +
  geom_smooth()

ggplot(data=dummy_data, aes(x=Time.for.Slew...., y=Time.for.Ext....)) +
  geom_point() +
  geom_smooth()
ggplot(data=dummy_data, aes(x=Time.Brick.mode...., y=Time.Digg.mode....)) +
  geom_point() +
  geom_smooth()
#ones with negativ correlation (-0.2)
ggplot(data=dummy_data, aes(x=Time.Tool.mode...., y=Time.Brick.mode....)) +
  geom_point() +
  geom_smooth()
ggplot(data=dummy_data, aes(x=Time.LightT.mode...., y=Time.Digg.mode....)) +
  geom_point() +
  geom_smooth()


######################################################################
#Boxplot as svante demanded
boxplot(IBLCf..s_current~calender_days,data=dummy_data, main="Crane Data", 
        xlab="Calendar days ", ylab="IBCLf S_current")
View(as.data.frame(aggregate(formula=IBLCf..s_current~calender_days,data=dummy_data,FUN ="mean" )))

######################################################################
#building polynomial model
plot(x = cranemore200d$calender_days,y = cranemore200d$IBLCf..s_current,
     main = "polynomial regression for above 2000 days",las=1)

######################################################################
#simple linear regression model, summary and checking the fit
model1= lm(IBLCf..s_current~ calender_days, data=cranemore200d)
summary(model1)
abline(model1,lwd=3,col="red")

coef(fit)
paste('y =', coef(fit)[[2]], '* x', '+', coef(fit)[[1]])

#Considering the higher degree of variable (2 in this case)
model2=lm(IBLCf..s_current ~ calender_days+ I(calender_days^2), data=cranemore200d)
summary(model2)
lines(smooth.spline(cranemore200d$calender_days,predict(model2)),col="green",lwd=10)


#compare model using F-test. ANOVCA TEST FOR COMPARISON
anova(model1,model2)

######################################################################
#for year basis devisions. done on a different dataset

yearlydata=dummy_data[,c("calender_days","IBLCf..s_current","IBLCp..s_current")]
yearlydata$group=     ifelse(yearlydata$calender_days<365,"Year 1",
                             ifelse(yearlydata$calender_days<(365*2),"Year 2",
                                    ifelse(yearlydata$calender_days<(365*3),"Year 3",
                                           ifelse(yearlydata$calender_days<(365*4),"Year 4",
                                                  ifelse(yearlydata$calender_days<(365*5),"Year 5",
                                                         ifelse(yearlydata$calender_days<(365*6),"Year 6",
                                                                ifelse(yearlydata$calender_days<(365*7),"Year 7",
                                                                       ifelse(yearlydata$calender_days<(365*8),"Year 8",
                                                                              ifelse(yearlydata$calender_days<(365*9),"Year 9",
                                                                                     ifelse(yearlydata$calender_days<(365*10),"Year 10",
                                                                                            ifelse(yearlydata$calender_days<(365*11),"Year 11",
                                                                                                   ifelse(yearlydata$calender_days<(365*12),"Year 12","Year 13"))))))))))))
#Arranging in descending order
yearlydata = yearlydata[order(yearlydata$calender_days),]

yearlydata$group =factor(yearlydata$group, as.character(yearlydata$group))
#box plots of 3 GROUPS. line cannot be plotted with single obs!?!? IMPPPPPPPPPPPPP
ggplot(yearlydata, aes(x=group, y=IBLCf..s_current)) +  
  #geom_line(data = cranemore200d,aes(x=group, y=mean(IBLCf..s_current)))+geom_boxplot()
  geom_boxplot((aes(fill = factor(group)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))


#finding out the summary of each group
# i=3
# summary(subset(yearlydata,yearlydata$group==as.character(cat(paste("Year",i)))))
#NOTE: THE BELOW CODE CAN BE PERFORMED IN A LOOP. WILL FIGURE IT OUT LATER.
summary(subset(yearlydata,yearlydata$group=='Year 1'))
summary(subset(yearlydata,yearlydata$group=='Year 2'))
summary(subset(yearlydata,yearlydata$group=='Year 3'))
summary(subset(yearlydata,yearlydata$group=='Year 4'))
summary(subset(yearlydata,yearlydata$group=='Year 5'))
summary(subset(yearlydata,yearlydata$group=='Year 6'))
summary(subset(yearlydata,yearlydata$group=='Year 7'))
summary(subset(yearlydata,yearlydata$group=='Year 8'))
summary(subset(yearlydata,yearlydata$group=='Year 9'))
summary(subset(yearlydata,yearlydata$group=='Year 10'))
summary(subset(yearlydata,yearlydata$group=='Year 11'))

#yearlydata[,list(summary),by=yearlydata$group]
# for (i in 10)
# {
#   summary(subset(yearlydata,yearlydata$group==cat(paste(i, "Year "))))
# }

# aaa=by(yearlydata[, 1:3], yearlydata$group, summary)
# rm(aaa)


yearlydata[,list(mean=mean(yearlydata$IBLCf..s_current),sd=sd(yearlydata$IBLCf..s_current)),by=yearlydata$group]
View(as.data.frame(sapply(X = hiab_data_1,FUN = summary)))


######################################################################
#checking for IBCLP values. and PLOTTING OTHER IMPORTANT GRAPHS
yearlydata=subset(yearlydata,yearlydata$IBLCp..s_current>0)
ggplot(yearlydata, aes(x=group, y=IBLCp..s_current)) +  
  #geom_line(data = cranemore200d,aes(x=group, y=mean(IBLCf..s_current)))+geom_boxplot()
  geom_boxplot((aes(fill = factor(group)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))
View(aggregate(x = yearlydata, by = list(yearlydata$group), FUN = "summary"))


#checking for regression model in yearlydata
#building polynomial model
plot(x = yearlydata$calender_days,y = yearlydata$IBLCp..s_current,
     main = "polynomial regression IBCLp...s_current",las=1)

######################################################################
#simple linear regression model
model1_cp= lm(IBLCp..s_current~ ., data=yearlydata)
summary(model1_cp)
abline(model1_cp,lwd=3,col="red")
coef(model1_cp)
paste('y =', coef(model1_cp)[[2]], '* x', '+', coef(model1_cp)[[1]])

#now for degree 2
model2_cp=lm(IBLCp..s_current ~ calender_days+ I(calender_days^2), data=yearlydata)
summary(model2_cp)
lines(smooth.spline(yearlydata$calender_days,predict(model2_cp)),col="green",lwd=10)

coef(model2_cp)
#compare model using F-test
anova(model1,model2)

#now for degree 3
model3_cp=lm(IBLCp..s_current ~ calender_days+ I(calender_days^3), data=yearlydata)
summary(model3_cp)
lines(smooth.spline(yearlydata$calender_days,predict(model3_cp)),col="yellow",lwd=10)
######################################################################
#Model comparison

anova(model1_cp,model2_cp,model3_cp)

View(subset(or_data,or_data$calender_days>(365*8)))
View(subset(or_data_new,or_data_new$calender_days>(365*8)))

######################################################################
#Year classification

or_data_new=subset(or_data_new,or_data_new$IBLCp..s_current>0)
yearlydata2=or_data_new[,c("crane_type","crane_ser_no","calender_days","IBLCf..s_current","IBLCp..s_current")]
yearlydata2$group=     ifelse(yearlydata2$calender_days<365,"Year 1",
                              ifelse(yearlydata2$calender_days<(365*2),"Year 2",
                                     ifelse(yearlydata2$calender_days<(365*3),"Year 3",
                                            ifelse(yearlydata2$calender_days<(365*4),"Year 4",
                                                   ifelse(yearlydata2$calender_days<(365*5),"Year 5",
                                                          ifelse(yearlydata2$calender_days<(365*6),"Year 6",
                                                                 ifelse(yearlydata2$calender_days<(365*7),"Year 7",
                                                                        ifelse(yearlydata2$calender_days<(365*8),"Year 8",
                                                                               ifelse(yearlydata2$calender_days<(365*9),"Year 9",
                                                                                      ifelse(yearlydata2$calender_days<(365*10),"Year 10",
                                                                                             ifelse(yearlydata2$calender_days<(365*11),"Year 11",
                                                                                                    ifelse(yearlydata2$calender_days<(365*12),"Year 12","Year 13"))))))))))))
aaa=(aggregate(crane_type~group,data = yearlydata2,FUN = length))
######################################################################
#Determining crane series types wrt calender days
yearlydata2$crane.series=substr(yearlydata2$crane_type,start = 1,stop = 4)
yearlydata2$crane.model=substr(yearlydata2$crane_type,start = 8,stop = 12)

#wrt model pf the crane
yearlydata2$crane.model=ifelse(yearlydata2$crane.model=="IPRO","HIPRO",yearlydata2$crane.model)
yearlydata2$crane.model=ifelse(yearlydata2$crane.model=="","PRO",yearlydata2$crane.model)
yearlydata2$crane.model=ifelse(yearlydata2$crane.model=="O","PRO",yearlydata2$crane.model)


#for plotting group vs days
aaa = aaa[order(-aaa$crane_type),]
aaa$group =factor(aaa$group, as.character(aaa$group))
yearlydata2$group =factor(yearlydata2$group, as.character(yearlydata2$group))
yearlydata2 = yearlydata2[order(yearlydata2$calender_days),]

ggplot(data=aaa, aes(x=group, y=crane_type,group=1)) +
  geom_line() + labs(x="Year",y="Number of cranes") + ggtitle("Crane usage") +geom_point()
ggplot(yearlydata2, aes(x=crane.series, y=calender_days)) +  
  geom_boxplot((aes(fill = factor(crane.series)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))

#Determining crane series types wrt calender days wrt IBCLF scurrent
ggplot(yearlydata2, aes(x=crane.series, y=IBLCf..s_current)) +  
  geom_boxplot((aes(fill = factor(group)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))

abd=aggregate(formula=group~crane.series,data = yearlydata2,FUN = length)

#Determining crane model types wrt calender days wrt IBCLF scurrent.
#IMPORTANT!!!TRY CHANGING THE FACTOR line 2 under aes (fill= factor(*******))
ggplot(yearlydata2, aes(x=crane.model, y=calender_days)) +  
  geom_boxplot((aes(fill = factor(crane.series)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))

ggplot(yearlydata2, aes(x=crane.model, y=calender_days)) +  
  geom_boxplot((aes(fill = factor(crane.model)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))

ggplot(yearlydata2, aes(x=group, y=IBLCf..s_current)) +  
  geom_boxplot((aes(fill = factor(crane.series)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))
# + theme(panel.background = element_blank())

#Same as above but factor component in this case is crane model
ggplot(yearlydata2, aes(x=group, y=IBLCf..s_current)) +  
  geom_boxplot((aes(fill = factor(crane.model)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))


ggplot(yearlydata2, aes(x=crane.series, y=count(crane.series))) +  
  geom_boxplot((aes(fill = factor(crane.series)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))
#IMPORTANT!!!Svanre's requisition number 1: no.od cranes with seires number. 2. with model number
#3. CFs current vs year(series based) 4.CPs current vs year (Series based)
#5. CFs current vs year(model based) 6.CPs current vs year (model based)

######################################################################
#another set of plots. NOTE: WITHIN GG PLOT IF ONE WANTS TO HIDE THE LEGEND USE: theme(legend.position = "none")

Agg.set.yrserie=(aggregate(crane_ser_no~group+crane.series,yearlydata2,FUN = "length"))
# ggplot(Agg.set.yrserie, aes(x=group, y=crane_ser_no)) +  
#   geom_boxplot((aes(fill = factor(crane.series)))) + 
#   stat_summary(fun.y=median, geom="smooth", aes(group=1))
ggplot(data=Agg.set.yrserie, aes(x=group, y=crane_ser_no, group = crane.series, colour = crane.series)) +
  geom_line() + labs(x="Year",y="Number of cranes") + ggtitle("Crane usage of different crane series")+
  geom_point( size=4, shape=21, fill="white")

agg.set.yrmodel=(aggregate(crane_ser_no~group+crane.model,yearlydata2,FUN ="length"))
ggplot(data=agg.set.yrmodel, aes(x=group, y=crane_ser_no, group = crane.model, colour = crane.model)) +
  geom_line() + labs(x="Year",y="Number of cranes") + ggtitle("Crane usage of different crane model")+
  geom_point( size=2, shape=21, fill="black")


agg.set.ibclf=(aggregate(IBLCf..s_current~crane.series+group,yearlydata2,FUN ="mean"))
ggplot(data=agg.set.ibclf, aes(x=group, y=IBLCf..s_current, group = crane.series, colour = crane.series)) +
  geom_line() + labs(x="Year",y="IBLCFs current value (MEAN)") + ggtitle("IBLCFs current value of different crane series")+
  geom_point( size=2, shape=21, fill="black")

agg.set.ibclp=(aggregate(IBLCp..s_current~crane.series+group,yearlydata2,FUN ="mean"))
ggplot(data=agg.set.ibclp, aes(x=group, y=IBLCp..s_current, group = crane.series, colour = crane.series)) +
  geom_line() + labs(x="Year",y="IBLCPs current value (MEAN)") + ggtitle("IBLCPs current value of different crane series")+
  geom_point( size=2, shape=21, fill="black")


agg.set.ibclf.model=(aggregate(IBLCf..s_current~crane.model+group,yearlydata2,FUN ="mean"))
ggplot(data=agg.set.ibclf.model, aes(x=group, y=IBLCf..s_current, group = crane.model, colour = crane.model)) +
  geom_line() + labs(x="Year",y="IBLCFs current value (MEAN)") + ggtitle("IBLCFs current value of different crane MODELS")+
  geom_point( size=2, shape=21, fill="black")

agg.set.ibclp.model=(aggregate(IBLCp..s_current~crane.model+group,yearlydata2,FUN ="mean"))
ggplot(data=agg.set.ibclp.model, aes(x=group, y=IBLCp..s_current, group = crane.model, colour = crane.model)) +
  geom_line() + labs(x="Year",y="IBLCPs current value (MEAN)") + ggtitle("IBLCPs current value of different crane MODELS")+
  geom_point( size=2, shape=21, fill="black")

#Equation and rough plot for IBCLp vs IBCLf s_current distribution
plot(yearlydata2$IBLCp..s_current,yearlydata2$IBLCf..s_current)

#MODEL SEEMS TO BE REDUNDANT WITH EXTRA VARIABLE!!!!!!NAMELY CALENDAR DAYS
model_actual= lm(IBLCf..s_current~IBLCp..s_current , data=yearlydata2)
summary(model_actual)
abline(model_actual,lwd=3,col="red")

lines(lowess(yearlydata2$IBLCf..s_current~yearlydata2$IBLCp..s_current))
paste('Y =',coef(model_actual)[[2]],'* x','+',coef(model_actual)[[1]])

#tasks assigned by mattias
dummy_data_corrIBLCFs=subset(dummy_data_corrIBLCFs,dummy_data_corrIBLCFs$IBLCf..s_current<0.02)
# (Removed some odd points less than 0.02 as they seemed to be outliers)
ggplot(data=dummy_data_corrIBLCFs, aes(x=calender_days, y=IBLCf..s_current)) +
  geom_point() +
  geom_smooth()

###########################################################################################
#building polynomial model
#check for lesser numbe of days
dummy_data_corrIBLCFs=subset(dummy_data_corrIBLCFs,dummy_data_corrIBLCFs$calender_days<2556)
dummy_data_corrIBLCFs=subset(dummy_data_corrIBLCFs,dummy_data_corrIBLCFs$calender_days>356)
dummy_data_corrIBLCFs=subset(dummy_data_corrIBLCFs,dummy_data_corrIBLCFs$calender_days<1825)
plot(x = dummy_data_corrIBLCFs$calender_days,y = dummy_data_corrIBLCFs$IBLCf..s_current,
     main = "polynomial regression ",las=1)

#simple linear regression model
model1= lm(IBLCf..s_current~calender_days, data=dummy_data_corrIBLCFs)
summary(model1)
abline(model1,lwd=3,col="red")


coef(model1)
paste('y =', coef(model1)[[2]], '* x', '+', coef(model1)[[1]])

#now for degree 2
model2=lm(IBLCf..s_current ~ calender_days+ I(calender_days^2), data=dummy_data_corrIBLCFs)
summary(model2)
lines(smooth.spline(dummy_data_corrIBLCFs$calender_days,predict(model2)),col="green",lwd=10)
confint(object=model1,level = 0.95)

model3=lm(IBLCf..s_current ~ calender_days+ I(calender_days^2)+ I(calender_days^3), data=dummy_data_corrIBLCFs)
summary(model3)
lines(smooth.spline(dummy_data_corrIBLCFs$calender_days,predict(model2)),col="blue",lwd=10)

#compare model using F-test
anova(model1,model2,model3)

###########################################################################################
###########TASK 2
required=(aggregate(IBLCf..s_current~crane_ser_no ,data = dummy_data,FUN = length))
required=subset(required,!(required$IBLCf..s_current %in% c(1,2)))

# dummy_data=subset(dummy_data,dummy_data$lift_ctr>250)
plot(dummy_data$calender_days,dummy_data$IBLCf..s_current)
plot(dummy_data$calender_days,dummy_data$lift_ctr)

newmodel=lm(IBLCf..s_current~lift_ctr+calender_days, data=dummy_data)
summary(newmodel)

newagg=aggregate(crane_ser_no~IBLCf..s_current,data = required,FUN = count)
required=subset(required,required$IBLCf..s_current %in% 4)

temp=dummy_data
temp=subset(temp,temp$crane_ser_no %in% required$crane_ser_no)

hist(required$IBLCf..s_current)

ggplot(datsa=temp, aes(y=lift_ctr, x=lift_days, group = crane_ser_no, colour = crane_ser_no)) +
  geom_line() + labs(x="Days",y="Number of lifts") + ggtitle("Crane usage of different crane serial numbers")+
  geom_point( size=2, shape=21, fill="black")

###############CHANGING NUMBERS FRom here
required=(aggregate(IBLCf..s_current~crane_ser_no ,data = dummy_data,FUN = length))
required=subset(required,!(required$IBLCf..s_current %in% c(1,2)))

required=subset(required,(required$IBLCf..s_current %in% c(3:6)))


temp=dummy_data
temp=subset(temp,temp$crane_ser_no %in% required$crane_ser_no)
temp=subset(temp,temp$lift_ctr %in% c(0:800))

plot(dummy_data$lift_ctr,dummy_data$IBLCf..s_current)  

ggplot(data=temp, aes(y=lift_ctr, x=lift_days, group = crane_ser_no, colour = crane_ser_no)) +
  geom_line()+ labs(x="Days",y="Number of lifts") + ggtitle("Crane usage of different crane serial numbers")+
  geom_point( size=2, shape=21, fill="black")

ggplot(data=dummy_data, aes(y=IBLCf..s_current, x=lift_ctr, colour = crane_ser_no)) +
  geom_line()+ labs(x="Lift counter",y="IBLCF s current") + ggtitle("Crane usage of different crane serial numbers")+
  geom_point( size=2, shape=21, fill="black")


ggplot(data=temp, aes(x=lift_ctr, y=lift_days, group = crane_ser_no, colour = crane_ser_no)) +
  geom_line() + labs(x="Lift days",y="IBLCFs_current") + 
  ggtitle("Crane usage-Medium used ones:first  1000 lifts ")+ guides(fill=FALSE)
+ theme(legend.position = "none")
#geom_point( size=2, shape=19, fill="white")

###########################################################################################
#keep changing number här

temp=subset(hiab_data_1,hiab_data_1$IBLCf..s_current>0.032)

#####################################MONDAY task
check.fit=lm(formula =IBLCf..s_current~lift_ctr,data = dummy_data)
summary(check.fit) 
abline(check.fit)
paste('y =', coef(check.fit)[[2]], '* x', '+', coef(check.fit)[[1]])

temp=dummy_data
# temp=subset(temp,temp$lift_ctr %in% c(0:5000))
temp=subset(temp,temp$lift_ctr %in% c(0:5000))
plot(temp$lift_ctr,temp$IBLCf..s_current)  

check.fit=lm(formula =IBLCf..s_current~lift_ctr,data = temp)
summary(check.fit) 
abline(check.fit)
paste('y =', coef(check.fit)[[2]], '* x', '+', coef(check.fit)[[1]])

#####################################################################
###########DAMAGE VS USAGE PROFILES : considering the onlydesired columns of the data
dummy_data=dummy_data_corrIBLCFs
dummy_data=dummy_data[,c(3,7,8,(11:15),33,40)]
dummy_data$Slew_percent=(dummy_data$Time.for.Slew..../dummy_data$use_time..hour.)
dummy_data$IB_percent=(dummy_data$Time.for.IB..../dummy_data$use_time..hour.)
dummy_data$OB_percent=(dummy_data$Time.for.OB..../dummy_data$use_time..hour.)
dummy_data$extension_percent=(dummy_data$Time.for.Ext..../dummy_data$use_time..hour.)
dummy_data=subset(dummy_data,dummy_data$lift_ctr>300)

######################################################################
#Considering all fractions less than 5 as 95% of values lie in the given region
dummy_data=subset(dummy_data,(dummy_data$Slew_percent<5))
dummy_data=subset(dummy_data,(dummy_data$IB_percent<5))
dummy_data=subset(dummy_data,(dummy_data$OB_percent<5))
dummy_data=subset(dummy_data,(dummy_data$extension_percent<5))

plot(dummy_data$IB_percent,dummy_data$IBLCf..s_current)
plot(dummy_data$Slew_percent,dummy_data$IBLCf..s_current)
plot(dummy_data$OB_percent,dummy_data$IBLCf..s_current)
plot(dummy_data$extension_percent,dummy_data$IBLCf..s_current)

# aa=lm(formula = IBLCf..s_current~IB_percent+I(IB_percent^2),data = dummy_data)
# summary(aa)
dummy_data=subset(dummy_data,dummy_data$lift_ctr %in% c(1:200000))

ggplot(data=dummy_data, aes(y=IBLCf..s_current, x=Time.for.Slew...., group = crane_ser_no, colour = crane_ser_no)) +
  geom_line()+ labs(x="Slew percentage",y="Damage") + ggtitle("Damage with respect to tool mode percentage")+
  geom_point( size=0, shape=21, fill="black")+ theme(legend.position = "none")

ggplot(data=dummy_data, aes(y=IBLCf..s_current, x=Time.for.IB...., group = crane_ser_no, colour = crane_ser_no)) +
  geom_line()+ labs(x="IB percentage",y="Damage") + ggtitle("Damage with respect to tool mode percentage")+
  geom_point( size=0, shape=21, fill="black")+ theme(legend.position = "none")

ggplot(data=dummy_data, aes(y=IBLCf..s_current, x=Time.for.OB...., group = crane_ser_no, colour = crane_ser_no)) +
  geom_line()+ labs(x="OB percentage",y="Damage") + ggtitle("Damage with respect to tool mode percentage")+
  geom_point( size=0, shape=21, fill="black")+ theme(legend.position = "none")

ggplot(data=dummy_data, aes(y=IBLCf..s_current, x=Time.for.Ext...., group = crane_ser_no, colour = crane_ser_no)) +
  geom_line()+ labs(x="Extension percentage",y="Damage") + ggtitle("Damage with respect to tool mode percentage")+
  geom_point( size=0, shape=21, fill="black")+ theme(legend.position = "none")

