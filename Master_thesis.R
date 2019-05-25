#master thesis workplace
#install.packages("RODBC")
#install.packages("ggplot2")
#install.packages("stringi")
#install.packages("h2o")
#install.packages("tseries")


#E:\Master_thesis\data_files_use
library("tseries")
library("h2o")
library("stringi")
library("ggplot2")
library("RODBC")
library("stringr")

#reading the necessary files
hjulaxel.m28=read.csv(file = "E:\\Master_thesis\\data_files_use\\Hjul_axel_m28_missing.xls.csv",header = TRUE)
banmotor.m28=read.csv(file = "E:\\Master_thesis\\data_files_use\\Ban_motor_m28.csv",header = TRUE)
omforare.m28=read.csv(file = "E:\\Master_thesis\\data_files_use\\Omforare_m28.xls.csv",header = TRUE)

hjulaxel.m29=read.csv(file = "E:\\Master_thesis\\data_files_use\\hjulaxel_m29.xls.csv",header = TRUE)
banmotor.m29=read.csv(file = "E:\\Master_thesis\\data_files_use\\Banmotor_m29.csv",header = TRUE)
pedal.m29=read.csv(file = "E:\\Master_thesis\\data_files_use\\Pedal_m29.xls.csv",header = TRUE)

motoraxel.m31=read.csv(file = "E:\\Master_thesis\\data_files_use\\motoraxel_m31.xls.csv",header = TRUE)
hjulsats.m31=read.csv(file = "E:\\Master_thesis\\data_files_use\\hjulsats_m31.xls.csv",header = TRUE)
banmotor.m31=read.csv(file = "E:\\Master_thesis\\data_files_use\\Banmotor_m31.xls.csv",header = TRUE)

bromsaggregat.m32=read.csv(file = "E:\\Master_thesis\\data_files_use\\brake_assembly_m32.xls.csv",header = TRUE)
truck.m32=read.csv(file = "E:\\Master_thesis\\data_files_use\\truck_m32.xls.csv",header = TRUE)
handikapramp.m32=read.csv(file = "E:\\Master_thesis\\data_files_use\\handikapramp_m32.xls.csv",header = TRUE)



filedb=read.csv(file = "F:\\DATA\\GBG_tram\\PS_SUK_IN.csv",header = TRUE)
pairs(x = filedb)

#GSAB_EXJOB
#connecting to SQL server 2014
#the one for the falu energi
dbhandle = odbcDriverConnect('driver={SQL Server};server=DESKTOP-NOIC8N5;
                             database=AutArch;trusted_connection=true')

#for goteborg trams
dbhandle2 = odbcDriverConnect('driver={SQL Server};server=DESKTOP-NOIC8N5;
                             database=GSAB_EXJOB;trusted_connection=true')


#currTableSQL=paste("SELECT TOP 1000 [AutArch].[dbo].[ASignalDef],sep="")
#currTableDF=sqlQuery(dbhandle,currTableSQL)
current.table=sqlFetch(channel = dbhandle,sqtable = "ASignalDef",colnames = FALSE)

curr.table2=as.data.frame(sqlQuery(channel = dbhandle,query =" SELECT TOP 1000 [SignalID]
      ,[MaxDivergence],[FirstChangeTime]
                     ,[FirstValue] FROM [AutArch].[dbo].[ASignalLog]" ))


#plot(x = filedb$SUK_SQLIDENTITY,y = filedb$SUK_ID)


#extracting tables from GBG trams

ps.suk.in=sqlFetch(channel = dbhandle2,sqtable = "PS_SUK_IN",colnames = FALSE)
#Make sure you ask what each table means in this regard

ps.suk.ut=sqlFetch(channel = dbhandle2,sqtable = "PS_SUK_UT",colnames = FALSE)



#a huge dataset indeed!!
ps.sup.in=sqlFetch(channel = dbhandle2,sqtable = "PS_SUP_IN",colnames = FALSE)




ps.sup.komphist=sqlFetch(channel = dbhandle2,sqtable = "PS_SUP_KOMPHIST",colnames = FALSE)



#Progress bar scrapped 2due to time constraints in reading the data set 
# total = 20
# # create progress bar takes lot of time
# pb = txtProgressBar(min = 0, max = total, style = 3)
# for(i in 1:total){
#   #ps.suk.ut=sqlFetch(channel = dbhandle2,sqtable = "PS_SUK_UT",colnames = FALSE)
#   ps.sup.ut=sqlFetch(channel = dbhandle2,sqtable = "PS_SUP_UT",colnames = FALSE)
#   # update progress bar
#   setTxtProgressBar(pb, i)
# }
# close(pb)


ps.sup.ut=sqlFetch(channel = dbhandle2,sqtable = "PS_SUP_UT",colnames = FALSE)

R5ACTCHECKLISTS=sqlFetch(channel = dbhandle2,sqtable = "R5ACTCHECKLISTS",colnames = FALSE)

R5ACTIVITIES=sqlFetch(channel = dbhandle2,sqtable = "R5ACTIVITIES",colnames = FALSE)

R5ADDETAILS=sqlFetch(channel = dbhandle2,sqtable = "R5ADDETAILS",colnames = FALSE)

R5EVENTS=sqlFetch(channel = dbhandle2,sqtable = "R5EVENTS",colnames = FALSE)

R5OBJECTS=sqlFetch(channel = dbhandle2,sqtable = "R5OBJECTS",colnames = FALSE)

R5OBJUSAGEDEFS=sqlFetch(channel = dbhandle2,sqtable = "R5OBJUSAGEDEFS",colnames = FALSE)

R5PERSONNEL=sqlFetch(channel = dbhandle2,sqtable = "R5PERSONNEL",colnames = FALSE)

R5PPMSL=sqlFetch(channel = dbhandle2,sqtable = "R5PPMS",colnames = FALSE)

R5STANDWORKS=sqlFetch(channel = dbhandle2,sqtable = "R5STANDWORKS",colnames = FALSE,
                      rownames = FALSE)


#some exercises to remove NA values
temp=R5OBJECTS

temp = temp[,colSums(is.na(temp))<nrow(temp)]

!apply (is.na(temp), 2, all)

temp[,-!apply(is.na(temp), 2, all)]


#READ THE TABLES AS STATED

ps.komphist.vy=sqlFetch(channel = dbhandle2,sqtable = "PS_KOMPHIST_VY",colnames = FALSE,
                      rownames = FALSE)

ps.puaohi=sqlFetch(channel = dbhandle2,sqtable = "PS_PUAOHI",colnames = FALSE,
                        rownames = FALSE)


ps.suppao=sqlFetch(channel = dbhandle2,sqtable = "PS_SUPPAO",colnames = FALSE,
                   rownames = FALSE)


ps.suppkao=sqlFetch(channel = dbhandle2,sqtable = "PS_SUPPKAO",colnames = FALSE,
                   rownames = FALSE)

# pairs(x = ps.komphist.vy)
# pairs(x = ps.puaohi)
# pairs(x = ps.suppao)
# pairs(x = ps.suppkao)

##########################################################################################################################
#starting with component history
ps.komphist.vy$total_days=ps.komphist.vy$suhv_todate-ps.komphist.vy$suhv_fromdate #get time in seconds
ps.komphist.vy$total_days=ceiling(ps.komphist.vy$total_days/86400)                #number of seconds in a day round off
ps.komphist.vy$suhv_km=abs(ps.komphist.vy$suhv_km)          #since there was a lot of data with negative distancec travelled
ps.komphist.vy$komponent=word(string = ps.komphist.vy$Suhv_kopdesc)

dev.off()
ggplot(data = ps.komphist.vy,mapping = aes(x =suhv_km  ,y = total_days,color=Suhv_vagndesc)
       ,color="steelblue")+ geom_point() + geom_smooth()

ggplot(data = ps.komphist.vy,mapping = aes(x =suhv_km  ,y = total_days,color=Suhv_kopdesc)
       ,color="steelblue")+ geom_point() + geom_smooth()

ggplot(data = ps.komphist.vy,mapping = aes(x =suhv_km  ,y = total_days,color=komponent)
       ,color="steelblue")+ geom_point() + geom_smooth()


ggplot(data = ps.puaohi,mapping = aes(x =kmsenast  ,y = total_days,color=tram_type)
       ,color="steelblue")+ geom_point() + geom_smooth()

ggplot(data = ps.suppkao,mapping = aes(x =SUU_KM  ,y=SUK_MRC,color=tram_type)
       ,color="steelblue")+ geom_point() + geom_smooth()


#assign the tram types to all data frames
#for kompist data frame
ps.komphist.vy$tram_type=ifelse(ps.komphist.vy$Suhv_vagnid<400,"M31",
                                ifelse(ps.komphist.vy$Suhv_vagnid<500,"M32",
                                  ifelse(ps.komphist.vy$Suhv_vagnid<800,"M28",
                                    ifelse(ps.komphist.vy$Suhv_vagnid<900,"M29","M25"))))

ggplot(data = ps.komphist.vy,mapping = aes(x =suhv_km  ,y = total_days,color=tram_type)
       ,color="steelblue")+ geom_point() + geom_smooth()

#for puaohi data frame
ps.puaohi$evt_object=as.integer(ps.puaohi$evt_object)
ps.puaohi$tram_type=ifelse(ps.puaohi$evt_udfchar09<400,"M31",
                                ifelse(ps.puaohi$evt_udfchar09<500,"M32",
                                       ifelse(ps.puaohi$evt_udfchar09<800,"M28",
                                              ifelse(ps.puaohi$evt_udfchar09<900,"M29","M25"))))

#for suppao dataframe (old systems)
class(ps.suppao$SUI_OBJ)
ps.suppao$tram_type=ifelse(ps.suppao$SUI_OBJ<400,"M31",
                           ifelse(ps.suppao$SUI_OBJ<500,"M32",
                                  ifelse(ps.suppao$SUI_OBJ<800,"M28",
                                         ifelse(ps.suppao$SUI_OBJ<900,"M29","M25"))))

#for suppkao system(since the tram number doesnt exist use the data from the parts table
#using string extract)
ps.suppkao$tram_type=stri_sub(str = ps.suppkao$SUK_OBJB,from = -3,to = -1)

#total time execution in the workshop
ps.puaohi$total_time=ps.puaohi$EVT_COMPLETED-ps.puaohi$EVT_REPORTED
ps.puaohi$total_time=ceiling(ps.puaohi$total_time/86400)       #for days     




#for (i in 1:10) print(i:1)
str(ps.komphist.vy)
library("nnet")
#testing a multinomial regression model
test = multinom(tram_type ~ suhv_km + komponent, data = ps.komphist.vy)
summary(test)


#for considering dates after may 1 2016

ps.komphist.vy.new=subset(x = ps.komphist.vy,ps.komphist.vy$suhv_fromdate>"2016-05-01")

ps.komphist.vy.new$tram_code=ifelse(ps.komphist.vy.new$tram_type=="M28",1,
                                 ifelse(ps.komphist.vy.new$tram_type=="M29",2,
                                         ifelse(ps.komphist.vy.new$tram_type=="M31",3,4)))

ps.komphist.vy.new$tram_code=as.integer(ps.komphist.vy.new$tram_code)

#########################################################
#could have used switch case for the one below, but the attempts failed
#########################################################
ps.komphist.vy.new$komponent_code= ifelse(ps.komphist.vy.new$komponent=="A",1,
                                   ifelse(ps.komphist.vy.new$komponent=="B",2,
                                   ifelse(ps.komphist.vy.new$komponent=="Banmotor",3,
                                   ifelse(ps.komphist.vy.new$komponent=="Bromsaggregat",4,
                                   ifelse(ps.komphist.vy.new$komponent=="Bromsok",5, 
                                   ifelse(ps.komphist.vy.new$komponent=="Färdskrivare",6,
                                   ifelse(ps.komphist.vy.new$komponent=="Fjäderbromscylinder",7,
                                   ifelse(ps.komphist.vy.new$komponent=="Fotpedal",8,     
                                   ifelse(ps.komphist.vy.new$komponent=="Handikappramp",9,       
                                  ifelse(ps.komphist.vy.new$komponent=="Handkontroll",10, 
                                  ifelse(ps.komphist.vy.new$komponent=="Hjulaxel",11, 
                                  ifelse(ps.komphist.vy.new$komponent=="Hjulsats",12,   
                                  ifelse(ps.komphist.vy.new$komponent=="Kompressor",13,              
                                  ifelse(ps.komphist.vy.new$komponent=="Länkstång",14,    
                                  ifelse(ps.komphist.vy.new$komponent=="Linjebrytare",15, 
                                  ifelse(ps.komphist.vy.new$komponent=="Motor",16,
                                  ifelse(ps.komphist.vy.new$komponent=="Motoraxel",17,       
                                  ifelse(ps.komphist.vy.new$komponent=="Omformare",18,       
                                  ifelse(ps.komphist.vy.new$komponent=="Övre",19,
                                  ifelse(ps.komphist.vy.new$komponent=="Pedal",20,
                                  ifelse(ps.komphist.vy.new$komponent=="Proportionalventil",21,
                                  ifelse(ps.komphist.vy.new$komponent=="Pumpenhet",22,
                                  ifelse(ps.komphist.vy.new$komponent=="Strömavtagare",23,
                                  ifelse(ps.komphist.vy.new$komponent=="Styrbox",24,                                                                                   
                                  ifelse(ps.komphist.vy.new$komponent=="Tångbroms",25,                                          
                                  ifelse(ps.komphist.vy.new$komponent=="Truck",26,27                                                           
                                                                                    ))))))))))))))))))))))))))





#only 4621 rows obtained having 2591 na values in todate

ggplot(data = ps.komphist.vy.new,mapping = aes(x =suhv_km  ,y = total_days,color=Suhv_kopdesc)
       ,color="steelblue")+ geom_point() + geom_smooth()



#reeeeli interesting plot days
ggplot(data = ps.komphist.vy.new  ,mapping = aes(x =suhv_km  ,y = total_days,color=tram_type)
       ,color="steelblue")+ geom_point() + geom_smooth()


#reeeeli interesting plot distance
ggplot(data = ps.komphist.vy.new  ,mapping = aes(x =suhv_km  ,y = total_days,color=tram_type)
       ,color="steelblue")+ geom_point() + geom_smooth()




model3=lm(suhv_km~total_days,data = ps.komphist.vy.new)

ggplot(data=ps.komphist.vy.new,mapping=aes(suhv_km,total_days))+stat_summary(fun.data=mean_cl_normal)+
  geom_smooth(method='lm',formula=ps.komphist.vy.new$suhv_km~ps.komphist.vy.new$total_days)






#with lm model 
# ggplot(data = ps.komphist.vy.new  ,mapping = aes(x =suhv_km  ,y = total_days,color=tram_type)
#        ,color="steelblue")+ geom_point() + geom_smooth(lm(suhv_km ~ total_days+tram_type, data=ps.komphist.vy.new))


ggplot(data = ps.komphist.vy.new  ,mapping = aes(x =suhv_km  ,y = total_days,color=komponent)
       ,color="steelblue")+ geom_point() + geom_smooth()
plot(as.numeric(ps.komphist.vy.new$total_days),ps.komphist.vy.new$suhv_km)


#check for a regression model
ps.komphist.vy.new$total_days=as.numeric(ps.komphist.vy.new$total_days)
#model1=lm(formula = tram_type~total_days+suhv_km,data = ps.komphist.vy.new,na.action = na.exclude)


temp=lm(suhv_km ~ total_days+tram_type, data=ps.komphist.vy.new)
dev.off()
plot.new()


plot(ps.komphist.vy.new$total_days, ps.komphist.vy.new$suv_km)
#abline(temp, lwd=2)

#abline(lm(suhv_km ~ total_days+tram_type, data=ps.komphist.vy.new))

line(predict(temp))
hist(ps.komphist.vy.new$komponent)



#building an arima model )for the time series in

#for tram type 1 (m28)

m28=subset(x = ps.komphist.vy.new,ps.komphist.vy.new$tram_code==1)
m29=subset(x = ps.komphist.vy.new,ps.komphist.vy.new$tram_code==2)
m31=subset(x = ps.komphist.vy.new,ps.komphist.vy.new$tram_code==3)
m32=subset(x=ps.komphist.vy.new,ps.komphist.vy.new$tram_code==4)

ggplot(data = m28  ,mapping = aes(x =suhv_km  ,y = total_days)
       ,color="steelblue")+ geom_point() + geom_smooth()

ggplot(data = m29  ,mapping = aes(x =suhv_km  ,y = total_days)
       ,color="steelblue")+ geom_point() + geom_smooth()


#plot a scatter plot wherein the x axis has the distance .and the different components lie scattered

ggplot(data = m29  ,mapping = aes(x =suhv_km  ,y = total_days)
       ,color="steelblue")+ geom_point() + geom_smooth()



# p = ggplot(data = m31, aes(x = suhv_km, y = total_days)) +
#   geom_smooth(method = "lm", se=FALSE, color="black", formula = m31$suhv_km ~ m31$total_days) +
#   geom_point()
# p

ggplot(data = m31  ,mapping = aes(x =suhv_km  ,y = total_days)
       ,color="steelblue")+ geom_point() + geom_smooth()

ggplot(data = m32  ,mapping = aes(x =suhv_km  ,y = total_days)
       ,color="steelblue")+ geom_point() + geom_smooth()



#for one particular component..Brake assempbly

bromsaggregat.m32$date=(word(bromsaggregat.m32$Slutförandedatum))
as.Date(bromsaggregat.m32$date, "%d/%m/%Y")


ggplot(banmotor.m28, aes(x=Slutförandedatum, y=km.sedan.revision.utf)) +  
  #geom_line(data = cranemore200d,aes(x=group, y=mean(IBLCf..s_current)))+geom_boxplot()
  geom_boxplot((aes(fill = factor(group)))) + 
  stat_summary(fun.y=median, geom="smooth", aes(group=1))



