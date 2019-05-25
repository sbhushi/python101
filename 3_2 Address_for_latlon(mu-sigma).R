setwd("D:\\September")

Standardized_accountpool<-read.csv("Output files/Standard_output.csv",as.is=T)


Standardized_accountpool$ZIP_CODE<-mapply(function (x) sprintf("%05g",as.numeric(as.character(x)))
                                ,Standardized_accountpool$ZIP_CODE)
Standardized_accountpool$Address_for_latlon <- paste(Standardized_accountpool$ADDR_LINE_1,
                                              Standardized_accountpool$CITY,Standardized_accountpool$STATE,
                                              Standardized_accountpool$ZIP_CODE,sep=", ")

# Remove the '#' from address line1

Standardized_accountpool$Address_for_latlon<-gsub("#","",Standardized_accountpool$Address_for_latlon)
Standardized_accountpool$X<-NULL
write.csv(Standardized_accountpool,"other files/cleaned_accountpool.csv",row.names=FALSE)             
