#Address Cleaning

## Input for address cleaning

setwd("D:\\September")

library(plyr)
replace <-read.csv("other files/replace_final.csv",as.is=T)
input_file <- read.csv(file="Output files/accountpool_with_addr.csv",as.is=T)
data.add.clean<-unique(input_file)
#write.csv(data.add.clean,"chk.csv", row.names=F)
data.add.clean$ADDR_LINE_2<-as.character(data.add.clean$ADDR_LINE_2)
data.add.clean$ADDR_LINE_2<-ifelse(is.na(data.add.clean$ADDR_LINE_2),"",data.add.clean$ADDR_LINE_2)
data.add.clean<-unique(data.add.clean)
data.add.clean <- as.matrix(data.add.clean)
data.add.clean[data.add.clean=='?'] <- ''
data.add.clean <- as.data.frame(data.add.clean,stringsAsFactors =F)
data.add.clean$ADDR_LINE_2<-paste(" ",data.add.clean$ADDR_LINE_2)
data.add.clean$ADDR_LINE_1_OLD <- data.add.clean$ADDR_LINE_1 
data.add.clean$ADDR_LINE_2_OLD <- data.add.clean$ADDR_LINE_2

data.add.clean<-data.frame(lapply(data.add.clean, function(v) {
  if (is.character(v))return(toupper(v))
  else return(v) }))
# Trim function
trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))

## Replace abbreviations in the data


data.add.clean$ADDR_LINE_1 <- paste(data.add.clean$ADDR_LINE_1,' ')
data.add.clean$ADDR_LINE_2 <- paste(data.add.clean$ADDR_LINE_2,' ')

for (i in 1:nrow(replace)){
  data.add.clean$ADDR_LINE_1 <- sub(replace[i,1],replace[i,2]
                                    ,data.add.clean$ADDR_LINE_1,fixed=T)
  data.add.clean$ADDR_LINE_2 <- sub(replace[i,1],replace[i,2]
                                    ,data.add.clean$ADDR_LINE_2,fixed=T)
}

data.add.clean$ADDR_LINE_1 <- trim(data.add.clean$ADDR_LINE_1)
data.add.clean$ADDR_LINE_2 <- trim(data.add.clean$ADDR_LINE_2)




add1 <- substr(data.add.clean$ADDR_LINE_1,1,1) %in% c('#','1','2','3','4','5',
                                                      '6','7','8','9','0')
add2 <- substr(data.add.clean$ADDR_LINE_2,1,1) %in% c('#','1','2','3','4','5',
                                                      '6','7','8','9','0')
chk <- !(add1) & add2
data.add.clean$ADDR_LINE_1[which(chk)] <- data.add.clean$ADDR_LINE_2[which(chk)]

## Move parts of the add1 containing info like "suite" to add2

strings <- c(" SUITE"," BUILDING"," FLOOR",
             " ROOM"," APARTMENT"," FLOOR ",
             " FLOOR, "," FLOOR. ","STE","SUITE",
             "ST")
rm(chk)
for(str in strings){
  
  temp <- regexpr(str,data.add.clean$ADDR_LINE_1)
  temp[which(temp==-1)]<- Inf
  
  if(str %in% c(" FLOOR"," FLOOR "," FLOOR, "," FLOOR. ")){
    str.logic1<- temp-4
    str.logic1[which(str.logic1<0)] <- 0
    str.logic1[which(str.logic1==Inf)] <- 0
    str.logic2 <- temp
    str.logic2[which(str.logic2==Inf)] <- 0
    logic <- grepl("[0-9]",substr(data.add.clean$ADDR_LINE_1
                                  ,str.logic1,str.logic2))
    temp[which(logic)] <- temp[which(logic)]-4
    temp[which(temp<0)] <- 0 
  }
  
  if(str %in% c("STE","SUITE","ST")){
    
    temp[which(temp==Inf)] <- nchar(data.add.clean
                                    $ADDR_LINE_1[which(temp==Inf)])+1
    temp[which(!(substr(data.add.clean$ADDR_LINE_1
                        ,temp+nchar(str),temp+nchar(str)) %in%
                   c('#','1','2','3','4','5','6','7','8','9','0')))] <- Inf
  }
  
  if(exists('chk')){
    chk <- pmin(chk,temp)
  }else{
    chk <- temp
  } 
}

chk[which(chk==Inf)] <- nchar(data.add.clean$ADDR_LINE_1[which(chk==Inf)])+1
data.add.clean$ADDR_LINE_2 <- paste(substr(data.add.clean$ADDR_LINE_1,chk,
                                           nchar(data.add.clean$ADDR_LINE_1))
                                    ,data.add.clean$ADDR_LINE_2)
data.add.clean$ADDR_LINE_1 <- substr(data.add.clean$ADDR_LINE_1,1,chk-1)

# Remove commas towards the end and trim
data.add.clean$ADDR_LINE_1<- trim(gsub(",+$", "",trim(data.add.clean$ADDR_LINE_1)))

## Address standardization: Zipcodes and ATTN

data.add.clean$ZIP_CODE<-mapply(function (x) sprintf("%05g",as.numeric(as.character(x)))
                                ,data.add.clean$ZIP_CODE)


# Remove add1/add2 with irrelavant values
data.add.clean <- mutate(data.add.clean,ADDR_LINE_1 = 
                           ifelse(tolower(ADDR_LINE_1) %in% c("unknown","dummy")|
                                    grepl("^attn|attn:|warehouse|receiving",
                                          tolower(ADDR_LINE_1)), NA, ADDR_LINE_1),
                         ADDR_LINE_2 = 
                           ifelse(ADDR_LINE_2 == "" | ADDR_LINE_2 == " " | 
                                    tolower(ADDR_LINE_2) %in% c("unknown","dummy") 
                                  | grepl("^attn|attn:|warehouse|receiving",
                                          tolower(ADDR_LINE_2)), NA, ADDR_LINE_2)
)

# Remove add1/add2/city with all numbers
data.add.clean <- mutate(data.add.clean,ADDR_LINE_1 = 
                           ifelse(grepl("^[0-9]+$",data.add.clean$ADDR_LINE_1),
                                  NA, ADDR_LINE_1),
                         ADDR_LINE_2 = 
                           ifelse(grepl("^[0-9]+$", data.add.clean$ADDR_LINE_2),
                                  NA, ADDR_LINE_2),
                         
)  

data.add.clean <- mutate(data.add.clean,ADDR_LINE_2 = 
                           ifelse(is.na(ADDR_LINE_1) | ADDR_LINE_1 == ADDR_LINE_2
                                  , NA, ADDR_LINE_2)
)

# Trim function defined in the main module
data.add.clean$ADDR_LINE_1 <- trim(data.add.clean$ADDR_LINE_1)
data.add.clean$ADDR_LINE_2 <- trim(data.add.clean$ADDR_LINE_2)
data.add.clean$CITY <- trim(data.add.clean$CITY)
data.add.clean$STATE <- trim(data.add.clean$STATE)
data.add.clean$ZIP_CODE <- trim(data.add.clean$ZIP_CODE)

write.csv(data.add.clean,"other files/cleaned_accountpool_without_format.csv",row.names=FALSE)             

#data.add.clean$Address_for_latlon <- paste(data.add.clean$ADDR_LINE_1,data.add.clean$CITY,data.add.clean$STATE,
#                                           data.add.clean$ZIP_CODE,sep=", ")

# Remove the '#' from address line1

#data.add.clean$Address_for_latlon<-gsub("#","",data.add.clean$Address_for_latlon)

#write.csv(data.add.clean,"other files/cleaned_accountpool.csv",row.names=FALSE)             
