install.packages("boot")
require("boot")
require("stats4")
#################################################################
getwd()
df=rgamma(n = 100,shape = 2,scale = 2)
hist(df)

calc.mean=mean(df)
calc.var=var(x = df)
paste(calc.mean)
paste(calc.var)



##################################################################
#assignment 2.1
#taking out a sample mean
View(as.data.frame(df))

x=matrix(data = NA,nrow = 1500)

for(i in 1:1500)
{
  x[i]=mean(sample(x = df,size = 100,replace = TRUE))
}
  
hist(x)
bias=mean(x)-calc.mean  
var=(1/1499)*(sum((x-calc.var)^2)) 

mean(x)
##################################################################
#assignment 2.2
meanfunc=function(df,i){mean(df[i])}
bootobj=boot(data = df,R = 1500,statistic = meanfunc)
#abc=boot(data = df,statistic = mean,R = 1500)
bootobj
#new.variance=bootobj$std.error rm(new.variance)
summary(bootobj)
#################################################################
#assignment 2.3

# alpha=0.05
# b_lcl=2*calc.mean-(qt(1-(alpha/2),mean(x)))
# b_ucl=2*calc.mean-(qt(alpha/2,mean(x)))
# 
# Basic.CI=b_ucl-b_lcl
# paste(b_lcl,b_ucl)
# paste(Basic.CI)
# #rm(b_lcl,b_ucl)
# percentile.CI=qt(1-(alpha/2),mean(x))-qt(alpha/2,mean(x))

###using the boot.ci function to generate CI intervals
aa=boot.ci(boot.out = bootobj,type = "basic")
aa=sort(aa)
#to find z value
Z=(bootobj$t-calc.mean)/sd(bootobj$t)

#basic CI
n=1000
alpha=0.05
b.lower=sort(mean(aa))
upper.basic=1001*(1-(alpha/2))
lower.basic=1001*(alpha/2)
Bu=2*calc.mean-aa[upper.basic]
Lu=2*calc.mean-aa[lower.basic]

basic=c(Bu,Lu)
print(basic)

#normal CI
bootobj.sort=sort(bootobj$t)
std.dev=sd(bootobj$t)
Z
Nu=calc.mean-Z[975]
Nl=calc.mean-Z[25]
print(c(Nu,Nl))

#percentile CI
pu=bootobj.sort[(1000*(1-alpha/2))]
pl=bootobj.sort[1000*alpha/2]
print(c(pu,pl))

###################################################################
#assignment 2.4

w=seq(10,100,10)
count=rep(0,9)
for(j in 1:1000){
  for (i in 1:length(w)){
    a= rgamma(w[i],2,scale=2)
    # mult=function(x,i){mean(x[i])}
    meanfunc=function(x,i){mean(x[i])}
    booter=boot(a,statistic = meanfunc,R=1000,sim = "ordinary")
    mul.ci=boot.ci(booter,conf= 0.95,type ="norm")$norm
    if (mul.ci[2]<=4  && mul.ci[3]>= 4){
      count[i] = count[i]+1;
    }
  }
}
print(count)
plot(count)
######################################################################
# for(count in c(10,20,30,40,50,60,70,80,90,100) )
#df2=rgamma(n = count,shape = 2,scale = 2)
   
## attempt 2  theor.mean=mean(df2)

# bootci=list(100)
# bootobjcts=matrix(data = NA,nrow = 100)
# boot.upper=integer(length = 1000)
# boot.lower=integer(length = 1000)
# for (loop in 1:1000)
# {
# bootcimat=matrix(0,10,2)
# for(i in 1:10)
# {
#  #for each i, i get 1 boot object 
# # bootobjcts[i]=boot(data = (sample(x = df2,size = 10,replace = TRUE))
# #                    ,R = 1000,statistic = meanfunc)
# # boot.ci(boot.out = bootobjcts[i],type = "basic")
#   n=i*10
#   df2=rgamma(n ,shape = 2,scale = 2)
#   
#    bootcimat[i,]=boot.ci(boot.out=boot(data = (sample(x = df2,size = 10,replace = TRUE))
#                                     ,R = 1000,statistic = meanfunc),type = "norm")$norm[2:3]
#    
#   
#   #bootcimat[i,]=boot.ci(boot.out = df2,type = "norm")
#   
#    #ifelse(theor.mean<boot.upper[i] && theor.mean>boot.lower[i],n+1,n)
#    if(mean(df2)<bootcimat[i,2])
#    if(theor.mean>bootcimat[i,1])
#    j=j+1
#   #
#   
# }
# }
# n
# 
# 
# plot(boot.upper)
# plot(boot.lower)
# plot(boot.lower,boot.upper)


##########################################################################
#assignment 3.1
# maximum-Likelihood Estimation (MLE) is a
# statistical technique for estimating model 
# parameters. It basically sets out to answer
# the question: what model parameters are most
# likely to characterise a given set of data

set.seed(100)
newdf=rgamma(n = 100,shape = 2,scale = 2)
mean(newdf)
sd(newdf)

#fitting the model of maximum likelihood by using LOG-likelihood estimation

log.likelihood=function(k,gma)
{
  R=dgamma(newdf,shape = k,scale = gma)
  -sum(log(R))
}
  
 mle(log.likelihood,start = list(k=1,gma=1)) 
#check for dgmma for na error  
 value=mle(log.likelihood, start = list(k = 1, gma=1), method = "L-BFGS-B", lower = c(-Inf, 0),
     upper = c(Inf, Inf))
 value
  
 ###########################################################################
 #assignment 3.2
 
 #the values of shape and scale are taken from previous question
 new.sample=rgamma(n=100,shape = 2.21,scale = 1.71)

 #the values found were close to original shape and size
 
 aa=matrix(data = NA,nrow = 1500)

 # my.func=function()
 # {
    
 for(i in 1:1500)
 {
   aa[i]=mean(sample(x = new.sample,size = 100,replace = TRUE))
 }
   
 #}
 #my.func()
 
 hist(aa)
 bias=mean(aa)-mean(new.sample) 
 bias
 var2=(1/1499)*(sum((aa-mean(new.sample))^2)) 
 var2
 mean(x)
 
 ###########################################################################
 #assignment 3.3
 meanfunc.two=function(abcd,i){mean(abcd[i])}
bootsummary=boot(data = new.sample,R = 1500,statistic = meanfunc.two)
 variance=var(bootsummary$t)
 variance
 
 ###########################################################################
 #assignment 3.4
 # for(count in c(10,20,30,40,50,60,70,80,90,100) )
 #df2=rgamma(n = 10,shape = 2,scale = 2)
 set.seed(100)
 w=seq(10,100,10)
 count=rep(0,9)
 for(j in 1:1000){
   for (i in 1:length(w)){
     a= rgamma(w[i],shape = 2.21,scale=1.71)
     # mult=function(x,i){mean(x[i])}
     meanfunc=function(x,i){mean(x[i])}
     booter=boot(a,statistic = meanfunc,R=1000,sim = "ordinary")
     mul.ci=boot.ci(booter,conf= 0.95,type ="norm")$norm
     if (mul.ci[2]<=4  && mul.ci[3]>= 4){
       count[i] = count[i]+1;
     }
   }
 }
 print(count)
 plot(count)
 
 
 
 
 # bootci=list(100)

 
 ###########################################################################
 #assignment 4
 #assignment 2.4 to be repeated but using variance instead of mean
 #check for CI here as well
 
 varfunc=function(df,i){var(df[i])}
 theor.mean=var(df2)
 
 # for (loop in 1:1000)
 # {
   bootcimat=matrix(0,10,2)
   for(i in 1:10)
   {
     #for each i, i get 1 boot object 
     # bootobjcts[i]=boot(data = (sample(x = df2,size = 10,replace = TRUE))
     #                    ,R = 1000,statistic = meanfunc)
     # boot.ci(boot.out = bootobjcts[i],type = "basic")
     n=i*10
     df2=rgamma(n ,shape = 2,scale = 2)
     
     bootcimat[i,]=boot.ci(boot.out=boot(data = (sample(x = df2,size = 10,replace = TRUE))
                                         ,R = 1000,statistic = varfunc),type = "norm")$norm[2:3]
     
     
     #bootcimat[i,]=boot.ci(boot.out = df2,type = "norm")
     
     #ifelse(theor.mean<boot.upper[i] && theor.mean>boot.lower[i],n+1,n)
     if(4<bootcimat[i,2])
       if(4>bootcimat[i,1])
         j=j+1
     #
     
   }
 # }
 # n
 # 
 
#comment about the performance
   
   aaaa=boot.ci(boot.out=boot(data = (sample(x = df2,size = 10,replace = TRUE))
                         ,R = 1000,statistic = varfunc),
           type = "norm")
   
   
   