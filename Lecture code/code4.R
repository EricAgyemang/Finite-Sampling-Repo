library(survey)

#############data package from book######
library(SDaA)

###load stratified sample####
dd=read.table("http://my.ilstu.edu/~mxu2/Spring-2020/MAT450/datagpa.txt",sep="",header=F)

# One-stage cluster sample  

N =100
n =5
M =4
wgt = N/n

colnames(dd)=c("suites_id","gpa")

fpc <- c(rep(N,n*M))

gpa_dat <- data.frame(cbind(dd,fpc))

dsgn_gpa<- svydesign(ids=~suites_id,weights=c(rep(wgt,n*M)),fpc=~fpc,data=gpa_dat)
dsgn_gpa
esttotal <- svytotal(~gpa,design=dsgn_gpa)
esttotal 
confint(esttotal,df=n-1)

estmean <- svymean(~gpa,design=dsgn_gpa)
estmean
confint(estmean,df=n-1)

#################
algebra=read.table("http://my.ilstu.edu/~mxu2/Spring-2020/MAT450/algebra.csv",sep=",",header=T)

N=187
n=12

algebra$wgt<- c(rep(N/n,299))

algebra$fpc<- c(rep(N,299))


# Create the sampling design

dsgn_ratio <-svydesign(ids=~class,weights=~wgt,fpc=~fpc,data=algebra)

 
# Estimation of population mean

est_mean <- svymean(~score,design=dsgn_ratio)
est_mean

confint(est_mean,level=.95,df=n-1)

#  Estimation of population total

est_total<- svytotal(~score,design=dsgn_ratio)

est_total

#####################two stage cluster sampling######
data("coots")


n=184
m=2

coots$wgt<- coots$csize/2

 


# Create the sampling design

dsgn_ratio <-svydesign(ids=~clutch,weights=~wgt,data=coots)


# Estimation of population mean

est_mean <- svymean(~volume,design=dsgn_ratio)
est_mean

confint(est_mean,level=.95,df=n-1)







