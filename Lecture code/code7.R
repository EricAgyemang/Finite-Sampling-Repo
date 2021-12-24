library(survey)

library(SDaA)
data("coots")

#####################original design#################
n=184
m=2

coots$wgt<- coots$csize/2

# Create the sampling design
dsgn_ratio <-svydesign(ids=~clutch,weights=~wgt,data=coots)

# Estimation of population mean
est_mean <- svymean(~volume,design=dsgn_ratio)
est_mean

confint(est_mean,level=.95,df=n-1)
#####################################replicates approach#####

## convert to JK1 jackknife
mean_jk1<-as.svrepdesign(dsgn_ratio)

est_mean_jk1 <- svymean(~volume,design=mean_jk1)
est_mean_jk1

## convert to bootstrap
mean_bt<-as.svrepdesign(dsgn_ratio,type="bootstrap", replicates=1000)
est_mean_bt <- svymean(~volume,design=mean_bt)
est_mean_bt



