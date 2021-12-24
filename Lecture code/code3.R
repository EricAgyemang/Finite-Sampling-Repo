library(survey)

#############data package from book######
library(SDaA)

#########load simple random sample################
data(agsrs)

###############assume population X is known for 87#########################
N=3078        # population size
n=300         # sample size

tx=964470625                 
barxmu = tx/N                    
 
#ratio<- mean(agsrs$acres92)/mean(agsrs$acres87)
fpc <- c(rep(N,n))

# Create the sampling design
agdsgn <- svydesign(data=agsrs,id=~1,fpc=~fpc)

# Estimation of the ratio

agratio <- svyratio(~acres92,~acres87,design=agdsgn)

#####confidence interval for the B######
confint(agratio,df=299)

#########confidence interval for y#########
#####known X mean########
ratio_mean=barxmu*agsrs$acres92

agratio_mean <- svyratio(~ratio_mean,~acres87,design=agdsgn)

confint(agratio_mean,df=299)

########X mean is unknown##########
mnhatx  = mean(agsrs$acres87)   
ratio_umean  = mnhatx*agsrs$acres92
agratio_umean <- svyratio(~ratio_umean,~acres87,design=agdsgn)
confint(agratio_umean,df=299)

######total##################################################
#####known tx########
ratio_t=tx*agsrs$acres92

agratio_t <- svyratio(~ratio_t,~acres87,design=agdsgn)
agratio_t 
confint(agratio_t,df=299)

########tx is unknown##########
utx  = N*mean(agsrs$acres87)   
uratio_t=utx*agsrs$acres92

agratio_ut <- svyratio(~uratio_t,~acres87,design=agdsgn)
agratio_ut 
confint(agratio_ut,df=299)


#####################proportion example######
y=c(0,0,1,2,10,15,3,2,1,27)
x=c(1,0,8,2,76,60,25,2,1,31)

prop=data.frame(y,x)
 

# Create the sampling design
prodsgn <- svydesign(data=prop,id=~1,fpc = ~0)

# Estimation of the ratio

propratio <- svyratio(~y,~x,design=prodsgn)
propratio
#####confidence interval for the B######
confint(propratio,df=9)


##############



