library(survey)



#############data package from book######
library(SDaA)

data(agsrs)

agsrs$fpc=3078

srs_design <- svydesign(ids=~1, fpc=~fpc, data=agsrs)
srs_design

svymean(~acres92,srs_design)
svytotal(~acres92,srs_design)

confint(svymean(~acres92,df=degf(srs_design),srs_design))  ####still use normal approximation#####

lt200k=ifelse(agsrs$acres92>200000,1,0)

##############proportion#####################
svymean(~lt200k,srs_design)
confint(svymean(~lt200k,df=degf(srs_design),srs_design)) 

#######draw sample####
library(sampler)

rsamp(df=agsrs, n=20, rep=FALSE)  ###replacement false


