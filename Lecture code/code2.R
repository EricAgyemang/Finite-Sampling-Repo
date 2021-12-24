library(survey)

#############data package from book######
library(SDaA)

###load stratified sample####
data(agstrat)

summary(agstrat$region)
levels(agstrat$region)

agstrat$fpc=c(rep(1054,103),rep(220,21),rep(1382,135),rep(422,41))


################look at each stratum######
strat_design <- svydesign(id=~1, fpc=~fpc, data=agstrat[1:103,])
strat_design

svymean(~acres92,strat_design)
svytotal(~acres92,strat_design)


###################stratified estimate########################
strat_design <- svydesign(id=~1, fpc=~fpc, strata=~region, data=agstrat)
strat_design

svymean(~acres92,strat_design)
svytotal(~acres92,strat_design)

confint(svymean(~acres92,strat_design),df=degf(strat_design))   
 
####weights:example####
1054/103

