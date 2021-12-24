library(survey)


###load statistics class data####
dd=read.table("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/stat_class.txt",sep="",header=F)

#Select a sample of size 5 with pps and without replacement
library(pps) 

dpps_ht<-sampford(dd[,2],5)
dpps_ht
dpps_ht_joint=sampfordpi(dd[,2],5)  ##return the inclusion prob. and joint inclusion prob.
dpps_ht_joint
###read the selected data####
stat_dd=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/classppsjp.csv",sep=",",header=T)
head(stat_dd)

####read the selected y's#########
stat_y=read.table("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/stat_class_y.txt",sep="",header=F)
colnames(stat_y)=c("classid","size","SelectionProb","SamplingWeight","hours")


stat_pps<- svydesign(id=~classid, fpc=~SelectionProb, weights =~SamplingWeight, data=stat_y, pps="brewer")

####estimate the total and mean########
svytotal(~hours,stat_pps)
svymean(~hours,stat_pps)




