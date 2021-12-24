index<-c(1:8)
y<-c(1,2,4,4,7,7,7,8)

library(prob)
library(combinat)


##without order##

#without replacement#
outcome1<-urnsamples(y,size=3,replace=FALSE,ordered=FALSE)
#with replacement#
outcome2<-urnsamples(y,size=3,replace=TRUE,ordered=FALSE)

#ybar and histgram#
oc1<-as.matrix(outcome1)
ybar1<-rowMeans(oc1)
hist(ybar1)

oc2<-as.matrix(outcome2)
ybar2<-rowMeans(oc2)
hist(ybar2)

###distribution###
Q1<-probspace(outcome1)
Q2<-probspace(outcome2)

##combine the same yi##
n1<-noorder(Q1)
n2<-noorder(Q2)
n1;n2

##distribution of ybar
d1<-cbind(rowMeans(n1[,c(1,2,3)]),n1[,4])
d2<-cbind(rowMeans(n2[,c(1,2,3)]),n2[,4])
d1;d2


##variance##  #without replacement#
Ey<-sum(d1[,1]*d1[,2])
Vy<-sum((d1[,1])^2*d1[,2])-Ey^2
Vy

##variance##  #with replacement#
Ey<-sum(d2[,1]*d2[,2])
Vy<-sum((d2[,1])^2*d2[,2])-Ey^2
Vy

