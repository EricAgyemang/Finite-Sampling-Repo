library(survey)


###stratificated data####
ht_stra=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/htstrat.csv",sep=",",header=T)

#########estimate####
N=1000
M1=160
M2=40
ht_stra[ht_stra[,3]=="F",4]=N/M1
ht_stra[ht_stra[,3]=="M",4]=N/M2
names(ht_stra)[4]="wgt_stra"   ###assign col name

stat_stra<- svydesign(id=~1, strata=~gender, weights =~wgt_stra, data=ht_stra)

svymean(~height,stat_stra)

svyquantile(~height, stat_stra, c(.5), ci=TRUE)

#######using Jacknife weights#
quantile_jk1<-as.svrepdesign(stat_stra)
svyquantile(~height,design=quantile_jk1, c(.5), ci=TRUE, interval.type="probability")

#######using BRR weights#
quantile_brr<-as.svrepdesign(stat_stra,type="BRR")
svyquantile(~height,design=quantile_brr, c(.5), ci=TRUE, interval.type="probability")
svyquantile(~height,design=quantile_brr, c(.5), ci=TRUE, interval.type="quantile")


#######using Bootstraping weights#
quantile_bt<-as.svrepdesign(stat_stra,type="bootstrap", replicates=5000)
svyquantile(~height,design=quantile_bt, c(.5), ci=TRUE, interval.type="probability")
svyquantile(~height,design=quantile_bt, c(.5), ci=TRUE, interval.type="quantile")

 