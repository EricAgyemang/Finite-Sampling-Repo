library(survey)


###load SRS and stratificated data####
ht_srs=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/htsrs.csv",sep=",",header=T)
ht_stra=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/htstrat.csv",sep=",",header=T)
 
####estimate the statistics quantities for SRS ########
N=2000
n=200
ht_srs$fpc_srs=N
ht_srs$wgt_srs=N/n

stat_pps<- svydesign(id=~1, fpc=~fpc_srs, weights =~wgt_srs, data=ht_srs)

svymean(~height,stat_pps)

svyquantile(~height, stat_pps, c(.25,.5,.9), ci=TRUE)

svyvar(~height,stat_pps)

############estimate the statistics quantities for stratification########
N=1000
M1=160
M2=40
ht_stra[ht_stra[,3]=="F",4]=N/M1
ht_stra[ht_stra[,3]=="M",4]=N/M2
names(ht_stra)[4]="wgt_stra"   ###assign col name

stat_stra<- svydesign(id=~1, strata=~gender, weights =~wgt_stra, data=ht_stra)

svymean(~height,stat_stra)

svyquantile(~height, stat_stra, c(.25,.5,.9), ci=TRUE)

svyvar(~height,stat_stra)

##################plot##################
##empirical distribution####
cdf.est<-svycdf(~height, stat_stra)

###population one###
ht_pop=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/htpop.csv",sep=",",header=T)

cdf.pop<-ecdf(ht_pop[,1])
##################population vs empirical distribution####
plot(cdf.pop, main="Population vs estimate",xlab="Height")
lines(cdf.est[[1]],col.points="red")
legend("right",col=c("red","black"),pch=1, legend=c("Estimate","Population"),bty="n",lty=1)

###svyplot for scatter plot###

##############histogram and boxplot##################
svyhist(~height,stat_stra)
svyboxplot(height~gender,stat_stra)

############density plot and smoothing######
dens<-svysmooth(~height, stat_stra,bandwidth=2)
dens1<-svysmooth(~height, stat_stra)
 
svyhist(~height,design=stat_stra)
lines(dens,col="purple",lwd=3)
lines(dens1, col="forestgreen",lwd=2)

