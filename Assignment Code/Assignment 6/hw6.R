library(survey)

###load the data set####
meas=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/measles.csv",sep=",",header=T)


#####question a###########

meas$wt_a=meas$Mitotal/meas$mi

meas$returnf1=meas$returnf
meas[meas$returnf1==9,]$returnf1=0  ###assign 0 for non-response

prop_est=rep(NA,10)  ###create a vector to record answers from a)

for(i in 1:10){

 
meas_cl<- svydesign(id=~1, fpc=~Mitotal,weights =~wt_a, data=subset(meas,meas$school==i))

prop_est[i]=svymean(~returnf1, meas_cl)
                    
}
prop_est

###### question b##########
meas=read.csv("I:/OneDrive/OneDrive - IL State University/teaching/MAT 450-online/measles.csv",sep=",",header=T)

meas$mi1=meas$mi
 

for(i in 1:10){
 
meas[meas$school==i,14]=sum(meas[meas$school==i,3]!=9)    ###only record the response
}

 
######question c###########
N=46
n=10
 
meas$returnf1=meas$returnf
meas[meas$returnf1==9,]$returnf1=0  ###assign 0 for non-response

meas$wt_a=(meas$Mitotal/meas$mi1)*N/n

meas_cl<- svydesign(id=~school, weights =~wt_a, data=meas)

svymean(~returnf1, meas_cl)

######if the cluster is ignored####
meas_cl<- svydesign(id=~1, weights =~wt_a, data=meas)

svymean(~returnf1, meas_cl)


