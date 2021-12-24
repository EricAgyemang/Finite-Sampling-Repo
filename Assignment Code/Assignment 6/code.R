#AGYEMANG ERIC
#MAT 450 HOME WORK 6
#QUESTION 16

##QUESTION 16A 
#The percentage of parents who returned a consent form is given by column ybar_i in the table meas_agg below.

library(dplyr)
meas_agg <-as.data.frame(measles %>%
                           group_by(`School No`= school, Mi = Mitotal, ki = mi) %>%
                           summarize(Return = sum(returnf==1, na.rm = TRUE), 
                                     mi = sum(returnf!=9, na.rm = TRUE),
                                     ybar_i = Return/mi))
meas_agg

N<-46
n<-nrow(meas_agg)

meas_agg$si_sq<-c(.25676,.25635,.19118,.24828,.25846,.25906,.22727,.25,.23193,.25735)

meas_agg$si_sq

####################################################################################################

#QUESTION 16B
#The sampling weight for each observation is given by the "weight" column in the table meas_agg1 below.
meas_agg11<-as.data.frame(meas_agg %>%
                            group_by(`School No`, Mi, ki, Return, mi, ybar_i, si_sq) %>%
                            summarize(est_ti = Mi*Return/mi))


ybar_r<-sum(meas_agg11[,"est_ti"])/sum(meas_agg11[, "Mi"])
ybar_r

var_ybar_r<-(1/(Mbar^2))*(((1-n/N)*(sr_squared/n))+(sum(final)/(n*N)))
var_ybar_r

meas_agg1 <-as.data.frame(meas_agg11 %>%
                            group_by(`School No`, Mi, ki, Return, mi, ybar_i, est_ti, si_sq) %>%
                            summarize(squared_deviation = (est_ti-Mi*ybar_r)^2,
                                      final = (Mi^2)*(1-mi/Mi)*(si_sq/mi)))
attach(meas_agg1)

meas_agg1$weight<-(N/n)*(Mi/mi)

meas_agg1

##################################################################################################

#QUESTION 16C
#The overall percentage of parents who received a consent form along with a 95% CI is given below
sr_squared<-sum(meas_agg1[,"squared_deviation"])/(nrow(meas_agg1)-1)
ybar_r<-sum(meas_agg1[,"est_ti"])/sum(meas_agg1[, "Mi"])
attach(meas_agg1)
N<-46

Mbar<-sum(Mi)/n
var_ybar_r<-(1/(Mbar^2))*(((1-n/N)*(sr_squared/n))+(sum(final)/(n*N)))
var_ybar_r
###point estimate of percentage of parents
ybar_r


##confidence interval for ybar_r

ybar_r-1.96*sqrt(var_ybar_r); ybar_r+1.96*sqrt(var_ybar_r)
#(0.5061113,0.6517851), As the CI 


