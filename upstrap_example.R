# upstrap example: sample size calculation for ...

# read in the data
dat = read.table(file = "data/shhs1.txt",
                 header = TRUE, na.strings="NA")

# show the head of the list `dat`
head(dat)

# show the dimension of the list
dim(dat)

# to explore the data, do a scatterplot of the Body Mass Index (BMI) versus Respiratory Disturbance Index (RDI), 
# variables labeled `bmi_s1` and `rdi4p`, respectively.  
# The figure below provides the scatter plot of BMI versus RDI, though, for presentation purposes,
# we only show RDI less than 50.
plot(dat$bmi_s1,dat$rdi4p,pch=".",col=rgb(0,0,1,.2),cex=3,ylim=c(0,50),bty="l",
     cex.axis=1.2,col.axis="blue",main=NULL,xlab="Body Mass Index",
     ylab="Respiratory Disturbance Index (4%)")
points(mean(dat$bmi_s1,na.rm=TRUE),mean(dat$rdi4p,na.rm=TRUE),cex=2,pch=19,col=rgb(1,0,0,0.5))

# calculate the mean BMI and RDI 
round(mean(dat$bmi_s1,na.rm=TRUE),digits=2)
round(mean(dat$rdi4p,na.rm=TRUE),digits=2)

## Fit a basic regression model
# define the moderate to severe sleep apnea variable from rdi4p
MtS_SA=dat$rdi4p>=15

# identify how many individuals in SHHS (visit 1) have moderate
# to severe sleep apnea
n_positive =sum(MtS_SA)
n_positive

# conduct a binary GLM regression with outcome moderate to severe
# sleep apnea (Yes=1, No=0) on a few covariates (main effects) 
# and an interaction (age by HTN)
fit<-glm(MtS_SA~gender+age_s1+bmi_s1+HTNDerv_s1+age_s1*HTNDerv_s1,
         family="binomial",data=dat)
summary(fit)

# answer the question:
# For a given value of beta (say, 0.1 or 0.2), what is the sample size that 
# will ensure that the null hypothesis of zero main effect of HTN will be rejected
# with a frequency at least equal to 100(1-beta)% (power)?

## The upstrap for sample size calculation

# set the seed for reproducibility
set.seed(08132018)

# sample size of the original data
n.oss=dim(dat)[1]

# number of grid points for the multiplier of the sample size
n.grid.multiplier=21

# minimum and maximum multiplier for the sample size
# here they are set to 1 (same sample size) and 5 (5 times the original sample size)
min.multiplier=1
max.multiplier=5

# set the grid of multipliers for the original sample size
# here 1.2 stands for a sampel size that is 20% larger than the original sample size
multiplier.grid=seq(from=min.multiplier, to=max.multiplier,length=n.grid.multiplier)

# set the number of upstraps for each grid point; 
# this impacts running time and variability of the sample size calculation: 
# more data points, more running time, less variability
# 10000 can take about 7-8 hours on a typical laptop
# decrease this number for faster results
n.upstrap=10000 

# build the matrix that will contain whether or not the null hypothesis that the
# HTN is zero for a given multiplier (r) and upstrap sample (u)
check<-matrix(rep(NA,n.upstrap*n.grid.multiplier),ncol=n.grid.multiplier)

# here j is the index of the multiplier of the original sample size, n.oss
for (j in 1:n.grid.multiplier)
{#Each loop corresponds to an increase in the sample size
  
  # here u is the u-th upstrap for the r-th sample size multiplier
  # this simulation can/will be done more efficiently using matrices
  for (u in 1:n.upstrap)
  {# each loop corresponds to an upstrap with a given sample size
    
    # construct the upstrap sample index 
    # from 1, ..., n.oss (original sample size)
    # size equal to original sample size times the multiplier
    # sampling is with replacement
    temp_index<-sample(1:n.oss,n.oss*multiplier.grid[j],replace=TRUE)
    
    # extract the data (covariates and outcome) using the upstrap sample index
    temp_data<-dat[temp_index,]
    MtS_SA=temp_data$rdi4p>=15
    
    # fit the same model on the upstrapped data
    fit<-glm(MtS_SA~gender+age_s1+bmi_s1+HTNDerv_s1+age_s1*HTNDerv_s1,
             family="binomial",data=temp_data)
    
    # obtain the p-value for HTN in the upstrapped data
    check[u,j]<-coef(summary(fit))[,4][5]<0.05
  }
}

# power check calculates the proportion of times the null hypothesis is rejected
power_check<-colMeans(check)

# plot the vector of multipliers of the original sample size, versus the power curve for 
# rejecting the null hypothesis that the parameter of HTN is equal to zero
plot(multiplier.grid,power_check,type="l",col="blue",lwd=3,
     xlab="Factor by which the sample size is multiplied",
     ylab="Power to detect the HTN effect",
     bty="l",cex.axis=1.5,cex.lab=1.4,col.lab="blue",
     col.axis="blue",main=NULL)
# add horizontal lines to indicate power equal to 0.8 (orange) and 0.9 (red).
abline(h=0.8,lwd=3,col="orange")
abline(h=0.9,lwd=3,col="red")

