#' Upstrap example for sample size calculation
#' Using a subset of the Sleep Heart Health Study data provided in this package, 
#' for a given value of beta (say, 0.1 or 0.2), what is the sample size that
#' will ensure that the null hypothesis of zero main effect of HTN will be rejected
#' with a frequency at least equal to 100(1-beta)% (power)?
#' 
#' @return none
#' @export
#'
#' @examples
#' upstrapExampleSampleSizeCalculation()
upstrapExampleSampleSizeCalculation = function() {
  
  fname = system.file("extdata", "shhs1.txt",
                      package = "upstrap")
  # read in the data
  dat = read.table(file = fname,
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
  
  # add the MtS_SA variable to the data frame
  dat$MtS_SA=MtS_SA
  
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
  n_oss=dim(dat)[1]
  
  # number of grid points for the multiplier of the sample size
  n_grid_multiplier=21
  
  # minimum and maximum multiplier for the sample size
  # here they are set to 1 (same sample size) and 5 (5 times the original sample size)
  min_multiplier=1
  max_multiplier=5
  
  # set the grid of multipliers for the original sample size
  # here 1.2 stands for a sampel size that is 20% larger than the original sample size
  multiplier_grid=seq(from=min_multiplier, to=max_multiplier,length=n_grid_multiplier)
  
  # vector of the new sample sizes
  new_sample_sizes = n_oss * multiplier_grid
  
  # set the number of upstraps for each grid point;
  # this impacts running time and variability of the sample size calculation:
  # more data points, more running time, less variability
  # 10000 can take about 7-8 hours on a typical laptop
  # decrease this number for faster results
  n_upstrap=100
  
  # define statistic function to compute the estimate we are interested in 
  statistic = function(data) {
    fit <- glm(MtS_SA~gender+age_s1+bmi_s1+HTNDerv_s1+age_s1*HTNDerv_s1, 
               family="binomial", 
               data=data)
    # could use broom
    #  tid = broom::tidy(fit)
    #  pval = tid$p.value[ tid$term == "HTNDerv_s1"]
    smod <- coef(summary(fit))
    pval <- smod["HTNDerv_s1", "Pr(>|z|)"]
    pval < 0.05
  }
  
  # invoke upstap for each new sample size
  check = upstrap(dat, statistic = statistic, 
                  R = n_upstrap, 
                  new_sample_size = new_sample_sizes)
  
  
  # power check calculates the proportion of times the null hypothesis is rejected
  power_check = colMeans(check)
  
  # plot the vector of multipliers of the original sample size, versus the power curve for
  # rejecting the null hypothesis that the parameter of HTN is equal to zero
  plot(multiplier_grid,power_check,type="l",col="blue",lwd=3,
       xlab="Factor by which the sample size is multiplied",
       ylab="Power to detect the HTN effect",
       bty="l",cex.axis=1.5,cex.lab=1.4,col.lab="blue",
       col.axis="blue",main=NULL)
  # add horizontal lines to indicate power equal to 0.8 (orange) and 0.9 (red).
  abline(h=0.8,lwd=3,col="orange")
  abline(h=0.9,lwd=3,col="red")
  
}