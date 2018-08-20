#' Title
#'
#' @param data
#' @param statistic
#' @param R
#' @param n.grid.multiplier number of grid points for the multiplier of the sample size
#' @param range_multiplier minimum and maximum multiplier for the sample size
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' fname = system.file("extdata", "shhs1.txt",
#' package = "upstrap")
#' # read in the data
#' data = read.table(file = fname,
#'                  header = TRUE, na.strings="NA")
#' data$MtS_SA = data$rdi4p>=15
#' statistic = function(data) {
#' fit<-glm(MtS_SA ~ gender + age_s1 + bmi_s1 +
#' HTNDerv_s1 + age_s1*HTNDerv_s1, family="binomial", data=temp_data)
#' coef(summary(fit))[,4][5]<0.05
#' }

}
#' data =
#' statistic =

# fit the same model on the upstrapped data

upstrap = function(data, statistic, R = 10000, n.grid.multiplier = 21,
                   range_multiplier = c(1, 5), ...) {

  if (!is.function(statistic)) {
    stop("statistic needs to be a function")
  }

  # lapply(seq_len(R), function(x))
  # upstrap = statistic(data, ...)

  # set the seed for reproducibility
  # set.seed(08132018)

  # sample size of the original data
  n.oss = nrow(data)

  if (length(range_multiplier) == 1) {
    range_multiplier = rep(range_multiplier, 2)
  }

  if (length(range_multiplier) != 2) {
    warning("Range multiplier can only have 2 values, limiting to first 2")
    range_multiplier = range_multiplier[1:2]
  }

  # minimum and maximum multiplier for the sample size
  # here they are set to 1 (same sample size) and 5 (5 times the original sample size)
  min.multiplier = range_multiplier[1]
  max.multiplier = range_multiplier[2]

  # set the grid of multipliers for the original sample size
  # here 1.2 stands for a sampel size that is 20% larger than the original sample size
  multiplier.grid = seq(from = min.multiplier,
                      to = max.multiplier,
                      length=n.grid.multiplier)

  # set the number of upstraps for each grid point;
  # this impacts running time and variability of the sample size calculation:
  # more data points, more running time, less variability
  # 10000 can take about 7-8 hours on a typical laptop
  # decrease this number for faster results
  n.upstrap = R

  # build the matrix that will contain whether or not the null hypothesis that the
  # HTN is zero for a given multiplier (r) and upstrap sample (u)
  check<-matrix(rep(NA,n.upstrap*n.grid.multiplier),
                ncol=n.grid.multiplier)

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
      temp_index<-sample(1:n.oss,
                         round(n.oss*multiplier.grid[j]),
                         replace=TRUE)

      # extract the data (covariates and outcome) using the upstrap sample index
      temp_data<-dat[temp_index,]


      # obtain the p-value for HTN in the upstrapped data
      check[u,j]<-coef(summary(fit))[,4][5]<0.05
    }
  }
}
