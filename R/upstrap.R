#' Upstrap Resampling
#'
#' @param data The data as a matrix or data frame.
#' If it is a matrix or data frame then each row
#' is considered as one multivariate observation.
#' @param statistic A function which when applied to data returns a vector
#' containing the statistic(s) of interest.
#' @param R The number of upstrap replicates for each point on the grid of
#' range multipliers. Usually this will be a single positive integer.
#' @param n.grid.multiplier number of grid points for the
#' multiplier of the sample size.
#' @param range_multiplier minimum and maximum multiplier for the sample size
#' @param ... additional arguments to pass to \code{statistic}.
#'
#' @return An object
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
#' HTNDerv_s1 + age_s1*HTNDerv_s1, family="binomial", data=data)
#' # could use broom
#' #  tid = broom::tidy(fit)
#' #  pval = tid$p.value[ tid$term == "HTNDerv_s1"]
#' smod = coef(summary(fit))
#' pval = smod["HTNDerv_s1", "Pr(>|z|)"]
#' pval < 0.05
#' }
#' res = upstrap(data,
#' statistic = statistic,
#' R = 50,
#' n.grid.multiplier = 1,
#' range_multiplier = c(1, 2))
#' res = colMeans(res)
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
  range_multiplier = sort(range_multiplier)
  # minimum and maximum multiplier for the sample size
  # here they are set to 1 (same sample size) and 5 (5 times the original sample size)
  min.multiplier = range_multiplier[1]
  max.multiplier = range_multiplier[2]

  # set the grid of multipliers for the original sample size
  # here 1.2 stands for a sampel size that is 20% larger than the original sample size
  multiplier.grid = seq(
    from = min.multiplier,
    to = max.multiplier,
    length = max(n.grid.multiplier,
                 diff(range_multiplier) + 1)
  )

  # set the number of upstraps for each grid point;
  # this impacts running time and variability of the sample size calculation:
  # more data points, more running time, less variability
  # 10000 can take about 7-8 hours on a typical laptop
  # decrease this number for faster results
  n.upstrap = R

  # build the matrix that will contain whether or not the null hypothesis that the
  # HTN is zero for a given multiplier (r) and upstrap sample (u)
  J = length(multiplier.grid)
  check <- matrix(
    nrow = n.upstrap,
    ncol = J)

  colnames(check) = round(n.oss*multiplier.grid)
  # here j is the index of the multiplier of the original sample size, n.oss
  for (j in 1:J)
  {#Each loop corresponds to an increase in the sample size
    N =  round(n.oss*multiplier.grid[j])
    temp_index_mat <- matrix(sample.int(
      n.oss, size = N * n.upstrap,
      replace = TRUE), nrow = N, ncol = n.upstrap)

    # here u is the u-th upstrap for the r-th sample size multiplier
    # this simulation can/will be done more efficiently using matrices
    for (u in 1:n.upstrap)
    {# each loop corresponds to an upstrap with a given sample size

      # construct the upstrap sample index
      # from 1, ..., n.oss (original sample size)
      # size equal to original sample size times the multiplier
      # sampling is with replacement
      temp_index = temp_index_mat[, u]

      # extract the data (covariates and outcome) using the upstrap sample index
      temp_data <- data[temp_index,]


      # obtain the p-value for HTN in the upstrapped data
      check[u,j] <- statistic(temp_data, ...)
    }
  }
  return(check)
}
