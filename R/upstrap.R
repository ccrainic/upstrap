#' Upstrap Resampling
#'
#' @description This function samples with replacement either more or fewer
#' samples than the original sample size
#' @param data The data as a vector, matrix, or data frame.
#' If it is a matrix or data frame then each row
#' is considered as one multivariate observation.
#' @param statistic A function which when applied to data returns a value
#' containing the statistic of interest.
#' @param R The number of upstrap replicates for each value in
#' new_sample_size vector. This should be a single positive integer.
#' @param new_sample_size A vector of one or more new sample sizes,
#' to be drawn from the orignal data set. The new sample sizes can be less than,
#' equal to, or larger than the original data set.
#' @param ... additional arguments to pass to \code{statistic}.
#'
#' @return matrix of R replicates of the statistic applied to
#' the upstrap data. One column for each value in new_sample_size
#'
#' @keywords upstrap, bootstrap, resampling
#'
#' @export
#'
#' @examples
#' # read in the data
#' fname = system.file("extdata", "shhs1.txt",
#' package = "upstrap")
#' data = read.table(file = fname,
#'                  header = TRUE, na.strings="NA")
#' # define the moderate to severe sleep apnea variable
#' # from rdi4p - Respiratory Disturbance Index (RDI)
#' data$MtS_SA = data$rdi4p>=15
#'
#' #define statistic function
#' statistic = function(data) {
#' fit<-glm(MtS_SA ~ gender + age_s1 + bmi_s1 +
#'  HTNDerv_s1 + age_s1*HTNDerv_s1,
#'  family="binomial",
#'  data=data)
#'
#' smod = coef(summary(fit))
#' pval = smod["HTNDerv_s1", "Pr(>|z|)"]
#' pval < 0.05
#' }
#'
#' # do upstrap for the same sample size, 1.5 the original size
#' # and twice the original size
#' res = upstrap(data,
#'   statistic = statistic,
#'   R = 50,
#'   new_sample_size = c(nrow(data), nrow(data)\*1.5, nrow(data)\*2))
#'
#' # display results
#' power_res = colMeans(res)
#'
upstrap = function(data, statistic, R, new_sample_size, ...) {

  if (!is.function(statistic)) {
    stop("statistic needs to be a function")
  }

  # lapply(seq_len(R), function(x))
  # upstrap = statistic(data, ...)

  # sample size of the original data 
  # (number of rows for data frame or matrix, length for a vector)
  if (is.vector(data)) {
    n_oss = length(data)
  } else {
    n_oss = nrow(data)
  }

  # number of new sample sizes
  J = length(new_sample_size)

  # build the result matrix of size RxJ that will contain
  # the statistic value for each resampled dataset of given size(s)
  result = matrix(
    nrow = R,
    ncol = J)

  # for each sample size, generate R samples and compute statistics
  # here j is the index of the new sample size
  for (j in 1:J)
  {
    # get current sample size
    N =  round(new_sample_size[j])

    # construct matrix of re-sampled indices
    # each column corresponds to one re-sampling of indices
    # sampling is with replacement
    temp_index_mat = matrix(sample.int(
      n_oss, size = N*R,
      replace = TRUE), nrow = N, ncol = R)

    # for each upstrap re-sampling with a given sample size,
    # compute the statistic
    # here u is the u-th upstrap for the j-th new sample size
    for (u in 1:R)
    {

      # construct the upstrap sample index
      temp_index = temp_index_mat[, u]

      # extract the data using the upstrap sample index
      # using the upstrap sample index
      # (rows index for data frame or matrix, element for a vector)
      if (is.vector(data)) {
        temp_data <- data[temp_index]
      } else {
        temp_data <- data[temp_index, ]
      }
      
      # compute statistic for new data
      result[u,j] <- statistic(temp_data, ...)
    }
  }

  # return the matrix of statistics for resampled data,
  # for each size specified in new_sample_size
  return(result)
}

