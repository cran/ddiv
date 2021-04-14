#' Outdoor Time Series I-V Curve Data from SDLE SunFarm.
#'
#' This dataset comes from a long term time-series I-V curves dataset of full sized modules traced by ESL-Solar 500. Data acquired include outdoor time-series I-V curve data taken every 5 minutes from 2013-12-29 09:00:00 to 2013-12-29 13:55:00.
#' This data set is an example of how to use I-V data for a timeseries. The first column is timestamp(tmst) and the second column is I-V curve data(ivdf) that can be separated into voltage(V) and current(I).
#'
#' @docType data
#' @usage data(IV_timeseries)
#' @name IV_timeseries
#' @author Jiqi Liu, Roger H. French, Megan M. Morbitzer
#'
#' @format A data frame with 60 rows and 2 variables:
#' \describe{
#'   \item{tmst}{date and time of measurement}
#'   \item{ivdf}{I-V curve data points in a string format separated by "#" and voltage and current value separated by "*"}
#' }
#' @source Solar Durability and Lifetime Extension (SDLE) Research Center, Case Western
#' Reserve University
"IV_timeseries"
