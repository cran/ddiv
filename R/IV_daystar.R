#' Outdoor Time Series I-V Curve Data from SDLE SunFarm.
#'
#' This dataset comes from SDLE SunFarm, which provides extensive outdoor exposure capabilities, including mini-modules and full size modules mounted on both dual axis trackers fixed racks. Modules are individually controlled and operated by Daystar multi-tracer system at their peak power during daylight hours. Data acquired include time-series I-V curve data taken every 10 minutes, and time series power, voltage, current and weather data collected every minute.
#' This data set is a study of IV curve with 1 step. The first column is voltage(V) and the second column is current(I).
#'
#' @docType data
#' @usage data(IV_daystar)
#' @name IV_daystar
#' @author Menghong Wang, Jiqi Liu, Jennifer L. Braid, Roger H. French
#'
#' @format A data frame with 48 rows and 2 variables:
#' \describe{
#'   \item{V}{increasing vector}
#'   \item{I}{decreasing vector}
#' }
#' @source Solar Durability and Lifetime Extension (SDLE) Research Center, Case Western
#' Reserve University
"IV_daystar"
