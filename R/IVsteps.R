##' Calculate steps of IV curve
##'
##' @title Calculate steps of IV curve
##' @import segmented
##'
##' @param I A vector of current values from IV data frame
##' @param V A vector of voltage values from IV data frame
##' @param plot.option True/False, it plots the IV curve. The default is false.
##' @param k The number of equally-spaced values to supply as starting values for the breakpoints. The default is 7.
##' @param diff_slp The difference between the slope on the left and on the right of the change point. The default is 0.01.
##'
##' @importFrom graphics plot
##' @importFrom graphics abline
##'
##'
##' @return a list of the following items:
##' \itemize{
##'  \item "step": a value that shows how many steps of IV curve
##'  \item "xsep": a vector of values (voltage) that shows the change point indicating steps. NA means that the IV curve has only one step and there is no change points.
##' }
##'
##' @export
##'
##' @examples
##' #this IV curve is of step=1
##' #load the data provided in the package
##' data(IV_step1)
##' IV1 <- data.frame(IV_step1)
##' result <- IVsteps(IV1$I,IV1$V)
##' #use the IV curve with step=2
##' data(IV_step2)
##' IV2 <- data.frame(IV_step2)
##' #with plot.option=TRUE, IV curve and steps are ploted
##' result2 <- IVsteps(IV2$I,IV2$V,plot.option=TRUE)
##'



IVsteps <- function(I, V, k = 7, diff_slp = 0.01, plot.option = FALSE){


  # make the spline of x (V) and y (I) (100 points)
  xspl <- ((1:100) / 100) * max(V)
  newDat <- predict(smooth.spline(V, I), xspl)
  x <- unlist(newDat[1])  ## voltage
  y <- unlist(newDat[2])  ## current

  #x <- V
  #y <- I


  # Use the segment regression function to find the breakpoints (as many as the function can find)
  trial <- try(segmented.lm(lm(y~x), seg.Z = ~x, psi = list(x = NA),
                     control = seg.control(K = k, fix.npsi = FALSE, n.boot = 0, it.max = 20)))

  if( "segmented" %in% class(trial)){
    f1 <- segmented.lm(lm(y~x), seg.Z = ~x, psi = list(x = NA),
                     control = seg.control(K = k, fix.npsi = FALSE, n.boot = 0, it.max = 20))
    if(plot.option){
      # plot the curve along with the breakpoints the function found
      plot(x, y, type = "l", xlab = "Voltage (V)", ylab = "Current (I)", main = "Final Change Points")
      points.segmented(f1)
    }
    # Find the slope for each cut range
    b <- slope(f1)

    # Calculate the steps according to the criteria that breakpoint is significant
    # And the absolute value of slope decrease
    # And the previous slope is negative
    step <- 1
    m <- 1
    xsep <- data.frame()
    for (i in 1:(nrow(b$x) - 1)) {
      if((abs(b$x[i,1]) > abs(b$x[i + 1,1])) & (b$x[i,1] < 0) & (abs(b$x[i,1]) - abs(b$x[i + 1,1])) > diff_slp) {
        step <- step + 1
        xsep[m,1] <- round(f1$psi[i,2], 3)
        m <- m + 1
      }
    }

    if (m %in% 1){ xsep <- NA }

  }else{
    step <- 1
    xsep <- NA
  }

  if(plot.option){
    if(length(V) > 100){
      # Plot the curve and the cutoff points to see if reasonable
      if (xsep %in% NA){
        plot(V, I, xlab = 'voltage', ylab = 'current', main = 'I-V curve')
      }else{
        plot(V, I, xlab = 'voltage', ylab = 'current', main = 'I-V curve')
        for (i in 1:nrow(xsep)) {
          abline(v = xsep[i,1], col = 'red')
        }
      }
    }else{
      # Plot the curve and the cutoff points to see if reasonable
      if (xsep %in% NA){
      plot(x, y, xlab = 'voltage', ylab = 'current', main = 'I-V curve')
    }else{
        plot(x, y, xlab = 'voltage', ylab = 'current', main = 'I-V curve')
        for (i in 1:nrow(xsep)) {
        abline(v = xsep[i,1], col = 'red')
      }
      }
    }

  }

  return(list(step = step, xsep = xsep))
}









