##' This function carries out IV feature extraction, by calculating steps then extract features for each step.
##'
##' @title IV feature extraction
##'
##' @param dat A dataframe of IV curve. The variable names should be "V" for voltage, and "I" for current. And rank with increasing voltage.
##' @param plot.option True/False, it plots the IV curve. The default is false.
##' @param crt A value to set for how large of regression coefficient change rate we use as not changing much. This is due to the value of IV curve, suggestion is to test this function with several IV curves for your data and find the proper value. The default is 0.2.
##' @param num A value of number of data points. The default is 75.
##' @param crtvalb A value to set the change of I(current) we want to use as changing very much (to detect the end of IV curve). Suggestion is to test this function with several IV curves for your data and find the proper value. The default is 0.3
##' @param k The number of equally-spaced values to supply as starting values for the breakpoints. The default is 7.
##'
##'
##'
##' @importFrom stats predict
##' @importFrom stats smooth.spline
##' @importFrom stats na.omit
##'
##' @return A list of the following items:
##' \itemize{
##'  \item "step": a value that shows the number of steps in the IV curve
##'  \item "Isc": short-circuit current, which is the current through the solar cell when the voltage across the solar cell is zero.
##'  \item "Rsh": shunt resistance, which is the inverse slope of the IV curve near Isc.
##'  \item "Voc": open-circuit voltage, which is the maximum voltage from a solar cell and occurs at zero current.
##'  \item "Rs": series resistance, which is the inverse slope of the IV curve near Voc.
##'  \item "Pmp": maximum power for a solar cell/PV module.
##'  \item "Imp": current at maximum power.
##'  \item "Vmp": voltage at maximum power.
##'  \item "FF": fill factor, which is the ratio of maximum power from a solar cell to the product of Voc and Isc.
##'  \item "Cutoff": a string of values (voltage) that shows the change point indicating steps. NA means that the IV curve has only one step and there is no change points.
##' }
##'
##' @export
##'
##' @examples
##' #this IV curve is of step=1
##' #load the data provided in the package
##' data(IV_step1)
##' IV1 <- data.frame(IV_step1)
##' result <- IVExtractResult(IV1,plot.option=FALSE)
##' #use the IV curve with step=2
##' data(IV_step2)
##' IV2 <- data.frame(IV_step2)
##' #with plot.option=TRUE, IV curve and steps are ploted
##' result2 <- IVExtractResult(IV2,plot.option=FALSE)
##' #use the IV curve with step=3
##' data(IV_step3)
##' IV3 <- data.frame(IV_step3)
##' IVExtractResult(IV3,plot.option=FALSE)
##'


IVExtractResult <- function(dat,k=7,crt=0.2,num=75,crtvalb=0.3,plot.option=F){

  dat <- subset(dat, V > 0)
  V <- dat$V ; I <- dat$I
  ## remove the NA value
  V <- na.omit(V) ; I <- na.omit(I)

  xspl <- ((1:500) / 500) * max(V)
  newDat <- predict(smooth.spline(V, I), xspl)
  x <- unlist(newDat[1])    ## Voltage
  y <- unlist(newDat[2])    ## Current

  #Use the IVsteps function to calculate how many steps are in our curve
  seg <- IVsteps(I,V,k,plot.option)
  step <- seg$step
  xsep <- seg$xsep

  #Use the IVfeature function to calculate the five parameters in our curve
  if (step %in% 1) {
    V_all <- x
    I_all <- y
    IVf <- IVfeature(I_all, V_all, crt=crt,num=num,crtvalb=crtvalb)
    isc <- IVf$Isc
    voc <- IVf$Voc
    R_sh <- IVf$Rsh
    R_s <- IVf$Rs
    ff <- IVf$FF
    pmp <- IVf$Pmp
    Imp <- IVf$Imp
    Vmp <- IVf$Vmp
  }else{
    isc <- data.frame()
    voc <- data.frame()
    R_sh <- data.frame()
    R_s <- data.frame()
    ff <- data.frame()
    pmp <- data.frame()
    Imp <- data.frame()
    Vmp <- data.frame()

    for (j in 1:step) {
      if (j %in% 1) {
        I_cut <- y[which(x < xsep[1,1])]
        V_cut <- x[which(x < xsep[1,1])]
        IVf <- IVfeature(I_cut,V_cut,crt=crt,num=num,crtvalb=crtvalb)
        isc[j,1] <- IVf$Isc
        voc[j,1] <- IVf$Voc
        R_sh[j,1] <- IVf$Rsh
        R_s[j,1] <- IVf$Rs
        ff[j,1] <- IVf$FF
        pmp[j,1] <- IVf$Pmp
        Imp[j,1] <- IVf$Imp
        Vmp[j,1] <- IVf$Vmp
      }else{
        if (j > 1 && j < step) {
          I_cut <- y[which(x > xsep[j-1,1] & x < xsep[j,1])]
          V_cut <- x[which(x > xsep[j-1,1] & x < xsep[j,1])]
          IVf <- IVfeature(I_cut,V_cut, crt=crt,num=num,crtvalb=crtvalb)
          isc[j,1] <- IVf$Isc
          voc[j,1] <- IVf$Voc
          R_sh[j,1] <- IVf$Rsh
          R_s[j,1] <- IVf$Rs
          ff[j,1] <- IVf$FF
          pmp[j,1] <- IVf$Pmp
          Imp[j,1] <- IVf$Imp
          Vmp[j,1] <- IVf$Vmp
        }else{
          I_cut <- y[which(x > xsep[j-1,1])]
          V_cut <- x[which(x > xsep[j-1,1])]
          IVf <- IVfeature(I_cut,V_cut, crt=crt,num=num,crtvalb=crtvalb)
          isc[j,1] <- IVf$Isc
          voc[j,1] <- IVf$Voc
          R_sh[j,1] <- IVf$Rsh
          R_s[j,1] <- IVf$Rs
          ff[j,1] <- IVf$FF
          pmp[j,1] <- IVf$Pmp
          Imp[j,1] <- IVf$Imp
          Vmp[j,1] <- IVf$Vmp
        }

      }
    }
    isc <- tab_to_char(isc)
    voc <- tab_to_char(voc)
    R_sh <- tab_to_char(R_sh)
    R_s <- tab_to_char(R_s)
    ff <- tab_to_char(ff)
    pmp <- tab_to_char(pmp)
    xsep <- tab_to_char(xsep)
    Imp <- tab_to_char(Imp)
    Vmp <- tab_to_char(Vmp)
  }

res <- data.frame(step=step,Isc=isc,Rsh=R_sh,Voc=voc,Rs=R_s,Pmp=pmp,Imp=Imp,Vmp=Vmp,FF=ff,Cutoff=xsep)
return(res)

}
