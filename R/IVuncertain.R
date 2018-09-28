##' Calculate the uncertainty of this data driven IV feature extraction
##'
##' @title Uncertainty for IV feature extraction
##' @param dat A dataframe of IV curve. The variable names should be "V" for voltage, and "I" for current. And rank with increasing voltage.
##' @param plot.option True/False, it plots the IV curve. The default is false.
##' @param crt a value to set for how large of regression coefficient change rate we use as not changing much. Should be the same as using function "IVExtractResult". Default is 0.6.
##' @param num a value of number of data points. The default is 25.
##' @param crtvalb a value to set the change of I(current) we want to use as changing very much (to detect the end of IV curve). Suggestion is to test this function with several IV curves for your data and find the proper value. The default is 0.3
##' @param iter number of iterations want to use to calculate uncertainty. Default is 1000, takes about 20 minutes to finish calculation.
##'
##' @importFrom stats lm
##' @importFrom stats sd
##'
##'
##' @return a list of the following items:
##' \itemize{
##'  \item "unct_Isc": uncertainty for short-circuit current, the number of variables is decided by the number of steps.
##'  \item "unct_Rsh": uncertainty for shunt resistance, the number of variables is decided by the number of steps.
##'  \item "unct_Voc": uncertainty for open-circuit voltage, the number of variables is decided by the number of steps.
##'  \item "unct_Rs": uncertainty for series resistance, the number of variables is decided by the number of steps.
##'  \item "unct_Pmp": uncertainty for maximum power for a solar cell/PV module, the number of variables is decided by the number of steps.
##'  \item "unct_Imp": uncertainty for current at maximum power, the number of variables is decided by the number of steps.
##'  \item "unct_Vmp": uncertainty for voltage at maximum power, the number of variables is decided by the number of steps.
##'  \item "unct_FF": uncertainty for fill factor, the number of variables is decided by the number of steps.
##'  \item "unct_Cutoff": uncertainty for change point indicating steps. NA means that the IV curve has only one step and there is no change points.
##' }
##'
##' @export
##'
##' @examples
##' #this IV curve is of step=1
##' data(IV_step1)
##' IV1 <- data.frame(IV_step1)
##' \donttest{IVuncertain(IV1)}
##'
IVuncertain <- function(dat,crt=0.2,num=75,crtvalb=0.3,iter=1000,plot.option=F){
  V <- dat$V ; I <- dat$I
  ## remove the NA value
  V <- na.omit(V) ; I <- na.omit(I)

  xspl <- ((1:1000) / 1000) * max(V)
  newDat <- predict(smooth.spline(V, I), xspl)
  x <- unlist(newDat[1])    ## Voltage
  y <- unlist(newDat[2])    ## Current

  rows <- length(x)
  dat <- data.frame(V,I)
  #Res <- data.frame()
  Extract <- IVExtractResult(dat,crt,num,crtvalb,plot.option)
  step <- Extract$step
  m <- 0
  Isc <- data.frame()
  Voc <- data.frame()
  Pmp <- data.frame()
  Rs <- data.frame()
  Rsh <- data.frame()
  FF <- data.frame()
  Imp <- data.frame()
  Vmp <- data.frame()
  Cutoff <- data.frame()

  for (i in 1:iter){
    indexes <- sample(rows-2,floor((rows-2)*0.9),replace=FALSE)
    samp_V <- c(x[1],x[indexes+1],x[rows])
    samp_I <- c(y[1],y[indexes+1],y[rows])
    samp_dat <- data.frame(samp_V,samp_I)
    names(samp_dat) <- c("V","I")
    trial <- try(IVExtractResult(samp_dat,crt,num,crtvalb,plot.option),silent=TRUE)
    if ('try-error' %in% class(trial)){
      m <- m
    }else{
      IVf <- IVExtractResult(samp_dat,crt,num,crtvalb,plot.option)
      if ((IVf$step %in% step) && (step > 1)){
        m <- m+1
        Temp_Isc <- as.numeric(as.character(char_to_tab(IVf$Isc)))
        Temp_Voc <- as.numeric(as.character(char_to_tab(IVf$Voc)))
        Temp_Pmp <- as.numeric(as.character(char_to_tab(IVf$Pmp)))
        Temp_Rs <- as.numeric(as.character(char_to_tab(IVf$Rs)))
        Temp_Rsh <- as.numeric(as.character(char_to_tab(IVf$Rsh)))
        Temp_FF <- as.numeric(as.character(char_to_tab(IVf$FF)))
        Temp_Imp <- as.numeric(as.character(char_to_tab(IVf$Imp)))
        Temp_Vmp <- as.numeric(as.character(char_to_tab(IVf$Vmp)))
        Temp_Cut <- as.numeric(as.character(char_to_tab(IVf$Cutoff)))
        for (j in 1:step){
          Isc[m,j] <- Temp_Isc[j]
          Voc[m,j] <- Temp_Voc[j]
          Pmp[m,j] <- Temp_Pmp[j]
          Rs[m,j] <- Temp_Rs[j]
          Rsh[m,j] <- Temp_Rsh[j]
          FF[m,j] <- Temp_FF[j]
          Imp[m,j] <- Temp_Imp[j]
          Vmp[m,j] <- Temp_Vmp[j]
        }
        for (j in 1:(step-1)){
          Cutoff[m,j] <- Temp_Cut[j]
        }
      }
      if ((IVf$step %in% step) && (step %in% 1)){
        m <- m+1
        Isc[m,1] <- IVf$Isc
        Voc[m,1] <- IVf$Voc
        Pmp[m,1] <- IVf$Pmp
        Rs[m,1] <- IVf$Rs
        Rsh[m,1] <- IVf$Rsh
        FF[m,1] <- IVf$FF
        Imp[m,1] <- IVf$Imp
        Vmp[m,1] <- IVf$Vmp
      }
    }
  }
  unct_Isc <- lapply(Isc,sd)
  unct_Voc <- lapply(Voc,sd)
  unct_Pmp <- lapply(Pmp,sd)
  unct_Rs <- lapply(Rs,sd)
  unct_Rsh <- lapply(Rsh,sd)
  unct_FF <- lapply(FF,sd)
  unct_Imp <- lapply(Imp,sd)
  unct_Vmp <- lapply(Vmp,sd)
  if (step>1){
    unct_Cutoff <- lapply(Cutoff,sd)
  }else{
    unct_Cutoff <- NA
  }
  return(list(unct_Isc=unct_Isc,unct_Voc=unct_Voc,unct_Pmp=unct_Pmp,unct_Rs=unct_Rs,unct_Rsh=unct_Rsh,unct_FF=unct_FF,unct_Imp=unct_Imp,unct_Vmp=unct_Vmp,unct_Cutoff=unct_Cutoff))
}
