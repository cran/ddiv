##' Calculate IV feature from the given IV curve with one step
##'
##' Note that IV curve should have voltage ordered from the smalles value to the largest value.
##'
##' This function can gives result for IV curves with more than one steps, but the results are not accurate.
##' To extract IV features for IV curves with more than one steps, or unsure about the steps of IV curves, please use IVExtractResult().
##' For IV curve has only one step, this function can be used.
##'
##' @title Calculate IV feature from a IV curve
##'
##' @param I A vector of current values from IV data frame
##' @param V A vector of voltage values from IV data frame
##' @param crt A value to set for how large of regression coefficient change rate we use as not changing much. This is due to the value of IV curve, suggestion is to test this function with several IV curves for your data and find the proper value. The default is 0.2.
##' @param num A value of number of data points. The default is 75.
##' @param crtvalb A value to set the change of I(current) we want to use as changing very much (to detect the end of IV curve). Suggestion is to test this function with several IV curves for your data and find the proper value. The default is 0.3
##'
##'
##' @importFrom stats lm
##'
##'
##' @return a list of the following items:
##' \itemize{
##'  \item "Isc": short-circuit current, which is the current through the solar cell when the voltage across the solar cell is zero.
##'  \item "Rsh": shunt resistance, which is the inverse slope of the IV curve near Isc.
##'  \item "Voc": open-circuit voltage, which is the maximum voltage from a solar cell and occurs at zero current.
##'  \item "Rs": series resistance, which is the inverse slope of the IV curve near Voc.
##'  \item "Pmp": maximum power for a solar cell/PV module.
##'  \item "Imp": current at maximum power.
##'  \item "Vmp": voltage at maximum power.
##'  \item "FF": fill factor, which is the ratio of maximum power from a solar cell to the product of Voc and Isc.
##' }
##'
##' @export
##'
##' @examples
##' #this IV curve is of step=1
##' #if IV curve is of step>1, please use IVsteps to first find the number of
##' #steps and change points, then apply this function for each steps
##' #load the data provided in the package
##' data(IV_step1)
##' IV1 <- data.frame(IV_step1)
##' result <- IVfeature(IV1$I,IV1$V,crt=0.2,num=75,crtvalb=0.3)
##'



IVfeature = function(I,V,crt=0.2,num=75,crtvalb=0.3){

  dat <- data.frame(V=V,I=I)
  dat <- subset(dat, V > 0)
  V <- dat$V ; I <- dat$I
  ## remove the NA value
  V <- na.omit(V) ; I <- na.omit(I)

  xspl <- ((1:500) / 500) * max(V)
  newDat <- predict(smooth.spline(V, I), xspl)
  x <- unlist(newDat[1])    ## Voltage
  y <- unlist(newDat[2])    ## Current

  V <- x
  I <- y

  rfit=data.frame()
  # Run regression of I~V for every 5 points
  for (j in 1:(length(I)-4)){
    tempFit3=lm(I[j:(j+4)] ~ V[j:(j+4)])
    rfit[j,1]=tempFit3$coefficient[2]
  }

  slprate=data.frame()

    # Calculate the change rate of slope (regression result for every 5 points)
  for (j in 1:(length(rfit[,1]))-1){
    slprate[j,1]=(rfit[j+1,1]-rfit[j,1])/rfit[j,1]
  }

  diffslprate=slprate[,1]

  record2=data.frame()
  # filter the points for steady state at front end
  m=1
  for (j in 1:(length(diffslprate)-1)){
    if (abs(diffslprate[j]) < crt & abs(diffslprate[j+1])<crt){
      record2[m,1]=j
      m=m+1
    }else{
      m=m
    }
  }

  record3=record2
  record3=record3[record3<num]

  # take the data for steady state at front end

  Iselect=I[(record3[1]):(record3[length(record3)])]
  Vselect=V[(record3[1]):(record3[length(record3)])]

  # count how many points are used to do the regression
  count=length(Iselect)

  # run regression for front end
  fit.fd=lm(Iselect~Vselect)

  # make modification according to the regression
  if (record3[1]>1){
    for (j in 1:record3[1]){
      I[j]=fit.fd$coefficients[1]+fit.fd$coefficients[2]*V[j]
    }

  }

  fit.rsh=lm(I[1:record3[length(record3)]]~V[1:record3[length(record3)]])

  Isc <- as.numeric(fit.rsh$coefficients[1])
  Isc <- round(Isc,3)
  R_sh <- as.numeric(-(1/fit.rsh$coefficients[2]))
  R_sh <- round(R_sh,3)

  diffV=data.frame()
  diffI=data.frame()
  # calculate the change rate of current
  for (j in 1:(length(I)-1)){
    diffV[j,1]=V[j+1]-V[j]
    diffI[j,1]=(I[j+1]-I[j])/I[j]
  }


  diffItp=diffI[,1]

  # filter the points at backend
  k=1
  record4=data.frame()
  for (j in 1:(length(diffItp))){
    if (abs(diffItp[j]) > crtvalb){
      record4[k,1]=j
       k=k+1
    }else{
      k=k
    }
  }

  # take the data for back end
  if(is.null(record4[1,1])){
    Iselect2=I[(length(I)-3):length(I)]
    Vselect2=V[(length(I)-3):length(V)]
  }else{
    record=record4[,1]
    # take the data for back end
    Iselect2=I[record[1]:length(I)]
    Vselect2=V[record[1]:length(V)]
   }


  # run regression for back end data set
  fit.bd=lm(Vselect2~Iselect2)

  # make prediction according to the regression
  voc <- as.numeric(fit.bd$coefficients[1])
  voc <- round(voc,3)
  rs <- as.numeric(-fit.bd$coefficients[2])
  rs <- round(rs,3)

  # calculate Pmp and FF
  pmp <- round(max(I*V),3)
  ff <- round(pmp/(Isc*voc)*100,2)

  # find Imp and Vmp
  p <- I*V
  Imp <- round(I[which(p==max(p))],3)
  Vmp <- round(V[which(p==max(p))],3)

  return(list(Isc=Isc,Rsh=R_sh,Voc=voc,Rs=rs,Pmp=pmp,Imp=Imp,Vmp=Vmp,FF=ff))

}
