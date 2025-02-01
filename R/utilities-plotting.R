# Plotting utilities

#' Plot changepoints over an existing uncertainties plot
#'
#' @description
#'
#' @param cpt.plot changepoint result to plot; output from changepoint::cpt.meanvar
#' @param time time increments from previous PrC analysis
#' @param timescale timescale to use. One of either "BP" or "BCAD".
#'
#' @noRd
#'
plot.cpts<- function(cpt.plot, time, timescale = "BCAD"){
  if(timescale == "BCAD"){
    timeMin <- max(time_i)
    timeMax <- min(time_i)
  }
  else if(timescale == "BP"){
    timeMin <- min(time_i)
    timeMax <- max(time_i)
  }
  for(n in 1:(length(cpts(cpt.plot))+1)){
    thisCpt <- time_i[cpts(cpt.plot)[n]]
    thatCpt <- time_i[cpts(cpt.plot)[n]+1]
    midCpt <- mean(c(thisCpt, thatCpt))
    #
    thisCpt_next <- time_i[cpts(cpt.plot)[n-1]]
    thatCpt_next <- time_i[cpts(cpt.plot)[n-1]+1]
    midCpt_next <- mean(c(thisCpt_next, thatCpt_next))
    #
    if(n == 1){
      lines(c(timeMin, midCpt), c(param.est(cpt.plot)$mean[n], param.est(cpt.plot)$mean[n]), col="blue", lwd=2)
    }
    else if(n == length(cpts(cpt.plot))+1) {
      lines(c(midCpt_next, timeMax) ,
            c(param.est(cpt.plot)$mean[n], param.est(cpt.plot)$mean[n]), col="blue", lwd=2)
    }
    else {
      lines(c(midCpt_next, midCpt) ,
            c(param.est(cpt.plot)$mean[n], param.est(cpt.plot)$mean[n]), col="blue", lwd=2)
    }
  }
}

#' Plot uncertainties
#'
#' @description
#' Plot uncertainty windows based on the supplied ages
#'
#' @param chron data frame containing age iterations
#' @param datV a numeric vector containing a principle curve or other data to be plotted
#' @param xmin min x axis value
#' @param xmax max x axis value
#' @param res resolution of sample sequence
#' @param spline TRUE/FALSE to plot spline
#'
#' @noRd
#'
plotUncert<-function(chron, datV, xmin, xmax, res, spline=FALSE){
  # chron = chron
  # datV = datV
  # xmin = xmin
  # xmax = xmax
  # res = res
  Z <- seq(xmin, xmax, res)
  aprofun <- function(x) approx(x, datV, Z)$y
  if(spline){
    aprofun <- function(x) predict(smooth.spline(x, datV), Z)$y}
  quantfun <- function(x) c(quantile(x, na.rm=TRUE, probs = c(0.05, 0.32, 0.5, 0.68, 0.95)), median(x, na.rm=TRUE), mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))
  apro <- apply(chron, 2, aprofun)
  quants <- data.frame(t(apply(apro, 1, quantfun)))
  xx <- c(Z, rev(Z))
  yy0.05 <- c(quants[,1], rev(quants[,5]))
  yy0.32 <- c(quants[,2], rev(quants[,4]))
  polygon(xx, yy0.05, col="#99dbfa", lty=0)
  polygon(xx, yy0.32, col="#47b6e7", lty=0)
  lines(Z, quants[,3], col="#3179b8", lwd=1)
}
