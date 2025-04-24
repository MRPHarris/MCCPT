# Plotting utilities

#' Plot changepoint locations
#'
#' @description Plot vertical lines at the location of changepoints in a given curve (previously generated)
#'
#' @param cpt.plot a cpt.plot object
#' @param ageits_i age iterations from this site
#' @param plot TRUE or FALSE to plot densities. Data is returned regardless.
#'
#' @importFrom stats weighted.mean
#' @importFrom stats density
#' @importFrom magrittr %>%
#' @importFrom changepoint cpts
#'
#' @noRd
#'
CPTwindow <- function(cpt.plot,
                      ageits_i,
                      plot = F){
  # making a list that we'll slot the cpt_density objects into.
  ncpts <- length(cpt.plot@cpts)
  density_list <- vector('list', length = ncpts)
  # test <- cpts(cpt.plot)
  for(n in seq_along(cpts(cpt.plot))){
    # cpt.plot <- cpt.plot
    # ageits_i <- ageits_i
    cptDates <- matrix(NA, ncol(ageits_i), 1)
    for(it in 1:ncol(ageits_i)){
      time_i <- ageits_i[,it] %>% as.matrix() %>% as.numeric()
      time_i[cpts(cpt.plot)[n]]
      endCPT <- time_i[cpts(cpt.plot)[n]]
      startCPT <- time_i[cpts(cpt.plot)[n]+1]
      cpt <- mean(c(endCPT, startCPT))
      cptDates[it] <- cpt
    }
    m <- mean(cptDates)
    med <- median(cptDates)
    wm <- weighted.mean(cptDates)
    firstQ <- summary((density(cptDates)$x))[2]
    thirdQ <- summary((density(cptDates)$x))[5]
    if(plot){
      plot(density(cptDates))
      abline(v=med, col="red", lwd=2)
      abline(v=wm, col="dark blue", lwd=2)
      abline(v=firstQ, col="forest green", lwd=2)
      abline(v=thirdQ, col="forest green", lwd=2)
    }
    # cptDensity <- density(cptDates)
    abline(v=med, col="red", lwd=2)
    abline(v=wm, col="orange",lwd=2)
    # assign(paste0("cptDensity_",n),cptDensity, envir = parent.frame())
    density_list[[n]] <- density(cptDates)
    density_list[[n]]$mean <- m
    density_list[[n]]$median <- med
    density_list[[n]]$wgtmean <- wm
    density_list[[n]]$firstQ <- firstQ
    density_list[[n]]$thirdQ <- thirdQ
  }
  density_list <- density_list[1:(ncpts)]
}

#' Plot changepoints over an existing uncertainties plot
#'
#' @description plot changepoints on a proxy record.
#'
#' @param cpt.plot changepoint result to plot; output from changepoint::cpt.meanvar
#' @param time time increments from previous PrC analysis
#' @param timescale timescale to use. One of either "BP" or "BCAD".
#'
#' @importFrom changepoint param.est
#'
#' @noRd
#'
plot.cpts<- function(cpt.plot, time, timescale = "BCAD"){
  time_i = time
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

#' Plot record prior to displaying changepoints
#'
#' @description plot a record with uncertainty windows across the supplied time period
#'
#' @param PrC_results output from generate_PrC
#' @param age_upper upper age limit in years
#' @param age_lower lower age limit in years
#' @param rev_y TRUE/FALSE to reverse y axis
#' @param uncertainty_res resolution of the uncertainty in years
#'
#' @noRd
#'
plot_cpt_pre <- function(PrC_results,
                         age_upperbound,
                         age_lowerbound,
                         rev_y,
                         uncertainty_res,
                         verbose,
                         name){
  # Get PrC results
  # PrC_results <- generate_PrC(site_data = site_data,
  #                             age_upper = age_upper,
  #                             age_lower = age_lower)
  dat_i <- PrC_results$scrs
  time_i <- PrC_results$time
  ageits_i <- PrC_results$ageits
  ## Plot 1: Data (or PrC scores) with age uncertainty
  # clear plots
  if (length(dev.list()!=0)) {dev.off()}
  # Set plot margins
  par(oma = c(3,0,0,0))
  par(fig=c(0,0.5,0.2,0.9), mar=c(2,2,2,1))
  if(verbose){
    message("Plotting age uncertainty.","\n","------")
  }
  # Plot data
  plot(time_i, dat_i, type="n", ylab="", xlab="", xlim=c(age_lowerbound, age_upperbound), ylim=(range(dat_i)), cex.axis=1.2)
  box(lwd=2)
  # Uncertainty
  plotUncert(chron = ageits_i, datV = dat_i, xmin = min(ageits_i), xmax = max(ageits_i), res = uncertainty_res, spline=FALSE)
  lines(time_i, dat_i, col = rgb(0,0,0, alpha=0.5), lwd=1.5)
  points(time_i, dat_i, pch=".", cex=3.5, col="black")
  # Text
  mtext(name, side=3, adj=0, cex=1.3, line=0.2)
  mtext("Age (cal. yr BP)", side=1, cex=1.3, line=3)
  box(lwd=2)
  # Reverse y axis if chosen
  if(rev_y){
    ylim<-rev(range(dat_i))
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
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats approx
#' @importFrom stats smooth.spline
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
    aprofun <- function(x) predict(smooth.spline(x, datV), Z)$y
  }
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
