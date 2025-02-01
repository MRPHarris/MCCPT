# Functions used for conducting changepoint analysis.
# Various internal and exported fns.

#' Run a changepoints analysis (internal)
#'
#' @description
#' Function used to run a changepoint analysis on a given set of data.
#'
#' @param site_data data from one site, a subset of a list returned by import_data()
#' @param site_name the name of the current site.
#' @param minseg_len NULL or a numeric value for minimum number of segments
#' @param n_cpts NULL or a numeric value for the number of changepoints
#' @param PrC_results results from generate_PrC()
#'
#' @noRd
#'
run_cpts <- function(site_data = sites_data[[i]],
                     site_name = names(sites_data)[i],
                     minseg_len = NULL,
                     n_cpts = NULL,
                     PrC_results = PrC_results){
  # # Vars
  # site_data <- sites_data[[i]]
  # site_name <- names(sites_data)[i]
  # minseg_len = NULL
  # n_cpts = NULL
  # PrC_results = PrC_results

  # Data passover
  dat_i = PrC_results$scrs
  time_i = PrC_results$time

  # Get n observation
  obvs<-as.numeric(nrow(as.data.frame(time_i)))
  # Here k opeates
  likemodel=0 # set k to zero so that you can reset the loop
  while(likemodel<1){
    ## Segment length selection
    if(is.null(minseg_len)){
      message("Set MinSeg length for ",site_name,
              "\n","Number of observations between ",age_lowerbound,"-",age_upperbound," is ", obvs)
      k <- readline(prompt = "MinSeg = ")
      k<-as.integer(k)
    } else {
      if(!is.numeric(minseg_len)){stop("minseg_len must be NULL or numeric")}
      k = minseg_len
    }
    model_status_df[i,4] <- k
    ## N changepoint selection
    if(is.null(n_cpts)){
      # Q = max n changepoints to search for
      message("Set n changepoints for ", site_name)
      q <- readline(prompt = "Q = ")
      q<-as.integer(q)
    } else {
      if(!is.numeric(n_cpts)){stop("n_cpts must be NULL or numeric")}
      q <- n_cpts
    }
    # first we set the skip logical to FALSE. If it fails the trycatch,
    # i.e. the model is so bad it fails, then the skip to next is set to true
    # and the user is prompted to try a different minseg and Q.
    skip_to_next <- FALSE
    # Attempt model, output error to a function (no real idea why R does this)
    test <- tryCatch(cpt.meanvar(dat_i, penalty = "Manual",pen.value="2*log(n)",
                                 minseglen = k, Q=q, method="BinSeg", class = TRUE),
                     error = function(e) {
                       skip_to_next <<- TRUE
                     })
    # if skip to next is true, skip iteration.
    if(skip_to_next) {
      message("\n","Models failed due to minseglen error. Skipping iteration.","\n","------")
      Site_Models_Status[i,2] = "FAIL"
      Site_Models_Status[i,3] = "minseglenth error"
      midY <- median(c(min(dat_i), max(dat_i)))
      lines(c(age_lowerbound_i, age_upperbound_i), c(midY,midY), col="blue", lwd=2)
      mtext(paste0(sites_df[i,1]), side=3, adj=0, cex=1.3, line=0.2)
      mtext("Age (cal. yr BP)", side=1, cex=1.3, line=3)
      box(lwd=2)
      dev.copy(pdf,paste0(output_folder,sites_df[i,1],"_CPT.pdf"),  width=10, height=6)
      if (length(dev.list()!=0)) {dev.off()}
      message("completed site ",i,"/",nrow(sites_df),"\n","-------")
      next
    } else {
      bm1_i <- cpt.meanvar(dat_i, penalty = "Manual",pen.value="2*log(n)",
                           minseglen = k, Q=q, method="BinSeg", class = TRUE )
      ncpts<-as.numeric(nrow(as.data.frame(bm1_i@cpts)))
      cpt.plot<-bm1_i
      plot.cpts(cpt.plot, time=time_i, timescale = "BP")
      likemodel <- readline("Do you like this model: 0=No, 1=Yes ")
      #k=1
    }
  }
  if (skip_to_next){
    next
  }
  cpt.plot
}

#' Generate principle curve scores for a given set of data
#'
#' @description
#' Do the PrC
#'
#' @param site_data data from a single
#' @param age_upper upperbound of age
#' @param age_lower lowerbound of age
#'
#' @noRd
#'
generate_PrC <- function(site_data,
                         age_upper = age_upperbound,
                         age_lower = age_lowerbound){
  ## test vars
  # sites_data = sites_data
  # age_upperbound = age_lowerbound
  # age_lowerbound = age_lowerbound
  ## var passover
  age_upperbound = age_upper
  age_lowerbound = age_lower
  ## Age handling
  # See e.g. data for format.
  bestage_i <- site_data$ages$Mean
  ages_i <- site_data$ages %>%
    select(-c(Depth_cm,Median))
  ## Per-iteration age handling
  if(is.null(age_lowerbound) & is.null(age_upperbound)){
    message("Set age bounds for site ",i,"/",nrow(sites_df)," named ",sites_df[i,1])#,
    # "\n","Number of observations is ", obvs)
    # Prompt user for age lowerbound
    l <- readline(prompt = "Lowerbound = ")
    age_lowerbound_i <- as.integer(l)
    model_status_df$upperbound[i] <- age_lowerbound_i
    # Do the same for upperbound
    u <- readline(prompt = "Uowerbound = ")
    age_upperbound_i <- as.integer(u)
    model_status_df$upperbound[i] <- age_upperbound_i
  } else {
    # This is the default; user should probably supply age bounds to be consistent.
    age_lowerbound_i = age_lowerbound
    age_upperbound_i = age_upperbound
  }
  ## Pollen site iteration
  PrC_res = vector('list', length = 3) %>%
    'names<-'(c('scrs','time','ageits'))
  # Pollen Sites
  if(site_data$metadata[which(site_data$metadata$category == 'Analysed Proxies'),2] == "Pollen"){
    # Reset the graphics device
    if (length(dev.list()!=0)) {dev.off()}
    # extract pollen % data from imported file
    dat_i = site_data$data [,-c(1)] %>%
      mutate(age = bestage_i) %>%
      relocate(age)
    # subset data to desired age range, specified as input parameters age_lowerbound and age_upperbound.
    dat_i <- subset(dat_i, dat_i$age > age_lowerbound_i & dat_i$age < age_upperbound_i)
    # This line would be the one to tweak if you wanted to ensure data points outside of the 15-35k range are included
    ages_i <- subset(ages_i, ages_i$Mean > age_lowerbound_i & ages_i$Mean < age_upperbound_i)
    bestage_i <- subset(bestage_i, bestage_i > age_lowerbound_i & bestage_i < age_upperbound_i)
    # Delete final column (contains 'Rumex' data)
    pollen_i <- dat_i %>%
      select(-c(age))
    # Run a prcurve on the pollen data
    pollen_pc_i <- analogue::prcurve(
      pollen_i,
      method = 'pca',
      trace = TRUE,
      plotit = FALSE,
      vary = FALSE,
      penalty = 1.2)
    # system sleep so that the output shows up.
    # Sys.sleep(0)
    # store prcurve scores
    scrs_i <- scores(pollen_pc_i)
    # Sys.sleep(0)
    PrC_res$scrs <- as.numeric(na.omit(scrs_i))
    PrC_res$time <- bestage_i
    PrC_res$ageits <- ages_i
    # dat_i <- as.numeric(na.omit(scrs_i))
    # time_i <- bestage_i
    # ageits_i <- ages_i
  } else {
    # NON-POLLEN SITES
    # Reset null device
    if (length(dev.list()!=0)) {dev.off()}
    # Get dat
    dat_i = site_data$data [,-c(1)] %>%
      mutate(age = bestage_i) %>%
      relocate(age)
    # subset data to desired age range, specified as input parameters age_lowerbound and age_upperbound.
    dat_i <- subset(dat_i, dat_i$age > age_lowerbound_i & dat_i$age < age_upperbound_i)
    # This line would be the one to tweak if you wanted to ensure data points outside of the 15-35k range are included
    ages_i <- subset(ages_i, ages_i$Mean > age_lowerbound_i & ages_i$Mean < age_upperbound_i)
    bestage_i <- subset(bestage_i, bestage_i > age_lowerbound_i & bestage_i < age_upperbound_i)
    # dat_i <- read.xlsx(paste0(data_dir,"/",sites_df[i,1],".xlsx"), sheetName = "Data", header = TRUE)
    # replace sample with corresponding age
    dat_i[,1] <- bestage_i
    colnames(dat_i)[1] <- "age"
    # subset data to desired age range, specified as input parameters age_lowerbound and age_upperbound.
    dat_i <- subset(dat_i, dat_i$age > age_lowerbound_i & dat_i$age < age_upperbound_i)
    # store prcurve scores
    scrs_i <- as.matrix(dat_i[,2])
    PrC_res$scrs <- as.numeric(na.omit(scrs_i))
    PrC_res$time <- bestage_i
    PrC_res$ageits <- ages_i
    # dat_i <- as.numeric(na.omit(scrs_i))
    # time_i <- bestage_i
    # ageits_i <- ages_i
  }
  # Return results
  return(PrC_res)
}

