# Functions for data import

#' Get all sites in the target folder
#'
#' @description
#' Scrape the supplied directory for sites for CPT analysis, and load them into a formatted list.
#'
#' @param folder_path a full path to the directory containing site xlsx files
#' @param verbose TRUE/FALSE to print per-file update progress
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
import_files <- function(target_folder,
                         verbose = TRUE){
  ## test vars
  # target_folder <- paste0(proj_dir,"/data-raw/Stradbroke-comp-raw/")
  # list files containing '.xlsx'
  files <- list.files(target_folder, full.names = T)[grep(".xlsx",list.files(target_folder, full.names = F))]
  # make list of same length as files name quantity
  flist <- vector('list', length(files))
  names <- vector('character',length = length(files))
  # Import, add to list, and get names of each site.
  for(f in seq_along(flist)){
    flist[[f]] <- import_site_xlsx(files[f])
    names[f] <- flist[[f]]$metadata[2,2]
    if(verbose){message("Imported file ",f,"/",length(flist))}
  }
  # assign names
  flist <- flist %>% 'names<-'(names %>% unlist())
  # return
  return(flist)
}

#' Import a of single site xlsx file for changepoint analysis
#'
#' @description
#' Use openxlsx to import a single spreadsheet's data for an individual site - metadata, data and data ages.
#'
#' @param file_path a full path to the site xlsx spreadsheet to be imported
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom magrittr %>%
#'
#' @noRd
#'
import_site_xlsx <- function(file_path){
  # example params
  # file_path = paste0(proj_dir,"/data-raw/Stradbroke-comp-raw/NativeCompanionLagoon_with ages.xlsx")
  # Init data list to be filled
  data_list <- vector('list',length = 3) %>%
    'names<-'(c('metadata','data','ages'))
  # Get metadata, suppress column renaming because it is annoying
  suppressMessages(
    data_list$metadata <- read.xlsx(xlsxFile = file_path, sheet = "Metadata", colNames = F) %>% 'colnames<-'(c('category','value'))
  )
  # Get pollen percent
  data_list$data <- read.xlsx(xlsxFile = file_path, sheet = "Data", colNames = T) #%>% 'colnames<-'(c('category','value'))
  # Get ages of pollen
  data_list$ages <- read.xlsx(xlsxFile = file_path, sheet = "Age_iterations", colNames = T) #%>% 'colnames<-'(c('category','value'))
  # return
  return(data_list)
}
