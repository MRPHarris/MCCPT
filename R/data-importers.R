# Functions for data import

#' Get all sites in the target folder
#'
#' @description
#' Scrape the supplied directory for sites for CPT analysis, and load them into a formatted list.
#'
#' @param folder_path a full path to the directory containing site xlsx files
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#'
#'
import_files <- function(target_folder, pattern_filter = NULL) {
  # List all .xlsx files
  files <- list.files(target_folder, pattern = "\\.xlsx$", full.names = TRUE)

  # If pattern_filter is given, keep only files that match any pattern in it
  if (!is.null(pattern_filter)) {
    # pattern_filter is expected to be a character vector of regex or fixed strings
    keep <- sapply(pattern_filter, function(pat) grepl(pat, basename(files), ignore.case = TRUE))
    keep <- apply(keep, 1, any)  # keep files matching ANY pattern
    files <- files[keep]
  }

  flist <- vector('list', length(files))
  names <- vector('character', length = length(files))

  for (f in seq_along(files)) {
    file_path <- files[f]
    file_name <- basename(file_path)
    file_base <- gsub("\\.xlsx$", "", file_name)  # Remove extension

    # Import data
    site_data <- import_site_xlsx(file_path)

    # Extract site name from metadata
    site_name <- site_data$metadata[2, 2]

    # Normalize site_name to match file_base style
    norm_site_name <- gsub(" ", "_", trimws(site_name))

    # Escape regex special characters (as before)
    escape_regex <- function(x) {
      gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x)
    }
    norm_site_name_esc <- escape_regex(norm_site_name)

    # Now remove norm_site_name + optional underscore from start of file_base
    suffix <- gsub(paste0("^", norm_site_name_esc, "_?"), "", trimws(file_base), ignore.case = TRUE)


    # Construct unique name for the list entry
    if (suffix != "") {
      list_name <- paste0(site_name, "_", suffix)
    } else {
      list_name <- site_name
    }

    flist[[f]] <- site_data
    names[f] <- list_name
  }

  names(flist) <- names
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
