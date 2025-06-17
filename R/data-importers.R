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

    # Function to normalize names for matching: replace underscores/spaces with nothing, lowercase
    normalize_name <- function(x) {
      tolower(gsub("[ _]", "", x))
    }

    norm_site_name_norm <- normalize_name(norm_site_name)
    file_base_norm <- normalize_name(file_base)

    # If file_base starts with norm_site_name → extract the suffix
    if (startsWith(file_base_norm, norm_site_name_norm)) {
      raw_suffix <- substr(file_base, nchar(norm_site_name) + 1, nchar(file_base))
      raw_suffix <- sub("^[_ ]*", "", raw_suffix)  # Remove leading _ or space from suffix
    } else {
      raw_suffix <- file_base  # fallback, shouldn't happen often
    }

    # Final list_name
    if (nzchar(raw_suffix)) {
      list_name <- paste0(norm_site_name, "_", raw_suffix)
    } else {
      list_name <- norm_site_name
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
