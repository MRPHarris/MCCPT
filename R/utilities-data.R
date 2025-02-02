# Utility functions for data wrangling

#' Convert a list of simple vectors to a data frame
#'
#' @description Given a list containing only vectors or individual values, convert to a data frame with each element as a separate column
#'
#' @param list_object a list
#'
#' @importFrom magrittr %>%
#'
#' @noRd
#'
convert_to_df <- function(list_object){
  # list_object = save_list[[1]]
  lens <- lengths(list_object) %>% as.numeric()
  nms <- names(list_object)
  new_df <- data.frame(matrix(NA,nrow = max(lens, na.rm = T)))
  list_len <- vector('list',length(list_object))
  for(t in seq_along(list_len)){
    this_item <- list_object[[t]]
    this_vec <- c(this_item,rep(NA, length = max(lens, na.rm = T) - lens[t]))
    new_df[,t] <- this_vec
  }
  new_df <- new_df %>% 'colnames<-'(c(nms))
}
