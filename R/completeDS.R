#'
#' @title DataSHIELD server-side wrapper function for mice::complete
#' @description This function is a wrapper for the complete function from the mice R package.
#' It extracts the completed data from a mids object containing multiply imputed datasets.
#' @details For additional details see the help header of complete function in native R mice package.
#' @param mids_obj a character string specifying the name of the mids object containing the 
#' multiply imputed datasets.
#' @param action a numeric value or character string specifying which imputation to extract. 
#' The default is 1, which returns the first completed dataset. Other possibilities are 
#' "long", "broad", "repeated". See mice::complete documentation for details.
#' @param include a logical value indicating whether the original data should be included. 
#' Only relevant when action is "long" or "repeated". Default is FALSE.
#' @return a data.frame containing the completed data as specified by the action parameter.
#' @author DataSHIELD Development Team
#' @import mice
#' @export
#'
completeDS <- function(mids_obj=NULL, action=1, include=FALSE){
  
  if(is.null(mids_obj)){
    stop("Please provide the name of the mids object", call.=FALSE)
  }
  
  # Evaluate the mids object from its name
  mids_obj <- eval(parse(text=mids_obj), envir = parent.frame())
  
  # Check if the provided object is a mids object
  if(!inherits(mids_obj, "mids")){
    stop("The provided object is not a mids object", call.=FALSE)
  }
  
  # Convert action to numeric if it's a number stored as character
  if(is.character(action) && !action %in% c("long", "broad", "repeated")){
    action <- as.numeric(action)
  }
  
  # Validate action parameter
  if(is.numeric(action)){
    if(action < 1 || action > mids_obj$m){
      stop("action must be between 1 and the number of imputations", call.=FALSE)
    }
  } else if(!action %in% c("long", "broad", "repeated")){
    stop("action must be either a number or one of 'long', 'broad', 'repeated'", call.=FALSE)
  }
  
  # Call mice::complete with the provided parameters
  completed_data <- mice::complete(data=mids_obj, action=action, include=include)
  
  return(completed_data)
}
