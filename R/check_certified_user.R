#' Function to check if user is certified
#' 
#' @noRd
#' 
#' @param syn synapseclient 
#' @param user_id synapse user_id (numerical)
#' 
#' @return boolean value if user is certified or not
check_certified_user <- function(syn, user_id) {
    is_certified <- FALSE
    if(syn$is_certified(user_id)){
        is_certified <- TRUE
    }
    return(is_certified)
}
