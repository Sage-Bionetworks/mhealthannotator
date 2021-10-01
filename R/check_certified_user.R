check_certified_user <- function(user_id) {
    is_certified <- FALSE
    if(syn$is_certified(user_id)){
        is_certified <- TRUE
    }
    return(is_certified)
}
