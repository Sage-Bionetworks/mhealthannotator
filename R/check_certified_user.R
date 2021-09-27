check_certified_user <- function(syn) {
    is_certified <- FALSE
    user <- syn$getUserProfile()
    user_id <- user$ownerId
    if(syn$is_certified(user_id)){
        is_certified <- TRUE
    }
    return(is_certified)
}
