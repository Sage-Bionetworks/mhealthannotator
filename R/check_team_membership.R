#' Function to check if user is under a Synapse team
#'
#' @noRd
#' 
#' @param syn synapselcient
#' @param user_id synapse user id (numerical)
#' @param team_id synapse team id
#' 
#' @return boolean value if user is a team member
check_team_membership <- function(syn, user_id, team_id) {
    team_check_pass <- FALSE
    if(!is.null(team_id)){
        user_teams <- syn$restGET(
            glue::glue("/user/{user_id}/team?limit=10000"))$results %>%
            purrr::map_chr(function(x) x$id)
        if(team_id %in% user_teams){
            team_check_pass <- TRUE
        }
    }
    return(team_check_pass)
}