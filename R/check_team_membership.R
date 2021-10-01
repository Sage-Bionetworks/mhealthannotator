#' Get teams a user belongs to
#'
#' Looks up the teams a user belongs to. By default, it looks for teams of the
#' current logged in user. You must be logged in to Synapse to use this
#' function.
#'
#' @noRd
#' @param user Synapse user object (e.g. output from syn$getUserProfile())
#' @return Character vector of team IDs the user belongs to
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' user <- syn$getUserProfile()
#' get_user_teams(user, syn)
#' }
check_team_membership <- function(syn, user_id, team_id) {
    team_check_pass <- TRUE
    if(!is.null(team_id)){
        user_teams <- syn$restGET(
            glue::glue("/user/{user_id}/team?limit=10000"))$results %>%
            purrr::map_chr(function(x) x$id)
        if(team_id %in% user_teams){
            team_check_pass <- TRUE
        }else{
            team_check_pass <- FALSE
        }
    }
    return(team_check_pass)
}