#' App Server
#' 
#' Create the server-side component of mhealthannotator app
#' 
#' @import shiny
#' @import shinydashboard
#' @import waiter
#' @import reticulate 
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @return none
#' @export
app_server <- function( input, output, session ) {
  # instantiate synapse
  syn <- synapseclient$Synapse()
  if(interactive()){
    attempt_login(syn)
  }else{
   # use oauth 
    syn <- mod_synapse_oauth_server(
      id = "oauth",
      syn = syn
    )
  }
  shiny::req(
    inherits(syn, "synapseclient.client.Synapse"), 
    logged_in(syn))
  mod_main_server("main", syn = syn) 
}
