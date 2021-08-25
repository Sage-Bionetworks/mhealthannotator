#' App UI
#' 
#' Create the UI component of mhealthannotator Shiny App
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request
#' @export
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_ui <- function(request) {
  mod_main_ui("main")
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
golem_add_external_resources <- function(){
  
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    golem::favicon(),
    golem::activate_js(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'mhealthannotator'
    ),
    tags$script(src = "www/readCookie.js")
  )
}

