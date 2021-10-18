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
  })

  #########################
  # render progress box
  #########################
  output$progressBox <- renderInfoBox({
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    infoBox(
      "Session Progress", glue::glue(total_curated,
                                     "/", nrow(values$useDf),
                                     " (", round(100 * total_curated/nrow(values$useDf), 1),"% Annotated)"),
      icon = icon("percentage"),
      color = "green"
    )
  })

  #########################
  # render total curated
  #########################
  output$totalCurated <- renderInfoBox({
    perc_curated <- (values$curatedDf %>% nrow())/(nrow(values$allDf))
    infoBox(
      "Total Curation in Synapse", glue::glue(values$curatedDf %>% nrow(),
                                              "/", nrow(values$allDf),
                                              " (", round(100 * perc_curated, 1),"% Annotated)"),
      icon = icon("tasks"),
      color = "purple"
    )
  })

  ##############################################
  # render survey prompt module
  ##############################################
  callModule(mod_survey_input_user_server, 
             "survey_input_ui", 
             survey_colnames = survey_config$survey_colnames,
             values = values)
  callModule(mod_render_image_server, 
             "render_image_ui",
             obj_path = values$useDf$imagePath[values$ii],
             input_width = image_config$width,
             input_height = image_config$height)

  ##################################
  # render go forward button
  ##################################
  observeEvent(input$goNext, {
    # store survey input 
    values$useDf <- values$useDf %>%
      store_inputs(
        curr_index = values$ii, 
        user_inputs = values$userInput,
        keep_metadata = synapse_config$keep_metadata,
        uid = synapse_config$uid
      )
    
    # call module to render image
    callModule(mod_render_image_server, 
               "render_image_ui",
               obj_path = values$useDf$imagePath[values$ii],
               input_width = image_config$width,
               input_height = image_config$height)
    
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    if((total_curated == nrow(values$useDf)) & !values$postConfirm){
      ask_confirmation(
        inputId = "confirmation",
        title = "Thank You!! \n You have finished this session!",
        btn_labels = c("Review before saving", "Save to Synapse"),
        btn_colors = c("#FE642E", "#04B404"),
        type = "success")
      values$postConfirm <- TRUE
    }
    
    if(values$ii == values$useDf %>% nrow(.)){
      tmpI <- 1
    } else{
      tmpI <- values$ii + 1
    }
    values$ii <- tmpI
    values <- update_inputs(
      reactive_values = values,
      session = session, 
      curr_index = values$ii,
      config = config$survey_opts)
  })

  #################
  # render go back button
  ##################
  observeEvent(input$goPrev, {
    values$useDf <- values$useDf %>%
      store_inputs(curr_index = values$ii, 
                         user_inputs = values$userInput,
                         keep_metadata = synapse_config$keep_metadata,
                         uid = synapse_config$uid)
    callModule(mod_render_image_server, 
               "render_image_ui",
               obj_path = values$useDf$imagePath[values$ii],
               input_width = image_config$width,
               input_height = image_config$height)
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    if((total_curated == nrow(values$useDf)) & !values$postConfirm){
      ask_confirmation(
        inputId = "confirmation",
        title = "Thank You!! \n You have finished your annotation!",
        btn_labels = c("Review before saving", "Save to Synapse"),
        btn_colors = c("#FE642E", "#04B404"),
        type = "success")
      values$postConfirm <- TRUE
    }
    if(values$ii > 1){
      tmpI <- values$ii - 1
    }else{
      tmpI <- values$useDf %>% nrow(.)
    }
    values$ii <- tmpI
    values <- update_inputs(
      reactive_values = values,
      session = session, 
      curr_index = values$ii,
      config = config$survey_opts)
  })
  
  ##################################
  # ask for confirmation
  ##################################
  observeEvent(input$confirmation, {
    if(input$confirmation){
      shinyjs::click(id = "save")
    }
  })

  ##################################
  # render save button
  ##################################
  observeEvent(input$save, {
    req(input$save)
    
    # reset post confirmation
    values$postConfirm <- FALSE
    
    # clear directory & create user directory
    clear_user_directory(values$currentAnnotator)
    create_user_directory(values$currentAnnotator)
    
    # show modal spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      text = shiny::tagList(
        h3("Please Wait..."),
        h4("We are uploading your data to Synapse."))
    )
    
    # save to synapse
    syn_id <- store_to_synapse(
      syn = syn,
      synapseclient = synapseclient,
      parent_id = synapse_config$output_parent_id,
      new_data = values$useDf,
      stored_data = values$curatedDf,
      current_annotator = values$currentAnnotator,
      output_filename = values$fileName
    )
    
    # remove when done
    Sys.sleep(2)
    shinybusy::remove_modal_spinner()
    
    # show modal spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      text = shiny::tagList(
        h3("Please Wait..."),
        h4("We are fetching more data..."))
    )
    
    # reset to 1
    values$ii <- 1
    
    #' get all data and previous data
    values$allDf <- get_source_table(
      syn = syn, 
      filehandle_cols = synapse_config$filehandle_cols,
      synapse_tbl_id = synapse_config$synapse_tbl_id)
    
    #' get previous image that has been curated
    values$curatedDf <- get_stored_annotation(
      syn = syn,
      parent_id = synapse_config$output_parent_id,
      stored_filename = values$fileName,
      uid = synapse_config$uid,
      keep_metadata = synapse_config$keep_metadata,
      survey_colnames = survey_config$survey_colnames
    )
    
    # refresh if ran out of images
    if(nrow(values$allDf) == nrow(values$curatedDf)){
      shinyjs::refresh()
    }else{
      # batch process filehandles
      values$useDf <- get_annotation_batch(
        syn = syn,
        all_data = values$allDf,
        curated_data = values$curatedDf,
        synapse_tbl_id = synapse_config$synapse_tbl_id,
        filehandle_cols = synapse_config$filehandle_cols,
        uid = synapse_config$uid, 
        survey_colnames = survey_config$survey_colnames,
        keep_metadata = synapse_config$keep_metadata,
        n_batch = synapse_config$n_batch,
        sort_keys = synapse_config$sort_keys,
        output_location = values$outputLocation,
        cache_location = values$cacheLocation,
        visualization_funs = visualization_funs
      )
      
      # update buttons
      values <- update_inputs(
        reactive_values = values,
        session = session, 
        curr_index = values$ii,
        config = config$survey_opts)
      
      # re-render image
      callModule(mod_render_image_server, 
                 "render_image_ui",
                 obj_path = values$useDf$imagePath[values$ii],
                 input_width = image_config$width,
                 input_height = image_config$height)
      
      # remove when done
      Sys.sleep(2)
      shinybusy::remove_modal_spinner()
      
      response <- glue::glue("Your saved annotation: {syn_id}")
                    
      # send sweet alert
      sendSweetAlert(
          session = session,
          title = "Session is updated!",
          text = response,
          type = "success"
      )
    }
  })
  
  ##################################
  # render data table
  ##################################
  output$metadata_table = DT::renderDataTable({
    data <- values$useDf[values$ii,] %>%
      dplyr::select(
        all_of(synapse_config$uid),
        all_of(synapse_config$keep_metadata), 
        all_of(survey_config$survey_colnames),
        fileColumnName,
        annotationTimestamp)
    DT::datatable(
      data, options = list(
        searching = FALSE, 
        scrollX = TRUE,
        lengthChange= FALSE))
  })
}
