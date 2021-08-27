#' @title UI Function main function
#' @description creates the RShiny overall UI
#' @export
#' @rdname main_ui
#' @param id identifier namespace
#' @return UI a dashboard page object that contains the RShiny App UI
mod_main_ui <- function(id) {
    ns <- NS(id)
    
    # create header
    header <- dashboardHeader(
        title = "mHealthAnnotator",
        titleWidth = 200)
    
    # create sidebar
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem(h4("About"), 
                     tabName = "about", 
                     icon = icon("question-circle")),
            menuItem(h4("Annotator-App"), 
                     icon = icon("user-nurse"), 
                     tabName = "annotator"))
    )
    
    # create body
    body <- dashboardBody(
        # Add resoources in www
        golem_add_external_resources(),
        
        # create tab items for UI
        tabItems(
            tabItem(tabName = "about",
                    h1("Welcome to the mHealthAnnotator RShiny App!"),
                    div("This app will help you go through Synapse Table-Attached Files and"),
                    div("based on your desired visualization for the files, the app will help you annotate each"),
                    div("scores/labels/information about the visualizations"),
                    br(),
                    h2("How-To:"),
                    div("1. Go to the 'Annotator-App' Tab"),
                    div("2. You can go through each image by using arrows"),
                    div("3. Score accordingly based on each prompts based on the right-side button UI"),
                    div("4. When finished, you can save the images by pressing 'Save Results' button"),
                    div("5. After saving, we will fetch you more data into the session"),
                    br(),
                    h2("References:"),
                    tags$a(href="https://github.com/Sage-Bionetworks/mhealthannotator", 
                           "Link to GitHub Package")
            ),
            
            # annotator UI
            tabItem(
                tabName = "annotator",
                fluidRow(
                    column(width = 3, infoBoxOutput(ns("userBox"), 
                                                    width = "200px")),
                    column(width = 4, infoBoxOutput(ns("progressBox"), 
                                                    width = "300px")),
                    column(width = 4, infoBoxOutput(ns("totalCurated"), 
                                                    width = "300px"))
                ),
                fluidRow(
                    br(),
                    column(width = 7, 
                           align = "center",
                           div(mod_render_image_ui(ns("render_image_ui")))),
                    column(width = 4,
                           offset = 1,
                           mod_survey_input_user_ui(ns("survey_input_ui")),
                           br(),
                           div(
                               style = "display:inline-block; float:left",
                               actionButton(ns("goPrev"), "", 
                                            icon = icon("arrow-left"), 
                                            width = "100px")),
                           div(
                               style = "display:inline-block; float:left",
                               actionButton(ns("goNext"), "", 
                                            icon = icon("arrow-right"), 
                                            width = "100px")),
                           br(),
                           br(),
                           br(),
                           div(style = "display:inline-block; float:left", 
                               actionButton(ns("save"), "Save My Results",
                                            style = "color: white; background-color: SteelBlue",
                                            icon = icon("cloud-upload"),
                                            width = "200px")))
                ),
                fluidRow(
                    br(),
                    br(),
                    br(),
                    conditionalPanel(
                        'input.dataset === "metadata-table"',
                    ),
                    tabsetPanel(
                        id = 'dataset',
                        tabPanel(
                            "metadata",
                            DT::dataTableOutput(ns("metadata_table"))))
                ),
                # instantiate waiter loading screen
                waiter::use_waiter(),
                waiter::waiter_show_on_load(
                    html = tagList(
                        img(src = "www/loading.gif"),
                        h4("Retrieving Synapse information...")
                    ),
                    color = "#424874"
                )
            )
        )
    )
    
    # consolidata header, sidebar and body
    dashboardPage(
        header,
        sidebar,
        body
    )
}


#' @title Server module for main function
#' @description create backend process of the RShiny App
#' @export
#' @rdname main_server
#' @param id namespace identifier
#' @param syn user Synapseclient object
mod_main_server <- function(id, syn) {
    moduleServer(
        id,
        function(input, 
                 output, 
                 session){
            shiny::req(inherits(
                syn, 
                "synapseclient.client.Synapse") & 
                    logged_in(syn))
            
            # read configuraiton file
            config_path <- 
                file.path(golem::get_golem_options("config"))
            config <- config::get(file = config_path)
            
            #' check config
            check_survey_config(config$survey_opts)
            check_synapse_config(config$synapse_opts)
            
            # gett all parameter
            synapse_config <- config$synapse_opts
            survey_config <- parse_survey_opts(config$survey_opts)
            image_config <- config$image_opts
            visualization_funs <- golem::get_golem_options("funs")
            synapse_profile <- syn$getUserProfile()$userName
            cache_location <- file.path(
                "dir", synapse_profile, "downloaded_files")
            output_location <- file.path(
                "dir", synapse_profile, "processed_files")
            output_filename <- glue::glue(
                "{annotator}_{filename}",
                filename = synapse_config$output_filename,
                annotator = synapse_profile)
            
            # create log directory
            dir.create("log", showWarnings = FALSE)
            dir.create("dir", showWarnings = FALSE) 
            
            # create user directory
            clear_user_directory(synapse_profile)
            create_user_directory(synapse_profile)
            
            #' get all data and previous data
            all_data <- get_source_table(
                syn = syn, 
                filehandle_cols = synapse_config$filehandle_cols,
                synapse_tbl_id = synapse_config$synapse_tbl_id)
            
            #' get previous image that has been curated
            curated_data <- get_stored_annotation(
                syn = syn,
                parent_id = synapse_config$output_parent_id,
                stored_filename = output_filename,
                uid = synapse_config$uid,
                keep_metadata = synapse_config$keep_metadata,
                survey_colnames = survey_config$survey_colnames
            )
            
            # return feedback message if all images are annotated
            if(nrow(curated_data) == nrow(all_data)){
                waiter_update(
                    html = tagList(
                        img(src = "www/synapse_logo.png", height = "120px"),
                        h2("Thank you, you have finished your annotations"),
                        h3("Come back next time!")
                    )
                )
                return("")
            }else{
                # update waiter loading screen once login successful
                waiter::waiter_update(
                    html = tagList(
                        img(src = "www/loading.gif"),
                        h4(sprintf("Retrieving Images from Synapse..."))))
                
                # batch process image filehandles
                annotation_data <- get_annotation_batch(
                    syn = syn,
                    all_data = all_data,
                    curated_data = curated_data,
                    synapse_tbl_id = synapse_config$synapse_tbl_id,
                    filehandle_cols = synapse_config$filehandle_cols,
                    uid = synapse_config$uid, 
                    survey_colnames = survey_config$survey_colnames,
                    keep_metadata = synapse_config$keep_metadata,
                    n_batch = synapse_config$n_batch,
                    sort_keys = synapse_config$sort_keys,
                    output_location = output_location,
                    cache_location = cache_location,
                    visualization_funs = visualization_funs)
                
                # update waiter loading screen once login successful
                waiter::waiter_update(
                    html = tagList(
                        img(src = "www/synapse_logo.png", height = "120px"),
                        h3(sprintf("Welcome, %s!", synapse_profile))
                    )
                )
                Sys.sleep(3)
                waiter::waiter_hide()
            }
            
            # define reactive values
            values <- reactiveValues(
                profile = synapse_profile,
                index = 1,  # image index
                user_input = list(),
                all_data = all_data,
                curated_data = curated_data,
                annotation_data = annotation_data,
                post_confirm = FALSE)
            
            #######################
            # render user box
            #######################
            output$userBox <- renderInfoBox({
                infoBox(
                    "Annotator", 
                    values$profile, 
                    icon = icon("user"),
                    color = "orange"
                )
            })
            

            #########################
            # render progress box
            #########################
            output$progressBox <- renderInfoBox({
                total_curated <- (values$annotation_data %>%
                                      tidyr::drop_na() %>% nrow(.))
                infoBox(
                    "Session Progress", glue::glue(
                        total_curated, "/", nrow(values$annotation_data),
                        " (", round(100 * total_curated/nrow(
                            values$annotation_data), 1),"% Annotated)"),
                    icon = icon("percentage"),
                    color = "green"
                )
            })

            #########################
            # render total curated
            #########################
            output$totalCurated <- renderInfoBox({
                perc_curated <- (values$curated_data %>% nrow())/(nrow(values$all_data))
                infoBox(
                    "Total Curation in Synapse", glue::glue(values$curated_data %>% nrow(),
                                                            "/", nrow(values$all_data),
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
                       obj_path = values$annotation_data$imagePath[values$index],
                       input_width = image_config$width,
                       input_height = image_config$height)

            ##################################
            # render go forward button
            ##################################
            observeEvent(input$goNext, {
                # store survey input
                values$annotation_data <- values$annotation_data %>%
                    store_inputs(
                        curr_index = values$index,
                        user_inputs = values$user_input,
                        keep_metadata = synapse_config$keep_metadata,
                        uid = synapse_config$uid)

                # call module to render image
                callModule(mod_render_image_server,
                           "render_image_ui",
                           obj_path = values$annotation_data$imagePath[values$index],
                           input_width = image_config$width,
                           input_height = image_config$height)

                total_curated <- (values$annotation_data %>% tidyr::drop_na() %>% nrow(.))
                print(total_curated)
                if((total_curated == nrow(values$annotation_data)) & !values$post_confirm){
                    ask_confirmation(
                        inputId = "confirmation",
                        title = "Thank You!! \n You have finished this session!",
                        btn_labels = c("Review before saving", "Save to Synapse"),
                        btn_colors = c("#FE642E", "#04B404"),
                        type = "success")
                    values$post_confirm <- TRUE
                }

                if(values$index == values$annotation_data %>% nrow(.)){
                    tmpI <- 1
                } else{
                    print("here")
                    tmpI <- values$index + 1
                }
                values$index <- tmpI
                values <- update_inputs(
                    id = "survey_input_ui",
                    values = values,
                    session = session,
                    curr_index = values$index,
                    config = config$survey_opts)
            })

            #################
            # render go back button
            ##################
            observeEvent(input$goPrev, {
                values$annotation_data <- values$annotation_data %>%
                    store_inputs(curr_index = values$index,
                                 user_inputs = values$user_input,
                                 keep_metadata = synapse_config$keep_metadata,
                                 uid = synapse_config$uid)
                callModule(mod_render_image_server,
                           "render_image_ui",
                           obj_path = values$annotation_data$imagePath[values$index],
                           input_width = image_config$width,
                           input_height = image_config$height)
                total_curated <- (values$annotation_data %>% tidyr::drop_na() %>% nrow(.))
                if((total_curated == nrow(values$annotation_data)) & !values$post_confirm){
                    ask_confirmation(
                        inputId = "confirmation",
                        title = "Thank You!! \n You have finished your annotation!",
                        btn_labels = c("Review before saving", "Save to Synapse"),
                        btn_colors = c("#FE642E", "#04B404"),
                        type = "success")
                    values$post_confirm <- TRUE
                }
                if(values$index > 1){
                    tmpI <- values$index - 1
                }else{
                    tmpI <- values$annotation_data %>% nrow(.)
                }
                values$index <- tmpI
                values <- update_inputs(
                    id = "survey_input_ui",
                    values = values,
                    session = session,
                    curr_index = values$index,
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
                values$post_confirm <- FALSE

                # clear directory & create user directory
                clear_user_directory(values$profile)
                create_user_directory(values$profile)

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
                    new_data = values$annotation_data,
                    stored_data = values$curated_data,
                    current_annotator = values$profile,
                    output_filename = output_filename
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
                values$index <- 1

                #' get all data and previous data
                values$all_data <- get_source_table(
                    syn = syn,
                    filehandle_cols = synapse_config$filehandle_cols,
                    synapse_tbl_id = synapse_config$synapse_tbl_id)

                #' get previous image that has been curated
                values$curated_data <- get_stored_annotation(
                    syn = syn,
                    parent_id = synapse_config$output_parent_id,
                    stored_filename = output_filename,
                    uid = synapse_config$uid,
                    keep_metadata = synapse_config$keep_metadata,
                    survey_colnames = survey_config$survey_colnames
                )

                # refresh if ran out of images
                if(nrow(values$all_data) == nrow(values$curated_data)){
                    shinyjs::refresh()
                }else{
                    # batch process filehandles
                    values$annotation_data <- get_annotation_batch(
                        syn = syn,
                        all_data = values$all_data,
                        curated_data = values$curated_data,
                        synapse_tbl_id = synapse_config$synapse_tbl_id,
                        filehandle_cols = synapse_config$filehandle_cols,
                        uid = synapse_config$uid,
                        survey_colnames = survey_config$survey_colnames,
                        keep_metadata = synapse_config$keep_metadata,
                        n_batch = synapse_config$n_batch,
                        sort_keys = synapse_config$sort_keys,
                        output_location = output_location,
                        cache_location = cache_location,
                        visualization_funs = visualization_funs
                    )

                    # update buttons
                    values <- update_inputs(
                        id = "survey_input_ui",
                        values = values,
                        session = session,
                        curr_index = values$index,
                        config = config$survey_opts)

                    # re-render image
                    callModule(mod_render_image_server,
                               "render_image_ui",
                               obj_path = values$annotation_data$imagePath[values$index],
                               input_width = image_config$width,
                               input_height = image_config$height)

                    # remove when done
                    Sys.sleep(2)
                    shinybusy::remove_modal_spinner()
                    
                    # create text response for saving
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
            
            # ##################################
            # # render data table
            # ##################################
            output$metadata_table = DT::renderDataTable({
                data <- values$annotation_data[values$index,] %>%
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
    )
}