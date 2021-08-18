#' @title Get new rows (unannotated)
#' 
#' @description get unannotated rows by anti-joining
#' fetched data with previously curated data
#' 
#' @param curated_data curated data stored in synapse
#' @param uid unique identifier to compare whether image
#' has been annotated or not
#' 
#' @return dataframe/tibble with unannotated rows
get_new_rows <- function(data, curated_data, uid){
  data %>%
    dplyr::anti_join(
      curated_data,
      by = c(uid, "fileColumnName"))
}


#' @title Sort rows arbitrary to sorting keys
#' 
#' @description sort dataframe rows for annotations
#' based on the keys provided
#' 
#' @param sort_keys the keys to sort based off
#' @return sorted dataframe/tibble
sort_rows <- function(data, sort_keys){
  # sort option 
  if(!is.null(sort_keys)){
    if(sort_keys == "random"){
      data <- data %>% sample_n(size = nrow(.))
    }else{
      data <- data %>%
        dplyr::arrange(!!sym(sort_keys))
    }
  }
  return(data)
}


#' @title Add input survey columns
#' 
#' @description add empty survey input columns 
#' and its corresponding filepath, and empty timestamps
#' to fill out in shiny session
#' 
#' @param survey_colnames the survey columns that will be stored
#' @return dataframe/tibble with survey columns
add_input_survey_cols <- function(data, survey_colnames){
  data %>%
    dplyr::mutate_all(as.character) %>%
      tidyr::drop_na(any_of(c("imagePath"))) %>%
      dplyr::bind_cols(
        (survey_tbl <- purrr::map_dfc(
          survey_colnames, function(x){
            tibble(!!sym(x) := as.character(NA))
          }))) %>%
      dplyr::mutate(annotationTimestamp = NA_character_)
}

#' @title Get Table Unique Identifier String Filter
#' 
#' @description This is a helper function to build string filter (SQL-like)
#' for filtering Synapse Table before downloading files 
#' Note: This is done to enable small-batch download
#' 
#' @param uid unique identifier used to 
#' @return a string of unique identifier that will
#' be included in the batch with parentheses 
get_table_string_filters <- function(uid){
  uid %>% 
    purrr::map_chr(., function(x){glue::glue("'", x, "'")}) %>%
    paste0(., collapse = ",") %>%
    glue::glue("(", ., ")")
}


#' @title Get unannotated files in batch
#' 
#' @description Helper function for downloading a number of Synapse table-attached 
#' files according to input batch, and process using desired function
#' 
#' @param data containing un-annotated data based on each user
#' @param syn synapse object
#' @param synapse_tbl_id synapse table id
#' @param filehandle_cols table filehandles
#' @param uid unique ID of images/plot/graphs
#' @param n_batch number of images/plot/graphs per annotation session
#' @param cache_location where to cache each user annotations
#' @param keep_metadata metadata to keep
#' @return unannotated data based on previously stored records
get_session_images <- function(data, 
                               syn, 
                               synapse_tbl_id, 
                               filehandle_cols, 
                               uid, 
                               n_batch,
                               cache_location,
                               keep_metadata = NULL){
  
  # set cache location
  syn$cache$cache_root_dir <- cache_location
  
  # get sql string statement for filtering data in synapse table
  get_subset <- data %>%
    dplyr::slice(1:n_batch) %>%
    .[[uid]] %>% 
    get_table_string_filters()
  
  # get synapse table entity
  entity <- syn$tableQuery(
    glue::glue(
      "SELECT * FROM {synapse_tbl_id} WHERE recordId IN {get_subset}"))
  
  # download all table columns
  syn$downloadTableColumns(
    table = entity, 
    columns = filehandle_cols) %>%
    tibble::enframe(.) %>%
    tidyr::unnest(value) %>%
    dplyr::select(
      fileHandleId = name, 
      filePath = value) %>%
    dplyr::mutate(filePath = unlist(filePath)) %>%
    dplyr::inner_join(data, by = c("fileHandleId")) %>%
    dplyr::select(all_of(uid), all_of(keep_metadata), 
                  fileColumnName, filePath)
}

#' @title Visualize Synapse Table Column Files
#' 
#' @description Helper function to visualize synapse column files
#' based on a custom function 
#' 
#' @param data data where it contains cached `filePath` of the table attached files
#' @param funs custom visualization function
#' @param output_location where to output the processed files location
#' 
#' @return a dataframe containing processed files
visualize_column_files <- function(data, funs, output_location){
  data %>%  
    dplyr::mutate(
      basePath = purrr::map_chr(
        filePath, function(fp){
          file.copy(fp, output_location)
          return(basename(fp))}),
      imagePath = file.path(output_location, basePath),
      imagePath = purrr::map_chr(
        imagePath, .f = funs))
}



#' @title Process Synapse Table Column Files
#' 
#' @description Process Synapse Table Filehandles in batch processing,
#' will take unannotated data and batch process each 
#' of them based on a visualization function
#' 
#' @param syn synapseclient
#' @param all_data dataframe of whole data from synapse table
#' @param curated_data dataframe of annotated ata
#' @param synapse_tbl_id synapse source table id
#' @param filehandle_cols filehandle column target to parse
#' @param uid unique identifier of each files
#' @param keep_metadata metadata to keep from the table
#' @param n_batch number of batch per session
#' @param output_location where to store processed files
#' @param cache_location where to find raw files
#' @param visualization_funs function to visualize data
#' @param survey_colnames the column for storing survey input
#' @param sort_keys sorting keys
#' 
#' @import tibble
#' @importFrom magrittr `%>%`
#' @import purrr
#' @import tidyr
#' 
#' @return a dataframe containing processed 
#' Table column files that will be used
#' for rendering in the Shiny App
#' 
#' @export
get_annotation_batch <- function(syn, 
                                 all_data, curated_data, 
                                 synapse_tbl_id, filehandle_cols, 
                                 uid, survey_colnames,
                                 n_batch, output_location, 
                                 visualization_funs,
                                 cache_location,
                                 keep_metadata = NULL,
                                 sort_keys = NULL){
  
  # get unannotated data and corresponding filehandleids
  result <- all_data %>%
    get_new_rows(curated_data = curated_data,
                 uid = uid) %>%
    sort_rows(sort_keys = sort_keys) %>%
    get_session_images(
      syn = syn,
      uid = uid,
      synapse_tbl_id = synapse_tbl_id,
      filehandle_cols = filehandle_cols,
      keep_metadata = keep_metadata,
      n_batch = n_batch,
      cache_location = cache_location) %>% 
    visualize_column_files(
      funs = visualization_funs,
      output_location = output_location) %>%
    add_input_survey_cols(survey_colnames = survey_colnames)
}