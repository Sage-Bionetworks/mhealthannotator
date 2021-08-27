#' Function to store survey inputs from shiny app
#' 
#' @param data data of annotations
#' @param curr_index current index marker
#' @param user_inputs input given by users
#' @param keep_metadata what metadata to keep
#' @param uid what are the unique ids (recordId, healthcode, participantId etc.)
#'
#' @export
#'
#' @return dataframe of annotation based on current index
store_inputs <- function(data, 
                         curr_index,
                         user_inputs, 
                         uid,
                         keep_metadata = NULL){
  tryCatch({
    curr_fileColumnName <- data$fileColumnName[curr_index]
    user_input_data <- user_inputs %>%
      tibble::enframe(.) %>% 
      dplyr::mutate(value = unlist(value)) %>%
      tidyr::pivot_wider(name)
    row_data <- purrr::map(uid, function(identifier){
      curr_uid <- data[[identifier]][curr_index]
      tibble(!!sym(identifier) := curr_uid)}) %>% 
      purrr::reduce(dplyr::bind_cols) %>%
      dplyr::mutate(
        annotationTimestamp = as.character(lubridate::now()),
        fileColumnName = curr_fileColumnName) %>%
      dplyr::bind_cols(user_input_data)
    data %>% 
      dplyr::mutate_all(as.character) %>%
      dplyr::rows_update(
        row_data, 
        by = c((uid), 
               "fileColumnName")) %>%
      dplyr::select(all_of(uid), 
                    all_of(keep_metadata), 
                    all_of(names(user_inputs)),
                    fileColumnName, 
                    imagePath, 
                    annotationTimestamp)
  }, error = function(e){data})
}
