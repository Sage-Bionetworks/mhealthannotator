####################################################
#' Test script to check if get_annotation_batch is able
#' to get desirable format
####################################################


#' Function to clean up test directories
clean_test_dir <- function(){
    unlink("output", recursive = TRUE)
    unlink("cache", recursive = TRUE)
}

#' Function to create directories for caching downloaded images
#' and storing output/processed images
create_directories <- function(){
    output_dir <- "output"
    dir.create(output_dir, showWarnings = FALSE)
    
    cache_dir <- "cache"
    dir.create(cache_dir, showWarnings = FALSE)
}


#' Function to create dummy data
create_data <- function(){
    tibble::tibble(t = seq(0,30, by = 0.01)) %>%
        dplyr::mutate(x = rnorm(nrow(.)),
                      y = rnorm(nrow(.)),
                      z = rnorm(nrow(.), mean = 1))
}

#' Function to create dummy files data
create_samples <- function(n){
    purrr::map_dfr(c(1:n), function(x){
        output <- glue::glue("cache/sample_{x}.tsv", x = x)
        data <- create_data() %>% 
            readr::write_tsv(output)
        return(tibble(filePath = output))
    })
}

#' Test function for testing if function
#' is parseable 
test_funs <- function(filePath){
    output_filepath <- file.path(
        glue::glue(gsub(
            "\\.tsv$", "", 
            filePath), ".jpg"))
    plot <- fread(filePath) %>%
        ggplot(aes(x = t, y = x)) + 
        geom_line()
    ggsave(filename = output_filepath, plot = plot)
    unlink(filePath)
    return(output_filepath)
}

syn <- attempt_instantiate()
tryCatch(
    attempt_login(syn),
    error = function(e) {
        print(glue::glue("Did not log into Synapse: {e$message}"))
    }
)

ref_list <- list(
    tbl_id = "syn22281748",
    n_batch = 5,
    filehandle_cols = c("psoriasisAreaPhoto.jpg",
                        "psoriasisAreaPhoto.png"),
    uid = c("recordId"),
    cache_location = "cache",
    output_dir = "output"
)

# test data 1
test_data_1 <- tibble::tibble(uid = c("record_1", "record_2", "record_3"),
                              fileColumnName = c("dummy_1", "dummy_2", "dummy_3"))

# test data 2
test_data_2 <- tibble::tibble(uid = c("record_1", "record_2", 
                                      "record_3", "record_4"),
                              fileColumnName = c("dummy_1", "dummy_2", 
                                                 "dummy_3", "dummy_4"))

# test data 3: synapse query table
tbl <- get_source_table(syn = syn, 
                        filehandle_cols = ref_list$filehandle_cols,
                        synapse_tbl_id = ref_list$tbl_id)

tbl_prev <- tbl %>%
    slice(1:2) %>%
    dplyr::select(recordId, fileColumnName)


#' Test 1: check if get table string filter is giving the right output
test_that("test get_new_rows is subsetting the data", {
    create_directories()
    desired_data <- tibble::tibble("uid" = c("record_4"),
                                   "fileColumnName" = c("dummy_4"))
    new_data <- test_data_2 %>% 
        get_new_rows(test_data_1, "uid")
  expect_true((desired_data %>%
                  dplyr::anti_join(new_data) %>%
                  nrow()) == 0)
  clean_test_dir()
})

#' Test 2: check if get table string filter is giving the right output
test_that("test get_table_string_filters is outputting the right string format", {
    create_directories()
    desired_query_string <- "uid IN ('record_1','record_2','record_3')"
    query_string <- test_data_1 %>% get_table_string_filters("uid")  
    expect_true(query_string == desired_query_string)
    
    clean_test_dir()
})


#' Test 3: check if able to get annotation session images
test_that("test get_session_images able to download tables and store in output folder",{
    create_directories()
    
    session_data <- tbl %>% 
        get_session_images(syn = syn, 
                           synapse_tbl_id = ref_list$tbl_id, 
                           filehandle_cols = ref_list$filehandle_cols,
                           uid = ref_list$uid, 
                           n_batch = ref_list$n_batch, 
                           cache_location = ref_list$cache_location)
    
    # test if returns dataframe
    expect_true(inherits(session_data, 
                         "data.frame"))
    
    # test if returns session data
    expect_equal(names(session_data), 
                 c("recordId", "fileColumnName", "filePath"))
    
    # test if returns right size for the dataframe
    expect_equal(session_data %>% 
                     tidyr::drop_na() %>%
                     nrow(), ref_list$n_batch)
    
    clean_test_dir()
})

#' Test 4: check if get source table results to a dataframe
test_that("visualize_column_files returns desired filepath output", {
    create_directories()
    test_data <- create_samples(5) %>%
        visualize_column_files(test_funs, ref_list$output_dir)
    output_target <- purrr::map_chr(test_data$imagePath, ~basename(.x))
    output_files <- list.files(ref_list$output_dir)
    expect_equal(output_target, output_files)
    clean_test_dir()
})


#' Test 5: check if able to get annotation session images
test_that("test get_annotation_batch to annotation app data placeholder",{
    create_directories()
    data <- get_annotation_batch(syn,
                         all_data = tbl,
                         curated_data= tbl_prev,
                         synapse_tbl_id = ref_list$tbl_id,
                         filehandle_cols = ref_list$filehandle_cols,
                         cache_location = ref_list$cache_location,
                         survey_colnames =c("test_col"),
                         n_batch = 5,
                         visualization_funs = mhealthannotator::visualize_photo,
                         uid = c("recordId"),
                         output_location = "cache")
    data_remove_empty <- data %>%
        dplyr::select(-test_col, -annotationTimestamp) %>%
        tidyr::drop_na()
    
    # check if test_col for survey is in annotation placeholder
    expect_true("test_col" %in% names(data))
    
    # check if annotation timestamp in annotation placeholder
    expect_true("annotationTimestamp" %in% names(data))
    
    # check if image path in data
    expect_true("imagePath" %in% names(data))
    
    # check if data is populated
    expect_true((data_remove_empty %>% nrow()) == ref_list$n_batch)
    clean_test_dir()
})



