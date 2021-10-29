####################################################
#' Test script to check if stored annotation
#' is returned in right format
####################################################

# attempt login to synapse
syn <- attempt_instantiate()
tryCatch(
    attempt_login(syn),
    error = function(e) {
        print(glue::glue("Did not log into Synapse: {e$message}"))
    }
)

cols_for_check <- c("recordId", "participantId", 
                    "createdOn", "induration", "PGA", 
                    "erythema", "scaling",
                    "annotationTimestamp", 
                    "fileColumnName", 
                    "annotator")

test_that("get_stored_annotations return all character columns",{
  skip_if_not(logged_in(syn = syn))
  annotated <- mhealthannotator::get_stored_annotation(
      syn = syn,
      parent_id = "syn26340454",
      uid = c("recordId"),
      stored_filename = "test_annotations.tsv",
      keep_metadata = c("participantId", "createdOn"),
      survey_colnames = c("PGA", "erythema", "induration", "scaling")) %>%
      dplyr::select_if(is.character)
  empty <- mhealthannotator::get_stored_annotation(
      syn = syn,
      parent_id = "syn26340454",
      uid = c("recordId"),
      stored_filename = "empty_test_annotations.tsv",
      keep_metadata = c("participantId", "createdOn"),
      survey_colnames = c("PGA", "erythema", "induration", "scaling")) %>%
      dplyr::select_if(is.character)
  expect_true(all((annotated %>% names()) %in% cols_for_check))
  expect_true(all((empty %>% names()) %in% cols_for_check))
})


test_that("get_stored_annotations returns the right row",{
    skip_if_not(logged_in(syn = syn))
    annotated <- mhealthannotator::get_stored_annotation(
        syn = syn,
        parent_id = "syn26340454",
        uid = c("recordId"),
        stored_filename = "test_annotations.tsv",
        keep_metadata = c("participantId", "createdOn"),
        survey_colnames = c("PGA", "erythema", "induration", "scaling")) %>%
        dplyr::select_if(is.character)
    empty <- mhealthannotator::get_stored_annotation(
        syn = syn,
        parent_id = "syn26340454",
        uid = c("recordId"),
        stored_filename = "empty_test_annotations.tsv",
        keep_metadata = c("participantId", "createdOn"),
        survey_colnames = c("PGA", "erythema", "induration", "scaling")) %>%
        dplyr::select_if(is.character)
    expect_true(nrow(annotated) == 2)
    expect_true(nrow(empty) == 0)
})