####################################################
#' Test script to check if get_source_table is able
#' to get desirable format
####################################################

syn <- attempt_instantiate()
tryCatch(
    attempt_login(syn),
    error = function(e) {
        print(glue::glue("Did not log into Synapse: {e$message}"))
    }
)

table_columns <- c("recordId", 
                   "appVersion", 
                   "externalId", 
                   "dataGroups",
                  "createdOn", 
                  "createdOnTimeZone", 
                  "participantId",
                  "selectedZoneIdentifier",
                  "fileColumnName", "fileHandleId")
file_columns_identifier <- c("psoriasisAreaPhoto.jpg","psoriasisAreaPhoto.png")

#' check if get source table results to a dataframe
test_that("get_source_table returns the right format", {
    skip_if_not(logged_in(syn = syn))
    result <- get_source_table(syn = syn, 
                               filehandle_cols = c(
                                   "psoriasisAreaPhoto.jpg",
                                   "psoriasisAreaPhoto.png"),
                               synapse_tbl_id = "syn22281748")
    expect_true(inherits(result, "data.frame"))
    expect_equal(result %>% 
                     names(.) %>%
                     sort(), 
                 table_columns %>%
                     sort())
    expect_equal(result %>% 
                     .$fileColumnName %>%
                     unique() %>%
                     sort(), 
                 file_columns_identifier %>% 
                     sort())
})
