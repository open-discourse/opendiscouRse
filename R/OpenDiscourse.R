#' Open Discourse Connector
#'
#'
#' @export
OpenDiscourse <- R6::R6Class(
  "OpenDiscourse",
  public = list(
    #' @description constructor
    #' @param docker_config (`list`) docker config as list
    #' @param dataverse (`logical`) Whether to retrieve data from the harvard
    #' data verse
    initialize =  function(
      docker_config = NULL,
      dataverse = FALSE) {

      if(is.null(docker_config)) {
         print("No Docker config provided. Default is used.")
       } else if(dataverse == FALSE) {
        print("Not yet implemented")
       }
      },
    #' Get Data from Database
    #'
    #' @param table_name (`character`) character string of the table name
    #'
    #' @return OData object
    #' @export
    get_data = function(table_name = "factions") {

      odata_obj <- OData$new(table_name)

      od_data <- odata_obj$get_table_data()
      return(od_data)
      }
    )
)

