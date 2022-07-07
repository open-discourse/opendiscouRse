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
         private$con = private$connect_to_docker(docker_config)
       } else {
       }
      }
    ),
  private = list(
    con = NULL,
    #' Connect to docker
    #'
    #' @param db (`character`) as used in `dbConnect`
    #' @param host_db (`character`) as used in `dbConnect`
    #' @param db_port (`character`) as used in `dbConnect`
    #' @param db_user (`character`) as used in `dbConnect`
    #' @param db_password (`character`) as used in `dbConnect`
    #'
    #' @return dbConnect connection object
    connect_to_docker = function(db = "next",
                                 host_db = "localhost",
                                 db_port = "5432",
                                 db_user = "postgres",
                                 db_password = "postgres") {
      con <- RPostgreSQL::dbConnect(
        RPostgres::Postgres(),
        dbname = db,
        host = host_db,
        port = db_port,
        user = db_user,
        password = db_password
      )
      return(con)
    }
  )
)

