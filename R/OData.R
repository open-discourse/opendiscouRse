#' Open Discourse Data Object Class
#' @description Data object class. Provides functionality like retrieving data
#'  or summary functions.
#' @field table_name (`character`) character string of the table name
#' @field data (`dataframe`) data frame of the retrieved table
#'
#'
#' @export
OData <- R6::R6Class(
  "OpenDisCourse Data Object",
  public = list(
    table_name = NULL,
    data = NULL,
    #' Initialize OData Object
    #'
    #' @param table_name (`character`) character string of the table name
    initialize = function(table_name = "factions") {
      stopifnot(is.character(table_name))

      self$table_name <- table_name
      },
    #' Get Data Table
    #' @description Retrieve Data from OD DB and keep it as data
    #' @param table_name (`character`) character string of the table name
    get_table_data = function(table_name = self$table_name) {

      con <- self$get_con()

      base_query <- DBI::sqlInterpolate(con,
                                "SELECT * FROM open_discourse.?table;",
                                table = DBI::SQL(table_name))

      self$data <- RPostgres::dbGetQuery(con, base_query)

      return(self)
    },
    #' Create db connection from OD docker defaults
    #'
    #' @param db (`character` character string of the connection arg)
    #' @param host_db (`character` character string of the connection arg)
    #' @param db_port (`character` character string of the connection arg)
    #' @param db_user (`character` character string of the connection arg)
    #' @param db_password (`character` character string of the connection arg)
    #'
    #' @return RPostgres conn object
    get_con = function(db = "next",
                       host_db = "localhost",
                       db_port = "5432",
                       db_user = "postgres",
                       db_password = "postgres") {
        con <- RPostgres::dbConnect(
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
