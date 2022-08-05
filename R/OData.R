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
    count_data = NULL,
    #' @description constructor
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
    },
    #' Count observations by grouping variables.
    #'
    #' @param table_name (`character`) character string of the table name
    #' @param grouping_vars A character vector containing grouping variables of a data table.
    #' @param sort_n_desc A boolean value indicating whether the whole data frame should be 
    #' sorted in descending order by n. Default is `FALSE`.
    #'
    #' @return A (grouped) data frame.
    #' @import checkmate
    #' @importFrom magrittr %>%
    get_count = function(grouping_vars,
                         sort_n_desc = FALSE) {

      assert_false(
        is.null(grouping_vars)
      )

      assert(
        check_subset(
          self$table_name,
          names(private$valid_grouping_vars)
        ),
        check_subset(
          grouping_vars,
          private$valid_grouping_vars[[private$table_name]]
        )
      )

      self$count_data <- self$data %>%
        dplyr::group_by(
          across(
            {{ grouping_vars }}
          )
        ) %>%
        dplyr::count() %>%
        dplyr::ungroup()

      if (sort_n_desc == TRUE) {
        self$count_data <- self$count_data %>%
          dplyr::ungroup() %>%
          dplyr::arrange(
            dplyr::desc(
              n
            )
          )
      }
      return(self$count_data)
      invisible(self)
    }
  ),
  private = list(
    # this list defines the valid grouping variables that can be used
    # for the get_count function
    valid_grouping_vars = list(
      speeches = c("session",
        "electoral_term",
        "politician_id",
        "faction_id",
        "position_short",
        "position_long",
        "date"),
      politicians = c("birth_place",
        "birth_country",
        "gender",
        "aristocracy"),
      contributions = c("speech_id",
        "type")
    )
  )
)
