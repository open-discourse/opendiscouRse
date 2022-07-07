library("RPostgreSQL")

# db_connection -----------------------------------------------------------
db <- "next"
host_db <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_password <- "postgres"
con <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = db,
    host = host_db,
    port = db_port,
    user = db_user,
    password = db_password
  )

# get data tables ---------------------------------------------------------
speeches <- dbGetQuery(con,
                       "SELECT *
                       FROM open_discourse.speeches;")

factions <- dbGetQuery(con,
                       "SELECT *
                       FROM open_discourse.factions;")

politicians <- dbGetQuery(con,
                          "SELECT *
                       FROM open_discourse.politicians;")

contributions_simplified <- dbGetQuery(con,
                                       "SELECT *
                       FROM open_discourse.contributions_simplified;")

contributions_extended <- dbGetQuery(con,
                                     "SELECT *
                       FROM open_discourse.contributions_extended;")

electoral_terms <- dbGetQuery(con,
                              "SELECT *
                       FROM open_discourse.electoral_terms;")

# store data tables for local analysis ------------------------------------
if (!dir.exists("data")) {
  dir.create("data")
  saveRDS(speeches, "data/speeches.RDS") 
} else if (!file.exists("data/speeches.RDS")) {
  saveRDS(speeches, "data/speeches.RDS") 
}

if (!dir.exists("data")) {
  dir.create("data")
  saveRDS(factions, "data/factions.RDS") 
} else if (!file.exists("data/factions.RDS")) {
  saveRDS(factions, "data/factions.RDS") 
} 

if (!dir.exists("data")) {
  dir.create("data")
  saveRDS(politicians, "data/politicians.RDS") 
} else if (!file.exists("data/politicians.RDS")) {
  saveRDS(politicians, "data/politicians.RDS") 
} 

if (!dir.exists("data")) {
  dir.create("data")
  saveRDS(contributions_simplified, "data/contributions_simplified.RDS") 
} else if (!file.exists("data/contributions_simplified.RDS")) {
  saveRDS(contributions_simplified, "data/contributions_simplified.RDS") 
} 

if (!dir.exists("data")) {
  dir.create("data")
  saveRDS(contributions_extended, "data/contributions_extended.RDS") 
} else if (!file.exists("data/contributions_extended.RDS")) {
  saveRDS(contributions_extended, "data/contributions_extended.RDS") 
} 

if (!dir.exists("data")) {
  dir.create("data")
  saveRDS(electoral_terms, "data/electoral_terms.RDS") 
} else if (!file.exists("data/electoral_terms.RDS")) {
  saveRDS(electoral_terms, "data/electoral_terms.RDS") 
} 




