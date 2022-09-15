library(tidyverse)

# source("scripts/get_data2.R") # make sure to have the docker container running

# create function to store test data files in right folder
create_test_file <- function(df) {
  write_csv(
    x = df,
    file = paste0(
      "tests/testthat/data/",
      deparse(
        substitute(
          df
        )
      ),
      ".csv"
    )
  )
}

# keep test data file sizes smaller than 1 mb

# test data set just includes most recent electoral term and excludes data heavy columns
test_speeches <- speeches %>%
  select(-c(document_url, speech_content, search_speech_content)) %>%
  sample_frac(0.01)
  # filter(electoral_term == max(electoral_term))

create_test_file(test_speeches)

# no need to reduce politicians test file
create_test_file(politicians)
