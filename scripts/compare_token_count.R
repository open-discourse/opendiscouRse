library(tidyverse)
library(tidytext)
library(quanteda)
library(microbenchmark)

# have docker container running
devtools::load_all()

od_obj <- OpenDiscourse$new()
speeches <- od_obj$get_data("speeches")$data

set.seed(42)
speeches_sample <- speeches %>%
  sample_frac(0.001)

# count tokens
# tokens_count_tidytext <- speeches %>%
#   tidytext::unnest_tokens(word, speech_content) %>%
#   dplyr::group_by(electoral_term) %>%
#   dplyr::count() %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(cum_sum_n = cumsum(n)) %>%
#   dplyr::rename(
#     `Tokens Count` = n,
#     `Cumulated Tokens Count` = cum_sum_n
#   )
#
# tokens_count_quanteda <- speeches %>%
#   mutate(n_words = quanteda::ntoken(speech_content, remove_punct = T)) %>%
#   dplyr::group_by(electoral_term) %>%
#   dplyr::summarise(n = sum(n_words)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(cum_sum_n = cumsum(n)) %>%
#   dplyr::rename(
#     `Tokens Count` = n,
#     `Cumulated Tokens Count` = cum_sum_n
#   )
#
tokens_count_stringr <- speeches %>%
  dplyr::mutate(
    # n_tokens_s = str_count(speech_content, "\\S+"),
    n_tokens_w = str_count(speech_content, "\\W+")
  ) %>%
  dplyr::group_by(electoral_term) %>%
  dplyr::summarise(
    # n_s = sum(n_tokens_s),
    n = sum(n_tokens_w)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cum_sum_n = cumsum(n)) %>%
  dplyr::rename(
    `Tokens Count` = n,
    `Cumulated Tokens Count` = cum_sum_n
  )

# count sample tokens
tokens_sample_count_tidytext <- speeches_sample %>%
  tidytext::unnest_tokens(word, speech_content) %>%
  dplyr::group_by(electoral_term) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cum_sum_n = cumsum(n)) %>%
  dplyr::rename(
    `Tokens Count` = n,
    `Cumulated Tokens Count` = cum_sum_n
  )


tokens_sample_count_quanteda <- speeches_sample %>%
  mutate(n_words = quanteda::ntoken(speech_content, remove_punct = T)) %>%
  dplyr::group_by(electoral_term) %>%
  dplyr::summarise(n = sum(n_words)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cum_sum_n = cumsum(n)) %>%
  dplyr::rename(
    `Tokens Count` = n,
    `Cumulated Tokens Count` = cum_sum_n
  )

tokens_sample_count_stringr <- speeches_sample %>%
  dplyr::mutate(
    # n_tokens_s = str_count(speech_content, "\\S+"),
    n_tokens_w = str_count(speech_content, "\\W+")
  ) %>%
  dplyr::group_by(electoral_term) %>%
  dplyr::summarise(
    # n_s = sum(n_tokens_s),
    n = sum(n_tokens_w)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cum_sum_n = cumsum(n)) %>%
  dplyr::rename(
    `Tokens Count` = n,
    `Cumulated Tokens Count` = cum_sum_n
  )

mbm <- microbenchmark(
  tidytext = speeches_sample %>%
    tidytext::unnest_tokens(word, speech_content) %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Tokens Count` = n,
      `Cumulated Tokens Count` = cum_sum_n
    ),
  quanteda = speeches_sample %>%
    mutate(n_words = quanteda::ntoken(speech_content, remove_punct = T)) %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::summarise(n = sum(n_words)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Tokens Count` = n,
      `Cumulated Tokens Count` = cum_sum_n
    ),
  stringr = speeches_sample %>%
    dplyr::mutate(
      # n_tokens_s = str_count(speech_content, "\\S+"),
      n_tokens_w = str_count(speech_content, "\\W+")
    ) %>%
    dplyr::group_by(electoral_term) %>%
    dplyr::summarise(
      # n_s = sum(n_tokens_s),
      n = sum(n_tokens_w)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cum_sum_n = cumsum(n)) %>%
    dplyr::rename(
      `Tokens Count` = n,
      `Cumulated Tokens Count` = cum_sum_n
    ),
  times = 42
)

mbm

plot(mbm)
