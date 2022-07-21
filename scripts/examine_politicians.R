library(tidyverse)
library(ggridges)
library(lubridate)

# get data ----------------------------------------------------------------
politicians <- readRDS("data/politicians.RDS")

politicians %>%
  glimpse()

# n politicians -- more than at speeches data table
politicians %>%
  select(id) %>%
  distinct %>%
  count

# profession
politicians %>%
  mutate(n_comma = str_count(profession, ",")) %>%
  summarise(max(n_comma, na.rm = T)) %>%
  pull + 1 -> max_n_jobs

# for (i in 1:max_n_jobs) {
#   print(paste0("profession_", i))
# }

# map_chr(1:max_n_jobs, ~ paste0("profession_", .x))

politicians %$% death_date %>% class

professions_long <- politicians %>%
  separate(profession, into = map_chr(1:max_n_jobs, ~ paste0("profession_", .x)), sep = ",") %>%
  pivot_longer(cols = starts_with("profession"), names_to = "profession_nr", values_to = "profession_value") %>%
  mutate(
    age = if_else(
      !is.na(death_date),
      (death_date - birth_date) %>% as.numeric / 365,
      (Sys.Date() - birth_date) %>% as.numeric / 365
      # difftime(death_date, birth_date, units = "days") %>% as.numeric / 365,
      # difftime(Sys.Date(), birth_date, units = "days") %>% as.numeric / 365,
    )
  ) %>%
  ungroup



# most famous professions -- too long for paper --> online appendix e.g. with shiny app?
professions_long %>%
  group_by(profession_value) %>%
  count() %>%
  na.omit() %>%
  arrange(desc(n))

# check whether there is no duplicate of profession per MdB (not exceeding n = 1)
professions_long %>%
  group_by(id, profession_value) %>%
  na.omit %>%
  count %>%
  arrange(desc(n))

top_10_professions <- professions_long %>%
  group_by(profession_value) %>%
  count() %>%
  na.omit() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(10) %>%
  # mutate(profession_value = fct_inorder(profession_value)) %>%
  pull(profession_value)

top_10_professions

top_20_professions <- professions_long %>%
  group_by(profession_value) %>%
  count() %>%
  na.omit() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(20) %>%
  # mutate(profession_value = fct_inorder(profession_value)) %>%
  pull(profession_value)

top_20_professions

# speeches data for most recent electoral term ----------------------------
speeches <- readRDS("data/speeches.RDS")

# speeches_recent_electoral_term <- speeches %>%
#   filter(electoral_term == max(electoral_term, na.rm = T))
#
# speeches_recent_electoral_term %>%
#   distinct(id) %>%
#   pull -> politicians_id_lp_20

professions_long %$% id %>% class

# id varies between across different data tables?
politicians_id_lp_fun <- function(df, lp_num) {
  df %>%
    filter(electoral_term == lp_num) %>%
    distinct(id) %>%
    mutate(id = id %>% as.character %>% paste0("1", .)) %>%
    pull
}

politicians_id_lp_19 <- politicians_id_lp_fun(speeches, 19)
class(politicians_id_lp_19)

politicians_id_lp_20 <- politicians_id_lp_fun(speeches, 20)
class(politicians_id_lp_20)

professions_long %>%
  filter(id %>% as.character %in% politicians_id_lp_19)

professions_long %>%
  filter(id %>% as.character %in% politicians_id_lp_20)


# age distribution (actual age of MdBs from LP 19, recalculation of starting date is more appropriate) by top professions
professions_long %>%
  # filter(id %>% as.character %in% politicians_id_lp_19) %>%
  filter(profession_value %in% top_10_professions) %>%
  ggplot(aes(x = profession_value, y = age)) +
  geom_boxplot()

professions_long %>%
  filter(id %>% as.character %in% politicians_id_lp_19) %>%
  filter(profession_value %in% top_10_professions) %>%
  mutate(profession_value = profession_value %>% fct_infreq %>% fct_rev) %>%
  ggplot(aes(x = age, y = profession_value)) +
  # geom_density_ridges()
  stat_density_ridges(quantile_lines = T)


# age of first speech per electoral term
speeches %>%
  select(- speech_content) %>%
  group_by(electoral_term, politician_id) %>%
  filter(date == min(date)) %>%
  distinct(politician_id, electoral_term, .keep_all = T) %>%
  left_join(politicians, by = c("politician_id" = "id")) %>%
  mutate(age_first_speech_lp = get_age_hist(date, birth_date) %>% as.numeric) %>%
  ungroup() -> test_df

test_df %$% age_first_speech_lp %>% class()

test_df %>%
  ggplot(aes(x = age_first_speech_lp, y = as.factor(electoral_term))) +
  # geom_boxplot()
  ggridges::geom_density_ridges() +
  theme_od()

plot_dist(test_df, test_df$age_first_speech_lp, test_df$electoral_term)

plot_dist(test_df, age_first_speech_lp, electoral_term, "ridge_plot") %>% class()





