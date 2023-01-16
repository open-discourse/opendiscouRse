library(tidyverse)
library(lubridate)
library(magrittr)
library(grid)
library(ggtext)
library(scales)
source("src/plots/services/od_plot_theme.R")
source("src/plots/services/finalise_plot.R")


#number of actual protocols existing per legislative period
n_protocols_true <- c(282, 227, 168, 198, 247, 199, 259, 230, 142, 256, 236, 243, 248, 253, 187, 233, 253, 245, 171)

#number of protocols covered by GermaParl
germa_parl <- list.files("data/germaparl_xml/") %>% as_tibble()

germa_parl %<>% mutate(period = substr(.$value, 4,5))

germa_parl <- germa_parl %>%
  group_by(period) %>%
  summarise(germaparl_n_protocols = n()) %>%
  mutate(period = as.numeric(period))

#number of protocols covered by ParlSpeech

parlspeech <- readRDS("data/parlspeech/Corp_Bundestag_V2.rds")
parlspeech$date <- date(as.POSIXct(parlspeech$date, origin = "1970-01-01"))

#contstruct lookup table for WPs
wp_lookup <- speeches %>%
  select(date, wp) %>%
  distinct()

parlspeech %<>% mutate(date = as.Date(date),
                       year = year(date))
parlspeech %<>%
  distinct(date) %>%
  left_join(wp_lookup, by = "date")

parlspeech[which(parlspeech$date %in% as.Date(
  c(
    "1999-02-24",
    "2001-02-16",
    "2003-02-20")
)),"wp"] <- c(14, 14, 15)

parlspeech %<>%
  group_by(wp) %>%
  summarise(parlspeech_protocol_n = n())


# plot --------------------------------------------------------------------
table_1 <- read_csv("data/table_1.csv")
true_protocols_bg <- table_1[, c("wp", "real_protocol_n")] %>% ungroup() %>% mutate(period = as.numeric(wp))

table_1 %<>% select(Wahlperiode = wp,
                    `Open Discourse` = protocols,
                    GermaParl = germaparl_n_protocols,
                    ParlSpeech = parlspeech_protocol_n)


plot <- table_1 %>%
  ungroup() %>%
  pivot_longer(cols = c("Open Discourse","GermaParl", "ParlSpeech"),
               names_to = "Korpus",
               values_to = "number_protocols") %>%
  select(Wahlperiode, Korpus, number_protocols) %>%
  mutate(Wahlperiode = as.numeric(Wahlperiode),
         Korpus = factor(Korpus, levels = c("Open Discourse", "ParlSpeech", "GermaParl"))) %>%
  ggplot(aes(x = Wahlperiode, y = number_protocols, fill = Korpus)) +
  geom_histogram(data = true_protocols_bg,
                 mapping = aes(x = period, y = real_protocol_n), fill = "grey",
                 stat = "identity",
                 alpha = 1) +
  geom_histogram(stat = "identity", alpha = 1) +
  labs(
    title = "Umfang verschiedener Plenarprotokoll-Korpora",
    subtitle = "Abgebildet ist die Abdeckung der verfügbaren Protokolle pro Wahlperiode. Dargstellt sind <b style='color:#fecc00'> Open Discourse </b>, <b style='color:#a2cd5a'>ParlSpeech</b> und <b style='color:#00b2ee'>GermaParl</b>.") +
  facet_grid(rows = vars(Korpus)) +
  scale_fill_manual(values = c("#fecc00", "darkolivegreen3", "deepskyblue3")) +
  od_style() +
  theme(strip.text.y = element_blank())

finalise_plot(plot,
              source_name = "Quelle: Plenarprotokolle des deutschen Bundestages - aufbereitet durch @OpenDiscourseDE",
              save_filepath = "src/plots/test_plots/corpora_comparision_twitter_nov2020.png",
              bg_path = "src/plots/backgrounds/bg_light8.png",
              logo_image_path = "data/od_logo_cropped.png")
