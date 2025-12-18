library(tidyverse)
library(readxl)
library(tidytext)
library(here)
library(DT)

plot_all <- function(my_file) {
  
  ## --- Load and clean the play
  my_play <- read_play_jsonl(here("test-data", my_file))
  
  # Handle character name variations
  variants <- read_excel(here("Rolleliste.xlsx")) %>% 
    unnest_tokens(variant, Alias, token = "regex", pattern = ", ") %>% 
    mutate(across(c(Karakter, variant), tolower))
  
  my_play <- my_play %>%
    mutate(speaker = tolower(speaker)) %>%
    left_join(variants, by = c("filename" = "Filnavn", "speaker" = "variant")) %>%
    mutate(speaker = if_else(!is.na(Karakter), Karakter, speaker)) %>%
    filter(!is.na(speaker), speaker != "") %>%
    distinct()
  
  # Calculate scene index
  act_length <- my_play %>%
    group_by(act_number) %>%
    summarise(scenes = max(scene_number), .groups = "drop") %>%
    pull(scenes) %>%
    cumsum()
  
  calc_scene_index <- function(act_number, scene_number, act_length) {
    ifelse(is.numeric(scene_number),
           scene_number + ifelse(act_number == 1, 0, act_length[act_number - 1]),
           NA)
  }
  
  ## --- Prepare main summary
  my_summary <- my_play %>%
    rowwise() %>%
    mutate(scene_index = calc_scene_index(act_number, scene_number, act_length)) %>%
    ungroup() %>%
    filter(act != "", scene != "", speaker != "", !is.na(spoke)) %>%
    mutate(boolean_spoke = if_else(spoke != "", "X", "Y")) %>%
    select(act_number, scene_number, scene_index, speaker, boolean_spoke) %>%
    distinct()
  
  # Table for pivoting
  my_speakers <- my_summary %>%
    select(scene_index, speaker, boolean_spoke) %>%
    distinct() %>%
    pivot_wider(names_from = scene_index, values_from = boolean_spoke)
  
  my_speakers[is.na(my_speakers)] <- ""
  
  # Optional: export as CSV
  # write.csv(my_speakers, here(paste0("csv/plot_who_speaks_", my_file, ".csv")))
  
  ## --- Plot 1: Who speaks when
  my_title <- my_play %>% select(docTitle) %>% distinct() %>% pull(1)
  my_year  <- my_play %>% select(year) %>% distinct() %>% pull(1)
  
  my_summary <- my_summary %>%
    group_by(act_number, scene_number) %>%
    mutate(total = n_distinct(speaker)) %>%
    ungroup()
  
  p1 <- ggplot(my_summary, aes(y = speaker, x = scene_index)) +
    geom_tile(aes(fill = boolean_spoke), colour = "grey", show.legend = FALSE) + 
    geom_text(aes(label = "X")) +
    scale_fill_manual(
      name = "", labels = c("Stum (S)", "Talende (X)"),
      values = c(rgb(240/255,228/255,66/255))
    ) +
    labs(
      title = paste(my_title, my_year, my_file),
      subtitle = "Who speaks when"
    ) +
    scale_x_discrete(position = "top") +
    xlab("Act \nScene \nNumber of characters speaking in the scene") +
    ylab("Speaker") + 
    theme(axis.text.x = element_blank(), line = element_blank(), rect = element_blank()) +
    facet_grid(cols = vars(act_number, scene_number, total))
  
  ggsave(here("graphs/plots/hvem-taler-hvornaar", paste0(my_file, ".hvem_taler_hvornaar.pdf")),
         plot = p1, width = 16, height = 9)
  
  ## --- Plot 2: Who is present but silent
  source(here("src", "present_without_speech.R"))
  my_play <- read_play_jsonl(here("test-data", my_file))
  
  present_silent <- present_without_speech_numbers(my_play) %>%
    rowwise() %>%
    mutate(scene_index = calc_scene_index(act_number, scene_number, act_length)) %>%
    ungroup() %>%
    mutate(boolean_spoke = "S") %>%
    select(-index)
  
  new_summary <- my_summary %>%
    select(-total) %>%
    full_join(present_silent,
              by = c("speaker" = "word", "act_number", "scene_number", "scene_index", "boolean_spoke")) %>%
    group_by(act_number, scene_number) %>%
    mutate(total = n_distinct(speaker)) %>%
    ungroup()
  
  p2 <- ggplot(new_summary, aes(y = speaker, x = scene_index)) +
    geom_tile(aes(fill = boolean_spoke), colour = "grey", show.legend = TRUE) +
    geom_text(aes(label = boolean_spoke)) +
    scale_fill_manual(
      name = "", labels = c("Mute (S)", "Speaking (X)"),
      values = c(rgb(230/255,159/255,0), rgb(240/255,228/255,66/255))
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = paste(my_title, my_year, my_file),
      subtitle = "Who is present"
    ) +
    xlab("Act \nScene \nNumber of characters in the scene") +
    ylab("Speaker") + 
    theme(
      axis.text.x = element_blank(), line = element_blank(), rect = element_blank(),
      legend.position = "bottom"
    ) +
    facet_grid(cols = vars(act_number, scene_number, total))
  
  ggsave(here("graphs/plots/hvem-til-stede", paste0(my_file, ".hvem_til_stede.pdf")),
         plot = p2, width = 16, height = 9)
  
  ## --- Plot 3: Mentions
  source(here("src", "omtale.R"))
  spoken_about_summary <- spoken_about(my_play) %>%
    rowwise() %>%
    mutate(scene_index = calc_scene_index(act_number, scene_number, act_length)) %>%
    ungroup() %>%
    mutate(boolean_spoke = "O", total = 0) %>%
    select(-index)
  
  full_mentions <- full_join(
    new_summary %>% select(-total),
    spoken_about_summary,
    by = c("speaker" = "word", "act_number", "scene_number", "scene_index", "boolean_spoke")
  ) %>%
    mutate(boolean_spoke = case_when(
      !is.na(boolean_spoke) ~ boolean_spoke,
      TRUE ~ "O"
    )) %>%
    group_by(act_number, scene_number) %>%
    mutate(total = n_distinct(speaker)) %>%
    ungroup()
  
  p3 <- ggplot(full_mentions, aes(y = speaker, x = scene_index)) +
    geom_tile(aes(fill = boolean_spoke), colour = "grey", show.legend = TRUE) +
    geom_text(aes(label = boolean_spoke)) +
    scale_fill_manual(
      name = "", labels = c("Mentioned (O)", "Mute (S)", "Speaking (X)"),
      values = c(rgb(86/255,180/255,233/255),
                 rgb(230/255,159/255,0),
                 rgb(240/255,228/255,66/255))
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = paste(my_title, my_year, my_file),
      subtitle = "Who is present and who is mentioned"
    ) +
    xlab("Act \nScene") +
    ylab("Speaker") + 
    theme(
      axis.text.x = element_blank(), line = element_blank(), rect = element_blank(),
      legend.position = "bottom"
    ) +
    facet_grid(cols = vars(act_number, scene_number))
  
  ggsave("graphs/plots/hvem-til-stede-hvem-omtalt", paste0(my_file, ".hvem_til_stede_hvem_omtalt.pdf"),
         plot = p3, width = 16, height = 9)
}
