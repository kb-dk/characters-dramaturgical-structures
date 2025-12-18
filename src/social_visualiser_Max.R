percentage_plot_status <- function(group1=c(), group2=c(), group3=c(), group4=c(), group5=c(), group6=c(), group7=c(), group8=c(), group9=c(), group10=c(), group11=c(), group12=c(),
                                   color1=NA, color2=NA, color3=NA, color4=NA, color5=NA, color6=NA, color7=NA, color8=NA, color9=NA, color10=NA, color11=NA, color12=NA) {
  
  # COMBINE SPEAKERS AND COLORS INTO VECTORS
  my_speakers <- c(group1, group2, group3, group4, group5, group6, group7, group8, group9, group10, group11, group12)
  my_colors <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9, color10, color11, color12) %>% na.omit()
  
  # CHECK INPUT, allow function to work without selected characters
  if (length(my_speakers) > 0) {
    stopifnot(my_speakers %in% allplays$speaker)
  }
  
  # PREPARE A DATA FRAME FOR PLOTTING
  df <- allplays %>% 
    filter(act != "", scene != "", speaker != "", !is.na(speaker), !is.na(spoke)) %>%
    mutate(n_spoken_words = str_count(spoke, '\\w+')) %>%
    group_by(year, title_year, act_number, scene_number, speaker, social_status) %>%
    summarise(words = sum(n_spoken_words), .groups = "drop") %>%
    mutate(social_status = ifelse(is.na(social_status) | social_status == "", "Unknown", social_status))
  
  # Find scene intercepts for vertical lines in plot
  intercepts <- df %>%
    group_by(act_number) %>%
    summarise(max_scene = max(scene_number), .groups = "drop") %>%
    mutate(intercepts = cumsum(max_scene + c(0.5, rep(0, length(act_number) - 1)))) %>%
    pull(intercepts)
  
  # Make order of plays chronological
  play_chronology <- unique(df$title_year)
  df$title_year <- factor(df$title_year, levels = play_chronology)
  
  # Calculate percentage of words spoken in each scene
  df <- df %>%
    group_by(title_year) %>%
    mutate(percent = 100 * (words / sum(words))) %>%
    ungroup() %>%
    mutate(act_scene = paste0(act_number, ":", str_pad(scene_number, 2, pad = "0")))
  
  # Generate readable group labels
  for (i in 1:12) {
    text <- paste(eval(parse(text = paste0("group", i))), collapse = ", ")
    assign(paste0("group", i, "text"), text)
  }
  
  # Assign initial group by social status
  df$group <- df$social_status
  
  # Overwrite group label for selected characters
  for (i in 1:12) {
    character_group <- eval(parse(text = paste0("group", i)))
    group_label <- eval(parse(text = paste0("group", i, "text")))
    if (group_label != "") {
      df$group <- ifelse(df$speaker %in% character_group, group_label, df$group)
    }
  }
  
  # Get the group labels used
  groups_used <- c(group12text, group11text, group10text, group9text, group8text, group7text,
                   group6text, group5text, group4text, group3text, group2text, group1text) %>%
    as_tibble() %>%
    filter(value != "") %>%
    pull(value)
  
  statuses_used <- unique(df$group[!df$group %in% groups_used])
  df$group <- factor(df$group, levels = c(statuses_used, groups_used))
  
  # Aggregate character groups or statuses by scene
  df <- df %>%
    group_by(title_year, act_scene, group) %>%
    summarise(percent_sum = sum(percent), .groups = "drop")
  
  # PLOTTING
  p <- ggplot(df, aes(fill = group, y = percent_sum, x = act_scene)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      breaks = c(groups_used[length(groups_used):1], statuses_used),
      values = c(my_colors, qualitative_hcl(length(statuses_used), palette = "Set 3"))
    ) +
    xlab("Act:Scene") +
    ylab("Percentage of spoken words") +
    facet_grid(rows = vars("title_year" = title_year), switch = "x", scales = "free_y") +
    geom_vline(xintercept = intercepts, size = 0.2) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 90, size = 6, hjust = 0, vjust = 0.5),
      axis.text.y = element_text(size = 4),
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
  
  return(p)
}
