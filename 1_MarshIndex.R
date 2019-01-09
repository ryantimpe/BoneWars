library(tidyverse)
library(ggrepel)
library(ngramr)

# Step 1 - Get Genus & paleontologists from PBDB ----

#I'm a terible person and downloaded more data than necessary from PBDB
pbdb_data <- read_csv("data/pbdb_data.csv", 
                      skip = 21)

#I don't care about fishes and spineless lifeforms
keep_class <- c("Aves", "Mammalia", "Ornithischia", "Reptilia", "Saurischia")

bw_data <- pbdb_data %>% 
  filter(accepted_rank == "genus", is_extant == "extinct", phylum=="Chordata") %>% 
  filter(class %in% keep_class) %>% 
  drop_na(genus, ref_author) %>% 
  select(class, genus, ref_author, ref_pubyr) %>% 
  distinct()

genus_author <- bw_data %>% 
  #Limit the scope to cool paleontologists... I probably forgot some
  filter(ref_author %in% c("Marsh", "Cope", "Osborn", "Ostrom", "Brown", "Owen", "Mantell") |
           str_detect(ref_author, "Meyer") & ref_pubyr < 1900) %>% 
  mutate(ref_author = ifelse(str_detect(ref_author, "Meyer"), "von Meyer", ref_author))

# Counts by Class
genus_author %>% 
  count(ref_author, class) %>% 
  spread(ref_author, n)

# Google NGrams of Genus names ----

bw_genus <- genus_author %>% 
  pull(genus) 

#Collect ngrams in groups
#.... might need to break this up so the server doesn't lock you out!
# I broke this into 5 sets of 5, then boudn them later. Had to wait ~15 minutes between each run
ng_genus5 <- seq(1, length(bw_genus), 12)[21:24] %>% 
  purrr::map_df(function(i, genus){
      get_ngrams <- bw_genus[i:(i+11)]
      get_ngrams <- get_ngrams[!is.na(get_ngrams)]
      
      ngram(get_ngrams, year_start = 1950, 
            smoothing = 0, count = TRUE, case_ins = TRUE)
    }, bw_genus)

ng_genus <- list(ng_genus1, ng_genus2, ng_genus3, ng_genus4, ng_genus5) %>% 
  bind_rows() %>% 
  mutate(genus = tolower(Phrase)) %>% 
  group_by(Year, genus) %>% 
  summarize_at(vars(Count, Frequency), sum, na.rm=TRUE) %>% 
  ungroup()


# Bone Wars Index - All Classes ----

marsh_index <- ng_genus %>% 
  left_join(genus_author %>% mutate(genus = tolower(genus)), by = "genus") %>% 
  count(ref_author, Year, wt=Count) %>% 
  group_by(ref_author) %>% 
  mutate(Count_cum = cumsum(n)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(index = n / sum(n),
         index_cum = Count_cum / sum(Count_cum)) %>%
  ungroup() %>% 
  group_by(ref_author) %>% 
  mutate(index_ma3 = (1/3)*(index + lag(index, 1) + lag(index,2))) %>% 
  ungroup()


marsh_index %>% 
  ggplot(aes(x = Year, y = index_cum, group = ref_author)) +
  geom_line(aes(color = ref_author, size = ref_author), alpha = 0.85) +
  scale_color_manual(values = c("Marsh" = "#00436b", "Cope" = "#ff6141", "Ostrom" = "#ff25ab", 
                                "Mantell" = "#ff9e53",
                                "Osborn" = "#2dd49c", "Owen" = "#ff4040", 
                                "von Meyer" = "#5384ff", 
                                "Brown" = "#9affd0"),
                     name = "Paleontologist") +
  scale_size_manual(values = c("Marsh" = 2, "Cope" = 2, "Ostrom" = 1, 
                               "Mantell" = 1, "Osborn" = 2, "Owen" = 1, 
                               "von Meyer" = 1, "Brown" = 1), guide = FALSE) +
  geom_text(data = marsh_index %>% filter(Year == 2008, ref_author %in% c("Marsh", "Cope", "Osborn")),
                  aes(x = 2009, y = index_cum, label = ref_author),
                  size = 4, hjust = 0) +
  labs(title = "Who won the Bone Wars?",
       subtitle = paste0("Legacy of paleontogolists using Google Ngrams of discovered genera", "\n",
                         "Class: ", paste(keep_class, collapse = ", ")),
       caption = "Data: Google Ngrams & paleobiodb.org\n
       Steps: (1) Collect named genera of each paleontogolist from paleobiodb.org. (2) Collect frequency of mentions of each genus in Google Ngrams. 
       (3) Sum over frequencies for each paleontologist by year, cumulate over year, divide each year by total cumulative count.\n
       @ Ryan Timpe .com",
       y = "Legacy Index\n(normalized cumulative genus frequency in literature)", 
       x= NULL) +
  coord_cartesian(xlim = c(1950, 2018), ylim = c(0, 0.7), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "#ebdcbb"),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold", size = 8),
    plot.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 6),
    panel.background = element_rect(fill = "#fcedcc"),
    legend.position = "bottom"
  )

ggsave("Output/MarshIndex_all.png", device = "png",
       width = 6, height = 6, units = "in")


# Bone Wars Index - Dinosaurs Only----
dino_class <- c("Ornithischia", "Saurischia")

marsh_index_dinos <- ng_genus %>% 
  left_join(genus_author %>% mutate(genus = tolower(genus)), by = "genus") %>% 
  filter(class %in% dino_class) %>% 
  count(ref_author, Year, wt=Count) %>% 
  group_by(ref_author) %>% 
  mutate(Count_cum = cumsum(n)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(index = n / sum(n),
         index_cum = Count_cum / sum(Count_cum)) %>%
  ungroup() %>% 
  group_by(ref_author) %>% 
  mutate(index_ma3 = (1/3)*(index + lag(index, 1) + lag(index,2))) %>% 
  ungroup()


marsh_index_dinos %>% 
  ggplot(aes(x = Year, y = index_cum, group = ref_author)) +
  geom_vline(aes(xintercept=1990), size = 1.25, color = "#666666") +
  annotate("text", x=1991, y=0.6, label = "Jurassic Park\n(novel)", hjust = 0) +
  geom_line(aes(color = ref_author, size = ref_author), alpha = 0.85) +
  scale_color_manual(values = c("Marsh" = "#00436b", "Cope" = "#ff6141", "Ostrom" = "#ff25ab", 
                                "Mantell" = "#ff9e53",
                                "Osborn" = "#2dd49c", "Owen" = "#ff4040", 
                                "von Meyer" = "#5384ff", 
                                "Brown" = "#9affd0"),
                     name = "Paleontologist") +
  scale_size_manual(values = c("Marsh" = 2, "Cope" = 2, "Ostrom" = 1, 
                               "Mantell" = 1, "Osborn" = 2, "Owen" = 1, 
                               "von Meyer" = 1, "Brown" = 1), guide = FALSE) +
  geom_text(data = marsh_index_dinos %>%
              filter(Year == 2008, ref_author %in% c("Marsh", "Cope", "Osborn")) %>%
              mutate(ref_author = ifelse(ref_author == "Osborn", "Osborn\n(T. rex)", ref_author)),
            aes(x = 2009, y = index_cum, label = ref_author),
            size = 4, hjust = 0) +
  labs(title = "Who won the Bone Wars? Dinosaur Edition",
       subtitle = paste0("Legacy of paleontogolists using Google Ngrams of discovered genera", "\n",
                         "Class: ", paste(dino_class, collapse = ", ")),
       caption = "Data: Google Ngrams & paleobiodb.org\n
       Steps: (1) Collect named genera of each paleontogolist from paleobiodb.org. (2) Collect frequency of mentions of each genus in Google Ngrams. 
       (3) Sum over frequencies for each paleontologist by year, cumulate over year, divide each year by total cumulative count.\n
       @ Ryan Timpe .com",
       y = "Legacy Index\n(normalized cumulative genus frequency in literature)", 
       x= NULL) +
  coord_cartesian(xlim = c(1950, 2018), ylim = c(0, 0.7), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "#ebdcbb"),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold", size = 8),
    plot.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 6),
    panel.background = element_rect(fill = "#fcedcc"),
    legend.position = "bottom"
  )

ggsave("Output/MarshIndex_dinos.png", device = "png",
       width = 6, height = 6, units = "in")

# Bone Wars Index - Mammals  Only----
mammal_class <- c("Mammalia")

marsh_index_mammal <- ng_genus %>% 
  left_join(genus_author %>% mutate(genus = tolower(genus)), by = "genus") %>% 
  filter(class %in% mammal_class) %>% 
  count(ref_author, Year, wt=Count) %>% 
  group_by(ref_author) %>% 
  mutate(Count_cum = cumsum(n)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(index = n / sum(n),
         index_cum = Count_cum / sum(Count_cum)) %>%
  ungroup() %>% 
  group_by(ref_author) %>% 
  mutate(index_ma3 = (1/3)*(index + lag(index, 1) + lag(index,2))) %>% 
  ungroup()


marsh_index_mammal %>% 
  ggplot(aes(x = Year, y = index_cum, group = ref_author)) +
  geom_line(aes(color = ref_author, size = ref_author), alpha = 0.85) +
  scale_color_manual(values = c("Marsh" = "#00436b", "Cope" = "#ff6141", "Ostrom" = "#ff25ab", 
                                "Mantell" = "#ff9e53",
                                "Osborn" = "#2dd49c", "Owen" = "#ff4040", 
                                "von Meyer" = "#5384ff", 
                                "Brown" = "#9affd0"),
                     name = "Paleontologist") +
  scale_size_manual(values = c("Marsh" = 2, "Cope" = 2, "Ostrom" = 1, 
                               "Mantell" = 1, "Osborn" = 2, "Owen" = 1, 
                               "von Meyer" = 1, "Brown" = 1), guide = FALSE) +
  geom_text(data = marsh_index_mammal %>% filter(Year == 2008, ref_author %in% c("Marsh", "Cope", "Osborn")),
            aes(x = 2009, y = index_cum, label = ref_author),
            size = 4, hjust = 0) +
  labs(title = "Who won the Bone Wars? Mammal Edition",
       subtitle = paste0("Legacy of paleontogolists using Google Ngrams of discovered genera", "\n",
                         "Class: ", paste(mammal_class, collapse = ", ")),
       caption = "Data: Google Ngrams & paleobiodb.org\n
       Steps: (1) Collect named genera of each paleontogolist from paleobiodb.org. (2) Collect frequency of mentions of each genus in Google Ngrams. 
       (3) Sum over frequencies for each paleontologist by year, cumulate over year, divide each year by total cumulative count.\n
       @ Ryan Timpe .com",
       y = "Legacy Index\n(normalized cumulative genus frequency in literature)", 
       x= NULL) +
  coord_cartesian(xlim = c(1950, 2018), ylim = c(0, 0.7), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "#ebdcbb"),
    strip.background = element_rect(fill = "#00436b"),
    strip.text = element_text(color = "white", face = "bold", size = 8),
    plot.background = element_rect(fill = "#fcedcc"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 6),
    panel.background = element_rect(fill = "#fcedcc"),
    legend.position = "bottom"
  )

ggsave("Output/MarshIndex_mammals.png", device = "png",
       width = 6, height = 6, units = "in")

