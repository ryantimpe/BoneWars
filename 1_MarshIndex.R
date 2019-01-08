library(tidyverse)
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

bonewars <- bw_data %>% 
  #Limit the scope to cool paleontologists... I probably forgot some
  filter(ref_author %in% c("Marsh", "Cope", "Osborn", "Ostrom", "Brown", "Owen", "Mantell") |
           str_detect(ref_author, "Meyer") & ref_pubyr < 1900) %>% 
  mutate(ref_author = ifelse(str_detect(ref_author, "Meyer"), "von Meyer", ref_author))

# Counts by Class
bonewars %>% 
  count(ref_author, class) %>% 
  spread(ref_author, n)

# Google NGrams of Genus names ----

bw_genus <- bonewars %>% 
  pull(genus) 

#Collect ngrams in groups
#.... might need to break this up so the server doesn't lock you out!
ng_genus1 <- seq(1, length(bw_genus), 12)[1:5] %>% 
  purrr::map_df(function(i, genus){
      get_ngrams <- tolower(bw_genus[i:(i+11)])
      get_ngrams <- get_ngrams[!is.na(get_ngrams)]
      
      ngram(get_ngrams, year_start = 1950)
    }, bw_genus)

