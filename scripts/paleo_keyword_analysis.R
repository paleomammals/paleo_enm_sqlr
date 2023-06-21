#Search conditions:
#TITLE-ABS-KEY ( ( "paleo*"  AND  "niche" )  OR  ( "palaeo*"  AND  "niche" )  OR  ( "hindcasting"  AND  "niche" )  OR  ( "fossil"  AND  "niche" )  OR  ( "paleo"  AND  "enm" )  OR  ( "palaeo"  AND  "enm" )  OR  ( "fossil"  AND  "enm" )  OR  ( "fossil"  AND  "sdm" )  OR  ( "deep"  AND  "time"  AND  "enm" )  OR  ( "deep"  AND  "time"  AND  "sdm" ) )  AND  SUBJAREA ( envi )  AND  ( LIMIT-TO ( PUBSTAGE ,  "final" ) )  AND  ( LIMIT-TO ( DOCTYPE ,  "ar" )  OR  LIMIT-TO ( DOCTYPE ,  "ch" ) )  AND  ( LIMIT-TO ( SUBJAREA ,  "ENVI" )  OR  LIMIT-TO ( SUBJAREA ,  "AGRI" )  OR  LIMIT-TO ( SUBJAREA ,  "EART" ) )  AND  ( LIMIT-TO ( LANGUAGE ,  "English" ) )

# Libraries ---------------------------------------------------------------
#Data manipulation
library(janitor)
library(dplyr) 
library(tidyr)

#Corpus cleaning
library(tidytext)

#Keyword modelling
library(akc)

#Geospatial study distribution
library(sf)

#visualisation
library(ggplot2)
library(viridis)


# Reading in data ---------------------------------------------------------
#ARTICLES DOWNLOADED ON 09/06/2023 (11:00 AM EST)

#envi subject area of scopus
paleo_df <- read.csv("./data/edited/paleo_scopus_edit_2023_06_19.csv") %>% 
  #Tidying column names for convenience
  clean_names() %>% 
  filter(relevant)

# #After a quick check, there does not appear to be any double up articles
# paleo_df %>% 
#   distinct(doi)
# #Yields 675 - however, there are 29 articles that have no DOI associated with them
# filter(paleo_df, doi == "") %>% 
#   select(title)

# Keyword analysis ----------------------------------------

# Stop words
data(stop_words) #stop words to be removed from scopus database

#Extra words that André chose on the basis of repeat groupings...
excluded_words <- data.frame(word = c("model", "models",  "modelling", "modeling", "deep", "result", "method", "study", "factor", "index", "base", "analysis",  "data set", "paleoecology", "palaeoecology", "paleontology", "china", "fossil", "fossils", "fossil record", "niche", "article", "book", "united states", "ecology", "ecological modeling", "ecological modelling", "france", "north america", "europe", "eurasia", "fossil fuel", "fossil fuels", "carbon dioxide", "ancient dna", "dna", "atlantic ocean ", "atlantic ocean", "africa", "australia", "iberian peninsula", "mediterranean sea"),
                             lexicon = "Andre") %>% 
  bind_rows(stop_words, .) %>% 
  mutate(word = toupper(word))

#Keyword data cleaning
paleo_kw_df <- paleo_df %>%
  #Using index_keywords as these are less varied than author ones
  select(c(title, year, index_keywords)) %>% 
  tibble() %>% 
  #Cleaning and separating the keywords
  keyword_clean(id = "title", 
                keyword = "index_keywords",
                lemmatize = FALSE) %>%
  #Making the upper case for uniformity
  mutate(keyword = toupper(keyword)) %>% 
  #Replacing keywords that are synonymous
  mutate(keyword = replace(keyword, keyword == "ECOSYSTEMS", "ECOSYSTEM"),
         keyword = replace(keyword, keyword == "ECOSYSTEM SERVICES", "ECOSYSTEM SERVICE"),
         keyword = replace(keyword, keyword == "ANIMALS", "ANIMAL"),
         keyword = replace(keyword, keyword == "ANIMALIA", "ANIMAL"),
         keyword = replace(keyword, keyword == "GREENHOUSE GAS", "GREENHOUSE GASES"),
         keyword = replace(keyword, keyword == "AVES", "BIRD"),
         keyword = replace(keyword, keyword == "MAMMALIA", "MAMMAL"),
         keyword = replace(keyword, keyword == "MAMMALS", "MAMMAL"),
         keyword = replace(keyword, keyword == "LGM", "LAST GLACIAL MAXIMUM"),
         keyword = replace(keyword, keyword == "SPECIES DIVERSITY", "BIODIVERSITY"),
         keyword = replace(keyword, keyword == "DIVERSITY", "BIODIVERSITY"),
         keyword = replace(keyword, keyword == "ANCIENT DNA", "DNA"),
         keyword = replace(keyword, keyword %in% c("GEOGRAPHICAL DISTRIBUTION", "SPATIAL DISTRIBUTION", "POPULATION DISTRIBUTION"), "DISTRIBUTION"),
         keyword = replace(keyword, keyword %in% c("CLIMATE VARIATION", "CLIMATE MODELING", "CLIMATE MODELLING"), "CLIMATE CHANGE"),
         keyword = replace(keyword, keyword == "PALEOBIOGEOGRAPHY", "BIOGEOGRAPHY"),
         keyword = replace(keyword, keyword %in% c("CLIMATE EFFECT", "PALEOCLIMATE", "PALAEOCLIMATE"), "PALEOCLIMATE"),
         keyword = replace(keyword, keyword %in% c("GENETIC STRUCTURE", "GENETIC MARKER"), "GENETIC VARIATION")) %>% 
  
  #Removing unhelpful words
  anti_join(excluded_words, 
            by = c("keyword" = "word"))

#Graphing
paleo_kw_df %>% 
  keyword_group(id = "id", 
                keyword = "keyword") %>%
  keyword_vis(facet = TRUE) +
  scale_fill_manual(values = viridis::viridis(n = 3, begin = 0.5, end = 1)) +
  scale_alpha_manual(values = 0.3)

paleo_kw_df %>% 
  keyword_group(id = "id", 
                keyword = "keyword") %>%
  keyword_vis(facet = FALSE) +
  scale_fill_manual(values = viridis::viridis(n = 3, begin = 0.5, end = 1)) +
  scale_alpha_manual(values = 0.3)

#Saving
#ggsave("./graphs/keywordNetwork.pdf", width = 15, height = 7.15)


# Taxa breakdown ----------------------------------------------------------

# paleo_long_df <- paleo_df %>% 
#   
#   filter(!is.na(hindcasting)) %>% 
#   
#   pivot_longer(cols = )
  
paleo_summ_df <- paleo_df %>% 
  
  filter(!is.na(hindcasting)) %>% 
  
  group_by(year, kingdom, use_fossils, period_focus) %>% 
  
  summarise(n_records = n()) %>% 
  
  mutate(fossil_purpose = use_fossils,
         fossil_purpose = replace(fossil_purpose, is.na(fossil_purpose), "Unused"),
         fossil_purpose = factor(fossil_purpose, 
                                 levels = c("Modelling",
                                            "Validation",
                                            "Unused")))


#Graphing
#By fossil use
ggplot(filter(paleo_summ_df),
       aes(fill = fossil_purpose,
           y = n_records,
           x = year)) + 
  
  geom_bar(position = "fill",
           stat = "identity") + 
  
  scale_fill_viridis_d(option = "magma",
                       direction = 1,
                       begin  = 0.9, 
                       end = 0.1) +
  
  labs(y = "Proportion of records",
       x = "Year") +
  
  theme_bw()


#By use with a period facet
ggplot(filter(paleo_summ_df),
       aes(fill = fossil_purpose,
           y = n_records,
           x = year)) + 
  
  geom_bar(position = "fill",
           stat = "identity") + 
  
  scale_fill_viridis_d(option = "magma",
                       direction = 1,
                       begin  = 0.9, 
                       end = 0.1) +
  
  labs(y = "Proportion of records",
       x = "Year") +
  
  facet_wrap(~period_focus) +
  
  theme_bw()

#By period
ggplot(filter(paleo_summ_df),
       aes(fill = period_focus,
           y = n_records,
           x = year)) + 
  
  geom_bar(position = "fill",
           stat = "identity") + 
  
  scale_fill_viridis_d(option = "magma",
                       direction = 1,
                       begin  = 0.9, 
                       end = 0.1) +
  
  labs(y = "Proportion of records",
       x = "Year") +
  
  theme_bw()


#By kingdom
ggplot(paleo_summ_df,
       aes(fill = kingdom,
           y = n_records,
           x = year)) + 
  
  geom_bar(position = "fill",
           stat = "identity")




# Geographic distribution of studies --------------------------------------


# Converting to a sf object for tidy plotting
paleo_sf <- paleo_df %>% 
  
  filter(!is.na(longitude)) %>% 
  
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)


#Plotting distribution
ggplot() + 
  
  geom_map(data = map_data("world"), 
           map = map_data("world"),
           aes(long, lat, map_id = region),
           fill = "grey") +
  
  geom_sf(data = paleo_sf, 
          aes(size = log(spatial_extent_km),
              shape = kingdom,
              colour = period_focus),
          alpha = 0.6) +
  
  scale_colour_viridis_d(option = "magma",
                  direction = 1,
                  begin  = 0.9, 
                  end = 0.1,
                  name = "Period of focus") +
  
  scale_shape_manual(values = c(16, 17),
                     name = "Kingdom") + 
  
  labs(y = "Latitude (°)",
       x = "Longitude (°)") +
  
  theme_bw()
  
  
  
  
