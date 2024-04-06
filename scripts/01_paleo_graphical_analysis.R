# Libraries ---------------------------------------------------------------

#Data manipulation
library(janitor)
library(dplyr) 
library(tidyr)
library(scales)
library(forcats)

#visualisation
library(ggplot2)
library(viridis)
library(ggbreak)
library(patchwork)


# Loading data ------------------------------------------------------------

#Merged and scored paleoENM literature database
paleo_df <- read.csv("./data/paleo_sqlr_all_2023_12_04.csv")

# Fossil use --------------------------------------------------------------

# Figure 2 -----------------------------------------------------------------

## All publications -------------------------------------------------
paleo_fossil_df <- paleo_df %>% 
  
  #Since the value for fossil_* is unique to each row a single title can have multiple entries where this varies - as we want to explore paper by paper, rather than model by model I have used a group_by -> summarise that captures whether the paper used this in any of their models. Alternatively the distinct frame work after these two calls can be used to investigate the trend on a model by model basis instead, although this will not reflect publication count.
  
  group_by(year, title) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, T, F)) %>% 
  
  group_by(year) %>% 
  
  mutate(year_record_n = n()) %>% 
  
  ungroup() %>%
  
  group_by(year,fossil_modelling, fossil_validation, fossil_used, year_record_n) %>% 
  
  summarise(n_records = n()) %>% 
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")))


#An alternative count graph which displays the percentages of each stacked bar
ggplot(paleo_fossil_df, 
       aes(fill = fossil_purpose,
           y = n_records,
           x = year)) + 
  
  geom_bar(position = "stack",
           stat = "identity") +
  
  geom_text(aes(label=ifelse(fossil_purpose=="No Fossils",
                             scales::percent(n_records/year_record_n, accuracy = 1, suffix=""), ""),
                colour = fossil_purpose),
            position = position_stack(vjust = 0.5),
            size = 3,
            show.legend = FALSE) +

  scale_colour_manual(values = c("black","black", "white","white"),
                      guide = NULL) +
  
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       begin  = 0.95,
                       end = 0.1,
                       name = "Fossil Use") +
  
  labs(y = "Publication Count (#)",
       x = "Year") +
  
  scale_y_continuous(limits = c(-0.5, 70), 
                     expand = c(0, 0)) +
  
  scale_x_continuous(limits = c(2001, 2024), 
                     expand = c(0, 0)) +
  
  theme_bw() +
  
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = c(0.175, 0.81),
        legend.key.size=unit(0.5, "cm"),
        legend.background = element_rect(colour = "black"))

ggsave("./graphs/Fig2_Bloisetal_2col_one_label_pre-pub.pdf",
       width = 15, height = 10, units='cm')

# Post-processing notes:
# In Illustrator, need to change the percentages font to black and move to the top of each bar. Also changed stroke size around legend to 0.5pt
# Output version labeled with "_pre-pub", publication version post-Illustrator has that label removed

# Figure 3 -----------------------------------------------------------------

## Kingdom -----------------------------------------------------------------

paleo_king_df <- paleo_df %>% 
  
  #Since the value for fossil_* is unique to each row a single title can have multiple entries where this varies - as we want to explore paper by paper, rather than model by model I have used a group_by -> summarise that captures whether the paper used this in any of their models. Alternatively the distinct frame work after these two calls can be used to investigate the trend on a model by model basis instead, although this will not reflect publication count.
  group_by(year, title, kingdom) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  filter(kingdom  %in% c("Animalia", "Plantae")) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, T, F)) %>% 
  
  group_by(year, kingdom) %>% 
  mutate(year_record_n = n()) %>% 
  ungroup() %>%
  
  group_by(year,fossil_modelling, fossil_validation, fossil_used, year_record_n, kingdom) %>% 
  
  summarise(n_records = n()) %>% 
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")))

#Fossil use split between Kingdoms
fig3a <- ggplot(paleo_king_df, 
       aes(fill = fossil_purpose,
           y = n_records,
           x = year)) + 
  
  geom_bar(position = "stack",
           stat = "identity", width = 0.9) + 
  
  scale_colour_manual(values = c("black","black", 
                                 "white","white"),
                      guide = NULL) +
  
  scale_y_continuous(limits = c(-0.5, 42), 
                     expand = c(0, 0)) +
  
  scale_x_continuous(limits = c(2001, 2024), 
                     expand = c(0, 0)) +
  
  
  facet_wrap(~kingdom) +
  
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       begin  = 0.95,
                       end = 0.1,
                       name = "Fossil Use") +
  
  labs(y = "Publication Count (#)",
       x = "") +
  
  theme_bw() +
  
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = c(0.175, 0.81),
        legend.key.size=unit(0.5, "cm"),
        legend.background = element_rect(colour = "black"))

fig3a
ggsave("./graphs/Fig3a_Bloisetal_2col.pdf",
       width = 15, height = 5, units="cm")


## Additional Evidence -----------------------------------------------------

add_evi_fossil_df <- paleo_df %>% 
  
  #Since the value for fossil_* is unique to each row a single title can have multiple entries where this varies - as we want to explore paper by paper, rather than model by model I have used a group_by -> summarise that captures whether the paper used this in any of their models. Alternatively the distinct frame work after these two calls can be used to investigate the trend on a model by model basis instead, although this will not reflect publication count.
  group_by(year, title, additional_evidence_integration) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  #Restricting it to one row per paper
  distinct(title, .keep_all = TRUE) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, T, F)) %>% 
  
  group_by(year, additional_evidence_integration) %>% 
  mutate(year_record_n = n()) %>% 
  ungroup() %>%
  
  group_by(year,fossil_modelling, fossil_validation, fossil_used, year_record_n, additional_evidence_integration) %>% 
  
  summarise(n_records = n()) %>% 
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")),
         additional_evidence_integration = ifelse(additional_evidence_integration, "ENM + Other", "ENM Alone") )


#Graphing
fig3b <- ggplot(add_evi_fossil_df, 
       aes(fill = fossil_purpose,
           y = n_records,
           x = year, 
           label = year_record_n)) + 
  
  geom_bar(position = "stack",
           stat = "identity",
           width = 0.9) + 
  
  scale_colour_manual(values = c("black","black", 
                                 "white","white"),
                      guide = NULL) +
  
    scale_y_continuous(limits = c(-0.5, 50),
                     expand = c(0, 0)) +
  
  scale_x_continuous(limits = c(2002, 2024),
                     expand = c(0, 0)) + 
  
  facet_wrap(~additional_evidence_integration) + 
  
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       begin  = 0.95,
                       end = 0.1,
                       name = "Fossil Use",
                       guide = NULL) +
  
  labs(y = "Publication Count (#)",
       x = "Year") +
  
  theme_bw() +
  
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = c(0.175, 0.81),
        legend.key.size=unit(0.5, "cm"),
        legend.background = element_rect(colour = "black"))

fig3b
#Saving
ggsave("./graphs/Fig3b_Bloisetal_2col.pdf",
       width = 15, height = 5, units="cm")

## Compiled Figure 3 --------------------------------------------------------------

fig3a + fig3b + 
  plot_layout(ncol = 1, nrow = 2, widths = c(1, 1), heights = c(1, 1), guide = "keep") 

ggsave("./graphs/Fig3_Bloisetal_2col_nolabels_pre-pub.pdf",
       width = 15, height = 15, units="cm")

# Post-processing note:
# Move the legend down to the ENM Alone panel
# Reduced legend box stroke to 0.5pt and trimmed white space in legend
# Output version labeled with "_pre-pub", publication version post-Illustrator has that label removed


# Figure 4 --------------------------------------------------------

## Geographic scale - Fig 4a --------------------------------------------------------

paleo_geog_df <- paleo_df %>% 
  
  #Since the value for fossil_* is unique to each row a single title can have multiple entries where this varies - as we want to explore paper by paper, rather than model by model I have used a group_by -> summarise that captures whether the paper used this in any of their models. Alternatively the distinct frame work after these two calls can be used to investigate the trend on a model by model basis instead, although this will not reflect publication count.
  group_by(year, title, geographic_scale) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, T, F)) %>% 
  
  group_by(geographic_scale) %>% 
  mutate(geog_record_n = n()) %>% 
  ungroup() %>%
  
  group_by(fossil_modelling, fossil_validation, fossil_used, geog_record_n, geographic_scale) %>% 
  
  summarise(n_records= n()) %>% 
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")),
         geographic_scale = factor(geographic_scale,
                                 levels = c("local",
                                            "regional",
                                            "continental",
                                            "cross-continental",
                                            "global")),
         perc_lab = scales::percent(n_records/geog_record_n,
                                    accuracy = 1),
         
         #Hacky solution to center the text within the labs (can't use nudge_y because it is a barplot). I hate it.
         perc_nchar = nchar(perc_lab),
         perc_lab = case_when(perc_nchar == 3 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 2 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 3 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0(perc_lab, " "),
                              perc_nchar == 2 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0( perc_lab, " "),
                              .default = perc_lab))
         
#Recoding levels for aesthetics
paleo_geog_df$geographic_scale <- fct_recode(.f = paleo_geog_df$geographic_scale,
                                             "Local" = "local",
                                             "Regional" = "regional",
                                             "Continental" = "continental",
                                             "Cross-continental" = "cross-continental",
                                             "Global" = "global")

#Graphing
fig4a <- ggplot(paleo_geog_df, 
       aes(fill = fossil_purpose,
           y = n_records,
           x = geographic_scale, 
           label = geog_record_n)) + 
  
  geom_bar(position = "stack",
           stat = "identity") + 
  
  scale_colour_manual(values = c("black","black", 
                                 "white","white"),
                      guide = NULL) +
  
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       begin  = 0.95,
                       end = 0.1,
                       name = "Fossil Use") +
  
  scale_y_continuous(limits = c(-2, 450), 
                     expand = c(0, 0)) +
  
  labs(y = "Publication Count (#)",
       x = "Geographic Scale",
       title = "A)") +
  
  theme_bw() +

  theme(plot.title = element_text(hjust = -0.125, size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9, angle=30, hjust=1),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = c(0.78, 0.8),
        legend.key.size=unit(0.5, "cm"),
        legend.background = element_rect(colour = "black"))

fig4a

#Saving
ggsave("./graphs/figure_4a_geographic_scale.pdf",
       width = 6, height = 4.5)


## Binned time -------------------------------------------------------------

paleo_time_bin_df <-  paleo_df %>% 
  
  mutate(time_projected = as.numeric(time_projected)) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, T, F),
         
         fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils"))) %>%
  ungroup() %>%
  
  #Excluding projections to the future or those projecting to the present
  filter(time_projected < 0) %>% 
  
#Binning times to show fossil use for varying periods
  mutate(quaternary_bin = case_when(
    #Breaking up the Quaternary
    # Jessica changed labels here
    time_projected >= -11700 ~ "Holocene",
    time_projected < -11700 & time_projected >= -22000 ~ "LGM - deglacial",
    time_projected < -22000 & time_projected >= -140000 ~ "LIG - LGM",
    time_projected < -140000 & time_projected >= -2600000 ~ "Early Pleistocene",
    .default = NA),
    
    # change order
    quaternary_bin = factor(quaternary_bin,
                      levels = c("Holocene",
                                 "LGM - deglacial",
                                 "LIG - LGM",
                                 "Early Pleistocene")),
    
    #Geologic Period
    period_bin = case_when(
    time_projected >= -2600000 ~ "Quaternary",
    time_projected < -2600000 & time_projected >= -23000000 ~ "Neogene",
    time_projected < -23000000 & time_projected >= -66000000 ~ "Paleogene",
    time_projected < -66000000 & time_projected >= -150000000 ~ "Cretaceous",
    time_projected < -150000000 & time_projected >= -201300000 ~ "Jurassic",
    time_projected < -201300000 & time_projected >= -252200000 ~ "Triassic",
    time_projected < -252200000 & time_projected >= -298900000 ~ "Permian",
    time_projected < -298900000 & time_projected >= -358900000 ~ "Carboniferous",
    time_projected < -358900000 & time_projected >= -419200000 ~ "Devonian",
    time_projected < -419200000 & time_projected >= -443400000 ~ "Silurian",
    time_projected < -443400000 & time_projected >= -485400000 ~ "Ordovician",
    time_projected < -485400000 & time_projected >= -541400000 ~ "Cambrian",
    .default = NA),
    period_bin = factor(period_bin,
                        levels = c("Quaternary",
                                   "Neogene",
                                   "Paleogene",
                                   "Cretaceous",
                                   "Jurassic",
                                   "Triassic",
                                   "Permian",
                                   "Carboniferous",
                                   "Devonian",
                                   "Silurian",
                                   "Ordovician",
                                   "Cambrian")),
    #Geologic Era
    era_bin = case_when(
      time_projected >= -66000000 ~ "Cenozoic",
      time_projected < -66000000 & time_projected >= -252200000 ~ "Mesozoic",
      time_projected < -252200000 & time_projected >= -541400000 ~ "Paleozoic",
      .default = NA),
    era_bin = factor(era_bin,
           levels = c("Cenozoic",
                      "Mesozoic",
                      "Paleozoic"))
    )

### Era Plot - Fig 4b-------------------------------------------------------------

era_df <- paleo_time_bin_df %>% 
  
  group_by(year, title, era_bin) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, 
                              T, F)) %>% 
  
  group_by(era_bin) %>% 
  mutate(year_record_n = n()) %>% 
  ungroup() %>%
  
  group_by(fossil_modelling, fossil_validation, 
           fossil_used, year_record_n, era_bin) %>% 
  
  summarise(n_records = n()) %>% 
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")),
         
         perc_lab = scales::percent(n_records/year_record_n,
                                    accuracy = 1),
         
         #Hacky solution to center the text within the labs (can't use nudge_y because it is a barplot). I hate it.
         perc_nchar = nchar(perc_lab),
         perc_lab = case_when(perc_nchar == 3 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 2 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 3 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0(perc_lab, " "),
                              perc_nchar == 2 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0( perc_lab, " "),
                              .default = perc_lab))


# Era Pie Chart
# Further summarisation
era_summ_df <- era_df %>%
  group_by(era_bin) %>%
  summarise(era_count = sum(n_records))

# Graphing
# With a label of percentage
fig4b <- ggplot(era_summ_df, 
                aes(y = era_count,
                    fill = era_bin,
                    x = "")) + 
  
  geom_bar(stat = "identity", 
           width = 1) +
  
  scale_fill_viridis_d(option = "magma",
                       name = "Geologic Era",
                       begin = 0.8,
                       end = 0.2) +
  
  coord_polar("y", start = 0) +
  
  theme_minimal() +
  
  labs(title = "B)") +
  
  theme(plot.title = element_text(hjust = -0.125, size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        legend.title.position = "bottom",
        legend.key.size=unit(0.5, "cm")) 

fig4b
# Saving
ggsave("./graphs/figure_4b_era_pie_inset_label.pdf",
       width = 6, height = 4.5)


### Period Plot - Fig 4c -------------------------------------------------------------
period_df_mod <- paleo_time_bin_df %>% 
  
  group_by(year, title, period_bin) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, 
                              T, F)) %>% 
  
  group_by(period_bin) %>% 
  mutate(year_record_n = n()) %>% 
  ungroup() %>%
  
  group_by(fossil_modelling, fossil_validation, 
           fossil_used, year_record_n, period_bin) %>% 
  
  summarise(n_records = n()) %>% 
  filter(period_bin != "Quaternary") %>%   # added this line
  droplevels() %>% # added this line
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")),
         
         perc_lab = scales::percent(n_records/year_record_n,
                                    accuracy = 1),
         
         #Hacky solution to center the text within the labs (can't use nudge_y because it is a barplot). I hate it.
         perc_nchar = nchar(perc_lab),
         perc_lab = case_when(perc_nchar == 3 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 2 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 3 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0(perc_lab, " "),
                              perc_nchar == 2 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0( perc_lab, " "),
                              .default = perc_lab))

#Plotting data - modified no Quaternary
fig4c <- ggplot(period_df_mod, 
       aes(fill = fossil_purpose,
           y = n_records,
           x = period_bin, 
           label = year_record_n)) + 
  
  geom_bar(position = "stack",
           stat = "identity") + 
  
  scale_colour_manual(values = c("black","black", 
                                 "white","white"),
                      guide = NULL) +
  
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       begin  = 0.95,
                       end = 0.1,
                       name = "Fossil Use",
                       guide = NULL) +
  
  labs(y = "Publication Count (#)",
       x = "Geologic Period",
       title = "C)") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust = -0.125, size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9, angle=30, hjust=1),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = c(0.78, 0.8),
        legend.key.size=unit(0.5, "cm"),
        legend.background = element_rect(colour = "black"))


fig4c
#Saving
ggsave("./graphs/figure_4c_period_bins.pdf",
       width = 6, height = 4.5)


### Quaternary Plot - Fig 4d---------------------------------------------------------

#Note! The same publication can be counted multiple times as some would capture multiple time periods
quaternary_df <- paleo_time_bin_df %>% 
  
  filter(!is.na(quaternary_bin)) %>% 
  
  group_by(year, title, quaternary_bin) %>%
  
  summarise(fossil_modelling = any(fossil_modelling),
            fossil_validation = any(fossil_validation)) %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, 
                              T, F)) %>% 
  
  group_by(quaternary_bin) %>% 
  mutate(year_record_n = n()) %>% 
  ungroup() %>%
  
  group_by(fossil_modelling, fossil_validation, 
           fossil_used, year_record_n, quaternary_bin) %>% 
  
  summarise(n_records = n()) %>% 
  
  mutate(fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils")),
         
         perc_lab = scales::percent(n_records/year_record_n,
                                    accuracy = 1),
         
         #Hacky solution to center the text within the labs (can't use nudge_y because it is a barplot). I hate it.
         perc_nchar = nchar(perc_lab),
         perc_lab = case_when(perc_nchar == 3 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 2 & !fossil_purpose %in% c("Modeling", "Validation") ~ paste0(" ", perc_lab),
                              perc_nchar == 3 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0(perc_lab, " "),
                              perc_nchar == 2 & fossil_purpose %in% c("Modeling", "Validation") ~ paste0( perc_lab, " "),
                              .default = perc_lab))

#Plotting data
fig4d <- ggplot(quaternary_df, 
                aes(fill = fossil_purpose,
                    y = n_records,
                    x = quaternary_bin, 
                    label = year_record_n)) + 
  
  geom_bar(position = "stack",
           stat = "identity") + 
  
  
  scale_colour_manual(values = c("black","black", 
                                 "white","white"),
                      guide = NULL) +
  
  scale_fill_viridis_d(option = "viridis",
                       direction = 1,
                       begin  = 0.95,
                       end = 0.1,
                       name = "Fossil Use",
                       guide = NULL) +
  
  labs(y = "",
       x = "Quaternary Epoch",
       title = "D)") +
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust = -0.125, size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9, angle=30, hjust=1),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = c(0.78, 0.8),
        legend.key.size=unit(0.5, "cm"),
        legend.background = element_rect(colour = "black"))

fig4d
#Saving
ggsave("./graphs/figure_4d_quaternary_bins.pdf",
       width = 6, height = 4.5)

# Compiled Figure 4 --------------------------------------------------------------

fig4a + fig4b + fig4c + fig4d + plot_layout(ncol = 2, nrow = 2, widths = c(1, 1, 1), heights = c(1, 1),
                                            guide = "keep") 

ggsave("./graphs/Fig4_Bloisetal_2col_pre-pub.pdf",
       width = 16, height = 16, units="cm")

# Post-processing note:
# This compiled figure is close, but needs a decent amount of post-processing to be publication ready since it is difficult to get the proportions right among the pie-chart vs the other panels
# Reduced overall size to 15cm wide x 14cm high (set at 16 to help with proportions)
# Moved labels A) and C) to be left justified, and Pub Count axis title right a few steps to offset labels.
# move Fossil Use legend to panel C, change stroke size to 0.5pt, and condense for space.
# Center Geologic Era Panel B legend and move it closer to pie chart, and center pie chart within the panel
# We also scaled the pie chart so that there was not so much white space around it. See the two versions saved in the graphs folder: '_scaled' and '_unscaled'

# Figure 5 --------------------------------------------------------------

# Projection --------------------------------------------------------------
## Through time ------------------------------------------------------------

#Version 2
paleo_temp_range_df <-  paleo_df %>% 
  
  #Creating a column to determine if fossils were used at all
  mutate(fossil_used = ifelse(fossil_modelling | fossil_validation, T, F),
         
         fossil_purpose = case_when(fossil_modelling & !fossil_validation ~ "Modeling",
                                    fossil_modelling & fossil_validation ~ "Modeling + Validation",
                                    !fossil_modelling & fossil_validation ~ "Validation",
                                    !fossil_modelling & !fossil_validation ~ "No Fossils"),
         
         fossil_purpose = factor(fossil_purpose,
                                 levels = c("Modeling",
                                            "Modeling + Validation",
                                            "Validation",
                                            "No Fossils"))) %>%
  ungroup() %>%
  
  #Excluding projections to the future or those projecting to the present
  filter(time_projected < 0) %>%
  
  mutate(fossil_in_model = ifelse(fossil_modelling, "Fossils", "No Fossils"),
         time_integrated_cor = ifelse(!time_integrated, FALSE, 
                                      ifelse(time_projected == time_modelled, FALSE, TRUE)),
         
         time_match = case_when(time_modelled > time_projected & time_integrated_cor == FALSE ~ "Hindcast", 
                                time_modelled == time_projected & time_integrated_cor == FALSE ~ "Contemporaneous",
                                time_modelled < time_projected & time_integrated_cor == FALSE ~ "Forecast",
                                time_integrated_cor == TRUE ~ "Time Pooled"),
         
         time_match = factor(time_match, 
                             levels = c("Forecast", "Contemporaneous", "Hindcast", 'Time Pooled')))

#NEARTIME
neartime_gg <- paleo_temp_range_df %>%
  
  filter(time_projected > -200000 & time_modelled > -200000) %>%
  
  ggplot(data = ., aes(x = year, 
                       y = time_projected/1000,
                       group = year)) +
  
  
  labs(y = "Time Projected (Ka)") + 
  
  scale_y_continuous(limits = c(-160, 0)) +
  
  #geom_violin() +
  
  geom_point(aes(fill = time_match,
                 shape = time_match),
             size = 2,
             alpha = 0.8) +
  
  scale_fill_manual(values = c("#BBDF27FF", "#21908CFF", 
                               "#482576FF", "grey"),
                    name ="Projection",
                    guide = "none") +
  
  scale_shape_manual(values = c(24, 23, 25, 21),
                     name = "Projection",
                     guide = "none") + 
  
  facet_wrap(~fossil_in_model) +
  
  
  scale_y_cut(breaks = c(-23, -81),
              scales = c(2, 1)) +
  
  scale_x_continuous(limits = c(2002, 2024),
                     breaks = c(2005, 2010, 2015, 2020)) + 
  
  theme_bw() + 
  
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 9),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.background = element_rect(colour = "black"),
        plot.margin = margin(0,1,0,1))

neartime_gg

#DEEPTIME
deeptime_gg <- paleo_temp_range_df %>%
  
  filter(time_projected < -200000) %>% 
  
  mutate(time_projected = time_projected/1000000) %>% 
  
  ggplot(data = ., aes(x = year, 
                       y = time_projected,
                       group = year)) +
  
  labs(y = "Time Projected (Ma)",
       x = "Publication Year") + 
  
  scale_y_continuous(limits = c(-100, -0.5)) +
  
  geom_point(aes(fill = time_match,
                 shape = time_match),
             size = 2,
             alpha = 0.8) +
  
  scale_fill_manual(values = c("#BBDF27FF", "#21908CFF", 
                               "#482576FF", "grey"),
                    name ="Projection") +
  
  scale_shape_manual(values = c(24, 23, 25, 21),
                     name = "Projection") + 
  
  facet_wrap(~fossil_in_model) +
  
  scale_y_cut(breaks = c(-0.85, -25),
              which = c(2, 2, 1),
              scales = c(2, 2, 0.5)) +
  
  scale_x_continuous(limits = c(2002, 2024),
                     breaks = c(2005, 2010, 2015, 2020)) + 
  
  theme_bw() + 
  
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 9),
        strip.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,1,0,1)) +
  
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0.5),
         shape = guide_legend(title.position = "top", 
                              title.hjust = 0.5))

deeptime_gg

#Stacking the plots above one another
neartime_gg +  deeptime_gg + plot_layout(ncol = 1)

#Saving
ggsave("./graphs/Fig5_Bloisetal_2col_pre_pub.pdf",
       width = 15, height = 21, units="cm")

# Post-processing notes:
# Changed the stroke weight on the points to 0.25
# Changed overall artboard height to 20 cm and moved the lower panel up a bit to be a bit more condensed.
