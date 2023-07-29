
##load packages##
library(tidyverse)
library(summarytools)

##load data##
dhs_final_dataset <- read_csv("dhs_final_dataset.csv")


##categorisation of continious variables##
dhs_final_dataset1 <- dhs_final_dataset %>% 
  mutate(mat_age_cat = cut(mat_age, breaks = c(14,19,24,29,34,39, 49),
                           labels = c("<=19","20-24", "25-29", "30-34", "35-39", ">=40"))) %>% 
  mutate(parity_cat = if_else(parity <4, "1-3", "4 or more")) %>% 
  mutate(birth_order_cat = if_else(birth_order <4, "1-3", "4 or more")) %>%
  ##categorisation of categorical variables##
  mutate(mat_anc_cat = recode(mat_anc, .default = mat_anc, 
                              "No antenatal visits" = "no anc",
                              "1" = "1-3 anc",
                              "2" = "1-3 anc",
                              "3" = "1-3 anc",
                              "4" = "4 or more",
                              "5" = "4 or more",
                              "6" = "4 or more",
                              "7" = "4 or more",
                              "8" = "4 or more",
                              "9" = "4 or more",
                              "10" = "4 or more",
                              "12" = "4 or more",
                              "13" = "4 or more",
                              "14" = "4 or more",
                              "18" = "4 or more",
                              "20" = "4 or more")) %>% 
  mutate(place_birth_cat = recode(place_birth, .default = place_birth,
                                  "Government health center" = "health facility",
                                  "Government health post" = "health facility",
                                  "Government hospital" = "health facility",
                                  "NGO hospital/ clinic" = "health facility",
                                  "Private hospital/clinic" = "health facility",
                                  "Other home" = "home",
                                  "Respondent's home" = "home",
                                  "Other" = "home")) %>% 
  mutate(radio = recode(radio,
                        "At least once a week" = "exposed to media",
                        "Less than once a week" = "exposed to media",
                        "Not at all" = "not exposed to media"),
         newspaper = recode(newspaper,
                        "At least once a week" = "exposed to media",
                        "Less than once a week" = "exposed to media",
                        "Not at all" = "not exposed to media"),
         tv = recode(tv,
                        "At least once a week" = "exposed to media",
                        "Less than once a week" = "exposed to media",
                        "Not at all" = "not exposed to media")) %>% 
  mutate(media_exposure = if_else(radio == "exposed to media" | newspaper == "exposed to media" | tv == "exposed to media",
                                  "exposed to media", "not exposed to media"))

##write final dataset to file##
write_csv(dhs_final_dataset1, "dhs_final_dataset1.csv")



  













