##load packages##
library(tidyverse)
library(summarytools)
library(sf)

##read data into R##
birth_recode <- read_csv("GMBR81SV/GMBR81FL.csv")
household_recode <- read_csv("GMHR81SV/GMHR81FL.csv")
womens_recode <- read_csv("GMIR81SV/GMIR81FL.csv")
child_recode <- read_csv("GMKR81SV/GMKR81FL.csv")
cluster_gps <- st_read("GMGE81FL/GMGE81FL.shp")


##select AND rename AND create_joing_ID variables##
women_select <- womens_recode %>% 
  select(CASEID, V001, V002, V003, V004, V005, 
         V131, V130, V024, V025, V106, V702, V012, V716, V704, V201, V157, V158, V159) %>% 
  rename(cluster = V001, household = V002, respondentid = V003, sample_weight = V005, ethnicty = V131,
         religion = V130, region = V024, rural_urban = V025, mat_educ = V106, mat_edu_years = V702,
         mat_age = V012, mat_occup = V716, partner_occup = V704, parity = V201, newspaper = V157, radio = V158, tv = V159) %>% 
  mutate(women_id = paste(cluster, household, respondentid, sep = "_"))

household_select <- household_recode %>% 
  select(HHID, HV001, HV002, HV004, HV005, HV270A, HV014, HV211, HV212, HV021, HV023) %>% 
  rename(cluster = HV001, household = HV002, wealth_index_urb_rural = HV270A, no_children_hh = HV014, 
         motocycle = HV211, car = HV212, PSU = HV021, strata = HV023) %>% 
  mutate(household_id = paste(cluster, household, sep = "_"))

Child_select <- child_recode %>% 
  filter(B19 >= 0 & B19 <=35) %>% 
  select(MIDX, V001, V002, V003, V004, V005, H1, H1A, H2, H2D, H2M, H2Y, H51, H51D, H51M, H51Y, H52, H52D, H52M, H52Y, H53, H53D, H53M, H53Y, H9, H9D, H9M, H9Y,
         H50, H50D, H50M, H50Y, B4, B17, B1, B2, B19, M15, BORD, M14, H4, H4D, H4M, H4Y, H6, H6D, H6M, H6Y, H8, H8D, H8M, H8Y) %>% 
  rename(birth_index = MIDX, cluster = V001, household = V002, respondentid = V003, sample_weight = V005,
         health_card = H1, BCg_health_card = H2, bcg_day = H2D, bcg_month = H2M, bcg_year = H2Y,
         penta1_health_card = H51, penta1_day = H51D, penta1_month = H51M, penta1_year = H51Y,
         penta2_health_card = H52, penta2_day = H52D, penta2_month = H52M, penta2_year = H52Y,
         penta3_health_card = H53, penta3_day = H53D, penta3_month = H53M, penta3_year = H53Y,
         mcv1_health_card = H9, mcv1_day = H9D, mcv1_month = H9M, mcv1_year = H9Y,
         opv1_health_card = H4, opv1_day = H4D, opv1_month = H4M, opv1_year = H4Y,
         opv2_health_card = H6, opv2_day = H6D, opv2_month = H6M, opv2_year = H6Y,
         opv3_health_card = H8, opv3_day = H8D, opv3_month = H8M, opv3_year = H8Y,
         hepB0_health_card = H50, hepB0_day = H50D, hepB0_month = H50M, hepB0_year = H50Y,
         sex = B4, day_birth = B17, month_birth = B1, year_birth = B2, age_in_month = B19, place_birth = M15,
         birth_order = BORD, mat_anc = M14) %>% 
  mutate(women_id = paste(cluster, household, respondentid, sep = "_")) %>% 
  mutate(household_id = paste(cluster, household, sep = "_")) %>% 
  mutate(sample_wt = sample_weight/1000000)

cluster_gps_select <- cluster_gps %>% 
  select(DHSCLUST, URBAN_RURA, LATNUM, LONGNUM) %>% 
  rename(cluster = DHSCLUST, urban_rural = URBAN_RURA, latitude = LATNUM, longitude = LONGNUM) %>% 
  mutate(geometry = NULL)


##summary of data frame###
dfSummary(Child_select)


##join all datasets into one common file##
dhs_final_dataset <- Child_select %>% 
  left_join(women_select, by = "women_id") %>% 
  left_join(household_select, by = "household_id") %>% 
  left_join(cluster_gps_select, by = "cluster")


##write final dataset to file##
write_csv(dhs_final_dataset, "dhs_final_dataset.csv")

