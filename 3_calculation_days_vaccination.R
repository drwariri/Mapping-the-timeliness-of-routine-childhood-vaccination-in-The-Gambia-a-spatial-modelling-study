###load packages###
library(tidyverse)
library(lubridate)


##read data into R###
dhs_final_dataset1 <- read_csv("dhs_final_dataset1.csv")


###make date of birth and date of vaccination###
dhs_final_dataset2 <- dhs_final_dataset1 %>%
  mutate(birth_date = make_date(year_birth,month_birth,day_birth)) %>% 
  mutate(bcg_date = make_date(bcg_year,bcg_month,bcg_day)) %>% 
  mutate(hepB0_date = make_date(hepB0_year,hepB0_month,hepB0_day)) %>% 
  mutate(penta1_date = make_date(penta1_year,penta1_month,penta1_day)) %>% 
  mutate(penta2_date = make_date(penta2_year,penta2_month,penta2_day)) %>%
  mutate(penta3_date = make_date(penta3_year,penta3_month,penta3_day)) %>%
  mutate(mcv1_date = make_date(mcv1_year,mcv1_month,mcv1_day)) %>%
  mutate(opv1_date = make_date(opv1_year,opv1_month,opv1_day)) %>% 
  mutate(opv2_date = make_date(opv2_year,opv2_month,opv2_day)) %>% 
  mutate(opv3_date = make_date(opv3_year,opv3_month,opv3_day)) %>% 

###calculate age in days at vaccination (date difference)###
  mutate(bcg_age_vac = difftime(bcg_date, birth_date, units = "days")) %>% 
  mutate(hepB0_age_vac = difftime(hepB0_date, birth_date, units = "days")) %>% 
  mutate(penta1_age_vac = difftime(penta1_date, birth_date, units = "days")) %>% 
  mutate(penta2_age_vac = difftime(penta2_date, birth_date, units = "days")) %>% 
  mutate(penta3_age_vac = difftime(penta3_date, birth_date, units = "days")) %>% 
  mutate(mcv1_age_vac = difftime(mcv1_date, birth_date, units = "days")) %>% 
  mutate(opv1_age_vac = difftime(opv1_date, birth_date, units = "days")) %>%
  mutate(opv2_age_vac = difftime(opv2_date, birth_date, units = "days")) %>% 
  mutate(opv3_age_vac = difftime(opv3_date, birth_date, units = "days")) %>% 
    
###calculate intervals between penta1, penta2, penta3###
  mutate(penta1_penta2_interval = difftime(penta2_date, penta1_date, units = "days")) %>%
  mutate(penta2_penta3_interval = difftime(penta3_date, penta2_date, units = "days")) %>%
  mutate(opv1_opv2_interval = difftime(opv2_date, opv1_date, units = "days")) %>% 
  mutate(opv2_opv3_interval = difftime(opv3_date, opv2_date, units = "days")) %>% 

###categorise dimensions of timeliness based on benchmark definitions for all vaccines###
  mutate(penta1_age_vac_cat = cut(as.numeric(penta1_age_vac), breaks = c(-Inf, 60, 90, Inf),
                           labels = c("early","61-90", "delayed"))) %>% 
  mutate(penta2_age_vac_cat = cut(as.numeric(penta2_age_vac), breaks = c(-Inf, 90, 120, Inf),
                                  labels = c("early","91-120", "delayed"))) %>% 
  mutate(penta3_age_vac_cat = cut(as.numeric(penta3_age_vac), breaks = c(-Inf, 120, 150, Inf),
                                  labels = c("early","121-150", "delayed"))) %>%
  mutate(opv1_age_vac_cat = cut(as.numeric(opv1_age_vac), breaks = c(-Inf, 60, 90, Inf),
                                    labels = c("early","61-90", "delayed"))) %>%
  mutate(opv2_age_vac_cat = cut(as.numeric(opv2_age_vac), breaks = c(-Inf, 90, 120, Inf),
                                    labels = c("early","91-120", "delayed"))) %>%
  mutate(opv3_age_vac_cat = cut(as.numeric(opv3_age_vac), breaks = c(-Inf, 120, 150, Inf),
                                    labels = c("early","121-150", "delayed"))) %>%
  mutate(mcv1_age_vac_cat = cut(as.numeric(mcv1_age_vac), breaks = c(-Inf, 270, 300, Inf),
                                  labels = c("early","271-300", "delayed"))) %>% 
  mutate(bcg_age_vac_cat = cut(as.numeric(bcg_age_vac), breaks = c(-Inf, 1, 14, Inf),
                                labels = c("0-1","2-14", "delayed"))) %>% 
  mutate(hepB0_age_vac_cat = cut(as.numeric(hepB0_age_vac), breaks = c(-Inf, 1, 14, Inf),
                               labels = c("0-1","2-14", "delayed"))) %>% 
  mutate(penta1_penta2_interval_cat = cut(as.numeric(penta1_penta2_interval), breaks = c(-Inf, 27, 56, Inf),
                                 labels = c("interval_early","28-56", "interval_delayed"))) %>% 
  mutate(penta2_penta3_interval_cat = cut(as.numeric(penta2_penta3_interval), breaks = c(-Inf, 27, 56, Inf),
                                          labels = c("interval_early","28-56", "interval_delayed"))) %>% 
  mutate(opv1_opv2_interval_cat = cut(as.numeric(opv1_opv2_interval), breaks = c(-Inf, 27, 56, Inf),
                                            labels = c("interval_early","28-56", "interval_delayed"))) %>% 
  mutate(opv2_opv3_interval_cat = cut(as.numeric(opv2_opv3_interval), breaks = c(-Inf, 27, 56, Inf),
                                            labels = c("interval_early","28-56", "interval_delayed"))) %>%
  
  ##convert age in days at vaccination to numeric variable##
  mutate(hepB0_age_vac_new = as.numeric(hepB0_age_vac)) %>%
  mutate(bcg_age_vac_new = as.numeric(bcg_age_vac)) %>%
  mutate(penta1_age_vac_new = as.numeric(penta1_age_vac)) %>%
  mutate(penta2_age_vac_new = as.numeric(penta2_age_vac)) %>%
  mutate(penta3_age_vac_new = as.numeric(penta3_age_vac)) %>%
  mutate(opv1_age_vac_new = as.numeric(opv1_age_vac)) %>%
  mutate(opv2_age_vac_new = as.numeric(opv2_age_vac)) %>%
  mutate(opv3_age_vac_new = as.numeric(opv3_age_vac)) %>% 
  mutate(mcv1_age_vac_new = as.numeric(mcv1_age_vac)) %>%
  mutate(penta1_penta2_interval_new = as.numeric(penta1_penta2_interval)) %>% 
  mutate(penta2_penta3_interval_new = as.numeric(penta2_penta3_interval)) %>% 
  mutate(opv1_opv2_interval_new = as.numeric(opv1_opv2_interval)) %>% 
  mutate(opv2_opv3_interval_new = as.numeric(opv2_opv3_interval)) %>% 
  
  ##calculate difference in timeliness as continious variable based on benchmark##
  mutate(hepB0_diff = case_when(is.na(hepB0_age_vac_new) == TRUE ~ 999,
                                hepB0_age_vac_new < 2 ~ 0,
                                hepB0_age_vac_new >= 2 ~ hepB0_age_vac_new-1)) %>%
  mutate(bcg_diff = case_when(is.na(bcg_age_vac_new) == TRUE ~ 999,
                                  bcg_age_vac_new < 15 ~ 0,
                                  bcg_age_vac_new >= 15 ~ bcg_age_vac_new-14)) %>% 
  mutate(penta1_diff = case_when(is.na(penta1_age_vac_new) == TRUE ~ 999,
                                penta1_age_vac_new >60 & penta1_age_vac_new <91 ~ 0,
                                penta1_age_vac_new >= 91 ~ penta1_age_vac_new-90, penta1_age_vac_new <61 ~ 61-penta1_age_vac_new)) %>% 
  mutate(penta2_diff = case_when(is.na(penta2_age_vac_new) == TRUE ~ 999,
                                 penta2_age_vac_new >90 & penta2_age_vac_new <121 ~ 0,
                                 penta2_age_vac_new >= 121 ~ penta2_age_vac_new-120, penta2_age_vac_new <91 ~ 91-penta1_age_vac_new)) %>% 
  mutate(penta3_diff = case_when(is.na(penta3_age_vac_new) == TRUE ~ 999,
                                 penta3_age_vac_new >120 & penta3_age_vac_new <151 ~ 0,
                                 penta3_age_vac_new >= 151 ~ penta3_age_vac_new-150, penta3_age_vac_new <121 ~ 121-penta3_age_vac_new)) %>%
  mutate(opv1_diff = case_when(is.na(opv1_age_vac_new) == TRUE ~ 999,
                                 opv1_age_vac_new >60 & opv1_age_vac_new <91 ~ 0,
                                 opv1_age_vac_new >= 91 ~ opv1_age_vac_new-90, opv1_age_vac_new <61 ~ 61-opv1_age_vac_new)) %>%
  mutate(opv2_diff = case_when(is.na(opv2_age_vac_new) == TRUE ~ 999,
                               opv2_age_vac_new >90 & opv2_age_vac_new <121 ~ 0,
                               opv2_age_vac_new >= 121 ~ opv2_age_vac_new-120, opv2_age_vac_new <91 ~ 91-opv2_age_vac_new)) %>%
  mutate(opv3_diff = case_when(is.na(opv3_age_vac_new) == TRUE ~ 999,
                                 opv3_age_vac_new >120 & opv3_age_vac_new <151 ~ 0,
                                 opv3_age_vac_new >= 151 ~ opv3_age_vac_new-150, opv3_age_vac_new <121 ~ 121-opv3_age_vac_new)) %>% 
  mutate(mcv1_diff = case_when(is.na(mcv1_age_vac_new) == TRUE ~ 999,
                               mcv1_age_vac_new >270 & mcv1_age_vac_new <301 ~ 0,
                               mcv1_age_vac_new >= 301 ~ mcv1_age_vac_new-300, mcv1_age_vac_new <271 ~ 271-mcv1_age_vac_new)) %>% 
  mutate(penta1_penta2_diff = case_when(is.na(penta1_penta2_interval_new) == TRUE ~ 999,
                               penta1_penta2_interval_new >27 & penta1_penta2_interval_new <57 ~ 0,
                               penta1_penta2_interval_new >= 57 ~ penta1_penta2_interval_new-56,
                               penta1_penta2_interval_new <28 ~ 28-penta1_penta2_interval_new)) %>% 
  mutate(penta2_penta3_diff = case_when(is.na(penta2_penta3_interval_new) == TRUE ~ 999,
                               penta2_penta3_interval_new >27 & penta2_penta3_interval_new <57 ~ 0,
                               penta2_penta3_interval_new >= 57 ~ penta2_penta3_interval_new-56,
                               penta2_penta3_interval_new <28 ~ 28-penta2_penta3_interval_new)) %>% 
  mutate(opv1_opv2_diff = case_when(is.na(opv1_opv2_interval_new) == TRUE ~ 999,
                               opv1_opv2_interval_new >27 & opv1_opv2_interval_new <57 ~ 0,
                               opv1_opv2_interval_new >= 57 ~ opv1_opv2_interval_new-56,
                               opv1_opv2_interval_new <28 ~ 28-opv1_opv2_interval_new)) %>% 
  mutate(opv2_opv3_diff = case_when(is.na(opv2_opv3_interval_new) == TRUE ~ 999,
                                        opv2_opv3_interval_new >27 & opv2_opv3_interval_new <57 ~ 0,
                                        opv2_opv3_interval_new >= 57 ~ opv2_opv3_interval_new-56,
                                        opv2_opv3_interval_new <28 ~ 28-opv2_opv3_interval_new))
  
  
##write final dataset to file##
  write_csv(dhs_final_dataset2, "dhs_final_dataset2.csv")
  
  