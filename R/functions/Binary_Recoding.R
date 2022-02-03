library(readr)
library(tidyverse)

recoding_preliminary <- function(r) {
  
  r=response


  
r$hhh_gender <- case_when( r$hhh == "yes" & r$gender_respondent_r == "female"| r$sex_head_hh == "female" ~  "Female",
                         TRUE ~ "Male")
  
# % of households with at least one member with a disability
r$HH_with_Disability <- case_when( r$pwd == "yes" ~ "withdisability",
                    TRUE ~ "nodisability")

# % of households with at least one member with a chronic disease
r$HH_with_chronic_disease <- case_when( r$chronic_disease == "yes" ~ "with_chronic",
                                TRUE ~ "no_chronic")

  
  
# % of interviews conducted with head of household
r$hh1 <- case_when(r$hhh == "yes" ~ 1, TRUE ~ 0)

##% of interviews conducted with male or female participants
r$hh1_i <- ifelse(r$gender_respondent_r == "female", 1, 0)
r$hh1_ii <- ifelse(r$gender_respondent_r == "male", 1, 0)


##% of female headed households
r$hh2_i <- case_when( r$hhh == "yes" & r$gender_respondent_r == "female"| r$sex_head_hh == "female" ~ 1,
                      TRUE ~ 0)


##% of male headed households
r$hh2_ii <- case_when( (r$hhh == "yes" & r$gender_respondent_r == "male") | r$sex_head_hh == "male" ~ 1,
                       TRUE ~ 0)


##average household size
r$average_hh_size <- round(mean(r$hh_size, na.rm = TRUE),0)


##% of household by gender:females

r$per_female <- round((r$tot_female/r$hh_size)*100,0)

##% of household by gender:males

r$per_male <- round((r$tot_male/r$hh_size)*100,0)

## % of households by age groups:children

r$per_children <- round(((r$tot_boys + r$tot_girls)/r$hh_size)*100,0)

## % of household by age groups: adults
r$per_adults <- round((r$tot_adults / r$hh_size)*100,0)

#% of households with at least one adult member working in the 30 days prior to data collection

r$hh3 <- case_when(r$adult_employment_num >= 1 ~ 1, TRUE ~ 0)

# % of households with at least one child member working in the 30 days prior to data collection
r$hh4 <- case_when(r$child_employment_num >= 1 ~ 1, TRUE ~ 0)

# % of households with at least one member pregnant or lactating
r$hh5 <-case_when(r$preg_lact == "yes" ~ 1,
                  r$preg_lact == "no"~ 0,
                  TRUE ~ NA_real_)

# # % of households with at least one member with a disability
r$pwd_1 <- case_when( r$pwd == "yes" ~ 1,
                   TRUE ~ 0)

# % of households with at least one member with a chronic disease
r$chronic_illness <- case_when( r$chronic_disease == "yes" ~ 1,
                    TRUE ~ 0)

# % of households reporting being displaced as a result of the May Escalation
r$displaced_escalation <- case_when( r$displaced_escalation == "yes" ~ 1,
                          TRUE ~ 0)

# % of households by primary shelter type reported in the 12 months prior to data collection
r$hh6_i <- case_when(r$shelter_type == "apartment" ~ 1,
                         TRUE ~ 0)

r$hh6_ii <- case_when(r$shelter_type == "house" ~ 1,
                         TRUE ~ 0)

r$hh6_iii <- case_when(r$shelter_type == "unfinished_abandoned_building" ~ 1,
                     TRUE ~ 0)

r$hh6_iv <- case_when(r$shelter_type == "makeshift_shelter" ~ 1,
                       TRUE ~ 0)

r$hh6_v <- case_when(r$shelter_type == "collective_shelter" ~ 1,
                      TRUE ~ 0)
# % of households by primary shelter type reported in the 12 months prior to data collection
r$hh7_i <- case_when(r$occupancy_arrangement == "rented" ~ 1,
                     TRUE ~ 0)
r$hh7_ii <- case_when(r$occupancy_arrangement == "ownership" ~ 1,
                     TRUE ~ 0)
r$hh7_iii <- case_when(r$occupancy_arrangement == "hosted_without_rent" ~ 1,
                      TRUE ~ 0)

r$hh7_iv <- case_when(r$occupancy_arrangement == "squatting" ~ 1,
                       TRUE ~ 0)
# % of households reporting fear of eviction
r$hh8 <-case_when(r$fear_eviction == "yes" ~ 1,
                  r$fear_eviction == "no" ~ 0,
                  TRUE ~ NA_real_)

# % of HHs receiving other kinds of assistance
r$hh9 <- case_when(r$receive_other_assistance == "yes" ~ 1,
                   TRUE ~ 0)

# Type of other assistance received
r$food_unrwa <- case_when(r$receive_other_assistance_which == "food_unrwa" ~ 1,
                       is.na(r$receive_other_assistance_which) ~ NA_real_,
                       TRUE ~ 0)

r$cva_unrwa <- case_when(r$receive_other_assistance_which == "cva_unrwa" ~ 1,
                      is.na(r$receive_other_assistance_which) ~ NA_real_,
                      TRUE ~ 0)
r$cash_qatar <- case_when(r$receive_other_assistance_which == "cash_qatar" ~ 1,
                       is.na(r$receive_other_assistance_which) ~ NA_real_,
                       TRUE ~ 0)
r$food_ingo <- case_when(r$receive_other_assistance_which == "food_ingo" ~ 1,
                        is.na(r$receive_other_assistance_which) ~ NA_real_,
                        TRUE ~ 0)


r$food_wfp <- case_when(r$receive_other_assistance_which == "food_wfp" ~ 1,
                       is.na(r$receive_other_assistance_which) ~ NA_real_,
                       TRUE ~ 0)
r$cva_cbo <- case_when(r$receive_other_assistance_which == "cva_cbo" ~ 1,
                       is.na(r$receive_other_assistance_which) ~ NA_real_,
                       TRUE ~ 0)
r$cva_wfp <- case_when(r$receive_other_assistance_which == "cva_wfp" ~ 1,
                       is.na(r$receive_other_assistance_which) ~ NA_real_,
                       TRUE ~ 0)
r$cva_gov <- case_when(r$receive_other_assistance_which == "cva_gov" ~ 1,
                     is.na(r$receive_other_assistance_which) ~ NA_real_,
                     TRUE ~ 0)
# % of HHs reporting that transfer amount has met their basic needs
# % of HHs reporting that transfer amount has met their cash needs
r$amm1_i <- case_when(r$cash %in% c("3.5","5")~1,
                    r$cash %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their food needs
r$amm1_ii <- case_when(r$food %in% c("3.5","5")~1,
                    r$food %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their shelter needs
r$amm1_iii <- case_when(r$shelter %in% c("3.5","5")~1,
                    r$shelter %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their water needs
r$amm1_iv <- case_when(r$water %in% c("3.5","5")~1,
                    r$water %in% c("0","1.5")~0,
                    TRUE~ NA_real_)

# % of HHs reporting that transfer amount has met their hygiene needs
r$amm1_v <- case_when(r$hygiene %in% c("3.5","5")~1,
                    r$hygiene %in% c("0","1.5")~0,
                    TRUE~ NA_real_)

# % of HHs reporting that transfer amount has met their health needs
r$amm1_vi <- case_when(r$health %in% c("3.5","5")~1,
                    r$health %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their education needs
r$amm1_vii <- case_when(r$education %in% c("3.5","5")~1,
                    r$education %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their energy needs
r$amm1_viii <- case_when(r$energy %in% c("3.5","5")~1,
                    r$energy %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their transportation needs
r$amm1_ix <- case_when(r$transportation %in% c("3.5","5")~1,
                    r$transportation %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their hh_items needs
r$amm1_x <- case_when(r$hh_items %in% c("3.5","5")~1,
                    r$hh_items %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their communication needs
r$amm1_xi <- case_when(r$communication %in% c("3.5","5")~1,
                    r$communication %in% c("0","1.5")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their debt needs
r$amm1_xii <- case_when(r$debt %in% c("3.5","5")~1,
                    r$debt %in% c("0","1.5")~0,
                    TRUE~ NA_real_)


#replacing all expenditures to NA whose total expenses are NA
r$shelter_need <- case_when(r$shelter == "fully_able" ~ 5,
                  r$shelter == "partially_able" ~ 1.5,
                  r$shelter == "mostly_able" ~ 3.5,
                  r$shelter == "completely_unable" ~ 0,
                  TRUE ~ NA_real_)

r$food_need <- case_when(r$food == "fully_able" ~ 5,
                         r$food == "partially_able" ~ 1.5,
                         r$food == "mostly_able" ~ 3.5,
                         r$food == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)

r$water_need <- case_when(r$water == "fully_able" ~ 5,
                         r$water == "partially_able" ~ 1.5,
                         r$water == "mostly_able" ~ 3.5,
                         r$water == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)

r$hygiene_need <- case_when(r$hygiene == "fully_able" ~ 5,
                         r$hygiene == "partially_able" ~ 1.5,
                         r$hygiene == "mostly_able" ~ 3.5,
                         r$hygiene == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)

r$health_need <- case_when(r$health == "fully_able" ~ 5,
                         r$health == "partially_able" ~ 1.5,
                         r$health == "mostly_able" ~ 3.5,
                         r$health == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)


r$education_need <- case_when(r$education == "fully_able" ~ 5,
                         r$education == "partially_able" ~ 1.5,
                         r$education == "mostly_able" ~ 3.5,
                         r$education == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)


r$energy_need <- case_when(r$energy == "fully_able" ~ 5,
                         r$energy == "partially_able" ~ 1.5,
                         r$energy == "mostly_able" ~ 3.5,
                         r$energy == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)


r$transportation_need <- case_when(r$transportation == "fully_able" ~ 5,
                         r$transportation == "partially_able" ~ 1.5,
                         r$transportation == "mostly_able" ~ 3.5,
                         r$transportation == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)

r$hh_items_need <- case_when(r$hh_items == "fully_able" ~ 5,
                         r$hh_items == "partially_able" ~ 1.5,
                         r$hh_items == "mostly_able" ~ 3.5,
                         r$hh_items == "completely_unable" ~ 0,
                         TRUE ~ NA_real_)

r$communication_need <- case_when(r$communication == "fully_able" ~ 5,
                             r$communication == "partially_able" ~ 1.5,
                             r$communication == "mostly_able" ~ 3.5,
                             r$communication == "completely_unable" ~ 0,
                             TRUE ~ NA_real_)

# % of HHs reporting that transfer amount has met their basic needs
# % of HHs reporting that transfer amount has met their cash needs
r$amm1_i <- case_when(r$cash %in% c("3.5","5")~1,
                      r$cash %in% c("0","1.5")~0,
                      TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their food needs
r$amm1_ii <- case_when(r$food %in% c("3.5","5")~1,
                       r$food %in% c("0","1.5")~0,
                       TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their shelter needs
r$amm1_iii <- case_when(r$shelter %in% c("3.5","5")~1,
                        r$shelter %in% c("0","1.5")~0,
                        TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their water needs
r$amm1_iv <- case_when(r$water %in% c("3.5","5")~1,
                       r$water %in% c("0","1.5")~0,
                       TRUE~ NA_real_)

# % of HHs reporting that transfer amount has met their hygiene needs
r$amm1_v <- case_when(r$hygiene %in% c("3.5","5")~1,
                      r$hygiene %in% c("0","1.5")~0,
                      TRUE~ NA_real_)

# % of HHs reporting that transfer amount has met their health needs
r$amm1_vi <- case_when(r$health %in% c("3.5","5")~1,
                       r$health %in% c("0","1.5")~0,
                       TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their education needs
r$amm1_vii <- case_when(r$education %in% c("3.5","5")~1,
                        r$education %in% c("0","1.5")~0,
                        TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their energy needs
r$amm1_viii <- case_when(r$energy %in% c("3.5","5")~1,
                         r$energy %in% c("0","1.5")~0,
                         TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their transportation needs
r$amm1_ix <- case_when(r$transportation %in% c("3.5","5")~1,
                       r$transportation %in% c("0","1.5")~0,
                       TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their hh_items needs
r$amm1_x <- case_when(r$hh_items %in% c("3.5","5")~1,
                      r$hh_items %in% c("0","1.5")~0,
                      TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their communication needs
r$amm1_xi <- case_when(r$communication %in% c("3.5","5")~1,
                       r$communication %in% c("0","1.5")~0,
                       TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their debt needs
r$amm1_xii <- case_when(r$debt %in% c("3.5","5")~1,
                        r$debt %in% c("0","1.5")~0,
                        TRUE~ NA_real_)





r$total_need <- r$shelter_need + r$food_need + r$water_need + r$hygiene_need + r$health_need + r$education_need + r$energy_need + r$transportation_need + r$hh_items_need + r$communication_need

r$average_need <-round(r$total_need/50, 2)

r$msi <- case_when(r$average_need <= 0.50 ~ 0,
                   r$average_need > 0.50 ~1,
                   TRUE ~ NA_real_)

# Based on the average MNI scores, households can be defined “not able” (0.0 – 0.25), only “partially able” (0.26 – 0.50), “mostly 
# able” (0.51 – 0.75), or “fully able” (0.76 – 1)



# % of HHs reporting that transfer amount has met their needs
r$amm1 <- case_when(r$amm1_i == 1 | r$amm1_ii == 1 | r$amm1_iii == 1 | r$amm1_iv == 1 | r$amm1_v == 1 | r$amm1_vi == 1 | r$amm1_vii == 1 |
                      r$amm1_viii == 1 | r$amm1_ix == 1 | r$amm1_x == 1 | r$amm1_xi == 1 | r$amm1_xii == 1 ~ 1,
                    r$amm1_i == 0 | r$amm1_ii == 0 | r$amm1_iii == 0 | r$amm1_iv == 0 | r$amm1_v == 0 | r$amm1_vi == 0 | r$amm1_vii == 0 |
                      r$amm1_viii == 0 | r$amm1_ix == 0 | r$amm1_x == 0 | r$amm1_xi == 0 | r$amm1_xii == 0 ~ 0,
                    TRUE ~ NA_real_)
# % of HHs reporting primary barriers to their cash needs
r$amm2_i <- case_when(r$cash_barriers.insufficient_money == 1 | r$cash_barriers.distance == 1 | r$cash_barriers.security_situation == 1 |
                        r$cash_barriers.social_discrimination == 1 | r$cash_barriers.insufficient_goods == 1 | r$cash_barriers.poor_quality == 1 ~ 1,
                      r$cash_barriers.insufficient_money == 0 | r$cash_barriers.distance == 0 | r$cash_barriers.security_situation == 0 |
                        r$cash_barriers.social_discrimination == 0 | r$cash_barriers.insufficient_goods == 0 | r$cash_barriers.poor_quality == 0 ~ 0,
                      TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their food needs
r$amm2_ii <- case_when(r$food_barriers.insufficient_money == 1 | r$food_barriers.distance == 1 | r$food_barriers.security_situation == 1 |
                         r$food_barriers.social_discrimination == 1 | r$food_barriers.insufficient_goods == 1 | r$food_barriers.poor_quality == 1 ~ 1,
                       r$food_barriers.insufficient_money == 0 | r$food_barriers.distance == 0 | r$food_barriers.security_situation == 0 |
                         r$food_barriers.social_discrimination == 0 | r$food_barriers.insufficient_goods == 0 | r$food_barriers.poor_quality == 0 ~ 0,
                       TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their shelter needs
r$amm2_iii <- case_when(r$shelter_barriers.insufficient_money == 1 | r$shelter_barriers.distance == 1 | r$shelter_barriers.security_situation == 1 |
                          r$shelter_barriers.social_discrimination == 1 | r$shelter_barriers.insufficient_goods == 1 | r$shelter_barriers.poor_quality == 1 ~ 1,
                        r$shelter_barriers.insufficient_money == 0 | r$shelter_barriers.distance == 0 | r$shelter_barriers.security_situation == 0 |
                          r$shelter_barriers.social_discrimination == 0 | r$shelter_barriers.insufficient_goods == 0 | r$shelter_barriers.poor_quality == 0 ~ 0,
                        TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their water needs
r$amm2_iv <- case_when(r$water_barriers.insufficient_money == 1 | r$water_barriers.distance == 1 | r$water_barriers.security_situation == 1 |
                         r$water_barriers.social_discrimination == 1 | r$water_barriers.insufficient_goods == 1 | r$water_barriers.poor_quality == 1 ~ 1,
                       r$water_barriers.insufficient_money == 0 | r$water_barriers.distance == 0 | r$water_barriers.security_situation == 0 |
                         r$water_barriers.social_discrimination == 0 | r$water_barriers.insufficient_goods == 0 | r$water_barriers.poor_quality == 0 ~ 0,
                       TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their hygiene needs
r$amm2_v <- case_when(r$hygiene_barriers.insufficient_money == 1 | r$hygiene_barriers.distance == 1 | r$hygiene_barriers.security_situation == 1 |
                        r$hygiene_barriers.social_discrimination == 1 | r$hygiene_barriers.insufficient_goods == 1 | r$hygiene_barriers.poor_quality == 1 ~ 1,
                      r$hygiene_barriers.insufficient_money == 0 | r$hygiene_barriers.distance == 0 | r$hygiene_barriers.security_situation == 0 |
                        r$hygiene_barriers.social_discrimination == 0 | r$hygiene_barriers.insufficient_goods == 0 | r$hygiene_barriers.poor_quality == 0 ~ 0,
                      TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their health needs
r$amm2_vi <- case_when(r$health_barriers.insufficient_money == 1 | r$health_barriers.distance == 1 | r$health_barriers.security_situation == 1 |
                         r$health_barriers.social_discrimination == 1 | r$health_barriers.insufficient_goods == 1 | r$health_barriers.poor_quality == 1 ~ 1,
                       r$health_barriers.insufficient_money == 0 | r$health_barriers.distance == 0 | r$health_barriers.security_situation == 0 |
                         r$health_barriers.social_discrimination == 0 | r$health_barriers.insufficient_goods == 0 | r$health_barriers.poor_quality == 0 ~ 0,
                       TRUE ~ NA_real_)
# % of HHs reporting primary barriers to their education needs
r$amm2_vii <- case_when(r$edu_barriers.insufficient_money == 1 | r$edu_barriers.distance == 1 | r$edu_barriers.security_situation == 1 |
                          r$edu_barriers.social_discrimination == 1 | r$edu_barriers.insufficient_goods == 1 | r$edu_barriers.poor_quality == 1 ~ 1,
                        r$edu_barriers.insufficient_money == 0 | r$edu_barriers.distance == 0 | r$edu_barriers.security_situation == 0 |
                          r$edu_barriers.social_discrimination == 0 | r$edu_barriers.insufficient_goods == 0 | r$edu_barriers.poor_quality == 0 ~ 0,
                        TRUE ~ NA_real_)
# % of HHs reporting primary barriers to their energy needs
r$amm2_Viii <- case_when(r$energy_barriers.insufficient_money == 1 | r$energy_barriers.distance == 1 | r$energy_barriers.security_situation == 1 |
                           r$energy_barriers.social_discrimination == 1 | r$energy_barriers.insufficient_goods == 1 | r$energy_barriers.poor_quality == 1 ~ 1,
                         r$energy_barriers.insufficient_money == 0 | r$energy_barriers.distance == 0 | r$energy_barriers.security_situation == 0 |
                           r$energy_barriers.social_discrimination == 0 | r$energy_barriers.insufficient_goods == 0 | r$energy_barriers.poor_quality == 0 ~ 0,
                         TRUE ~ NA_real_)
# % of HHs reporting primary barriers to their transportation needs
r$amm2_ix <- case_when(r$transportation_barriers.insufficient_money == 1 | r$transportation_barriers.distance == 1 | r$transportation_barriers.security_situation == 1 |
                         r$transportation_barriers.social_discrimination == 1 | r$transportation_barriers.insufficient_goods == 1 | r$transportation_barriers.poor_quality == 1 ~ 1,
                       r$transportation_barriers.insufficient_money == 0 | r$transportation_barriers.distance == 0 | r$transportation_barriers.security_situation == 0 |
                         r$transportation_barriers.social_discrimination == 0 | r$transportation_barriers.insufficient_goods == 0 | r$transportation_barriers.poor_quality == 0 ~ 0,
                       TRUE ~ NA_real_)
# % of HHs reporting primary barriers to their hh_items needs
r$amm2_x <- case_when(r$hh_items_barriers.insufficient_money == 1 | r$hh_items_barriers.distance == 1 | r$hh_items_barriers.security_situation == 1 |
                        r$hh_items_barriers.social_discrimination == 1 | r$hh_items_barriers.insufficient_goods == 1 | r$hh_items_barriers.poor_quality == 1 ~ 1,
                      r$hh_items_barriers.insufficient_money == 0 | r$hh_items_barriers.distance == 0 | r$hh_items_barriers.security_situation == 0 |
                        r$hh_items_barriers.social_discrimination == 0 | r$hh_items_barriers.insufficient_goods == 0 | r$hh_items_barriers.poor_quality == 0 ~ 0,
                      TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their communication needs
r$amm2_xi <- case_when(r$comms_barriers.insufficient_money == 1 | r$comms_barriers.distance == 1 | r$comms_barriers.security_situation == 1 |
                         r$comms_barriers.social_discrimination == 1 | r$comms_barriers.insufficient_goods == 1 | r$comms_barriers.poor_quality == 1 ~ 1,
                       r$comms_barriers.insufficient_money == 0 | r$comms_barriers.distance == 0 | r$comms_barriers.security_situation == 0 |
                         r$comms_barriers.social_discrimination == 0 | r$comms_barriers.insufficient_goods == 0 | r$comms_barriers.poor_quality == 0 ~ 0,
                       TRUE ~ NA_real_)

# % of HHs reporting primary barriers to their debt needs
r$amm2_Xii <- case_when(r$debt_barriers.insufficient_money == 1 | r$debt_barriers.distance == 1 | r$debt_barriers.security_situation == 1 |
                          r$debt_barriers.social_discrimination == 1 | r$debt_barriers.insufficient_goods == 1 | r$debt_barriers.poor_quality == 1 ~ 1,
                        r$debt_barriers.insufficient_money == 0 | r$debt_barriers.distance == 0 | r$debt_barriers.security_situation == 0 |
                          r$debt_barriers.social_discrimination == 0 | r$debt_barriers.insufficient_goods == 0 | r$debt_barriers.poor_quality == 0 ~ 0,
                        TRUE ~ NA_real_)

# % of HHs reporting taking one of the following behaviors or activities during the duration they received MPCA
# Pay down household debt in part
r$amm3_i <- case_when(r$pay_debt_partly == "yes" ~ 1,
                      r$pay_debt_partly == "no" ~ 0,
                      TRUE ~ NA_real_)
# Pay down household debt in full
r$amm3_ii <- case_when(r$pay_debt_full == "yes" ~ 1,
                      r$pay_debt_full == "no" ~ 0,
                      TRUE ~ NA_real_)
# Not take on any new debts
r$amm3_iii <- case_when(r$no_new_debt == "yes" ~ 1,
                      r$no_new_debt == "no" ~ 0,
                      TRUE ~ NA_real_)
# Replenish or start a savings (formal or informal)
r$amm3_iv <- case_when(r$savings == "yes" ~ 1,
                      r$savings == "no" ~ 0,
                      TRUE ~ NA_real_)
# Purchase/repair/replacement of at least one productive asset
r$amm3_v <- case_when(r$purchase_asset == "yes" ~ 1,
                      r$purchase_asset == "no" ~ 0,
                      TRUE ~ NA_real_)
# Establishment or strengthening of income-generating activities
r$amm3_vi <- case_when(r$establish_income_generation == "yes" ~ 1,
                      r$establish_income_generation == "no" ~ 0,
                      TRUE ~ NA_real_)
# Repair or replace a significant NFI (e.g. oven, heater, car, refrigerator)
r$amm3_vii <- case_when(r$new_nfi == "yes" ~ 1,
                      r$new_nfi == "no" ~ 0,
                      TRUE ~ NA_real_)

# % of HHs attributing the ability to pursue one of the above behaviors or activities on account of MPCA
# Pay down household debt in part
r$amm4_i <- case_when(r$pay_debt_partly_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$pay_debt_partly_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# Pay down household debt in full
r$amm4_ii <- case_when(r$pay_debt_full_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$pay_debt_full_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# Not take on any new debts
r$amm4_iii <- case_when(r$no_new_debt_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$no_new_debt_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# Replenish or start a savings (formal or informal)
r$amm4_iv <- case_when(r$savings_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$savings_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# Purchase/repair/replacement of at least one productive asset
r$amm4_v<- case_when(r$purchase_asset_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$purchase_asset_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# Establishment or strengthening of income-generating activities
r$amm4_vi <- case_when(r$establish_income_generation_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$establish_income_generation_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# Repair or replace a significant NFI (e.g. oven, heater, car, refrigerator)
r$amm4_vii <- case_when(r$new_nfi_result %in% c("completely_mpca", "largely_mpca") ~ 1,
                      r$new_nfi_result %in% c("partly_mpca", "not_mpca") ~ 0,
                      TRUE~NA_real_)

# % of HHs attributing the ability to pursue one of the above behaviors or activities on account of MPCA
r$amm4 <- case_when( r$amm4_i == 1|r$amm4_ii == 1| r$amm4_iii == 1| r$amm4_iv == 1 | r$amm4_v == 1 | r$amm4_vi == 1 | r$amm4_vii == 1 ~ 1,
                     r$amm4_i == 0|r$amm4_ii == 0| r$amm4_iii == 0| r$amm4_iv == 0 | r$amm4_v == 0 | r$amm4_vi == 0 | r$amm4_vii == 0 ~ 0,
                     TRUE ~ NA_real_)


# % of HHs reporting that cash distribution frequency was appropriate enough
r$access1 <- case_when(r$transfer_alignment %in% c("completely_aligned","mostly" ) ~ 1,
                       r$transfer_alignment %in% c("partially","misaligned" ) ~ 0,
                       TRUE ~ NA_real_)

# % of HHs reporting primary reasons frequency of distributions were unaligned with household needs by primary reason.
# Most HH expenses were incurred at a more frequent basis (e.g. weekly, biweekly)
r$access2_i <- case_when(r$transfer_misaligned_why == "expenses_incuured_frequently" ~ 1,
                         is.na(r$transfer_misaligned_why)~ NA_real_,
                         TRUE ~ 0)

# HHs prefer to receive smaller amounts (weekly)
r$access2_ii <- case_when(r$transfer_misaligned_why == "prefer_cash_weekly" ~ 1,
                         is.na(r$transfer_misaligned_why)~ NA_real_,
                         TRUE ~ 0)

# HHs prefer to receive smaller amounts (bi-weekly)
r$access2_iii <- case_when(r$transfer_misaligned_why == "prefer_cash_bi_weekly" ~ 1,
                          is.na(r$transfer_misaligned_why)~ NA_real_,
                          TRUE ~ 0)

# % of HHs reporting that cash distribution timing was appropriate enough
r$access3 <- case_when(r$transfer_timing %in% c("high_degree","some_degree") ~ 1,
                       r$transfer_timing %in% c("limited_degree","not_appropriate") ~ 0)

# % of HHs reporting that MPCA supported in meeting HH needs following the May Escalation
r$shocks2 <- case_when(r$mpca_help_escalation_priorities %in% c("great_help","some_help") ~ 1,
                       r$mpca_help_escalation_priorities %in% c("did_not_help_much","did_not_help_at_all") ~ 0)

# % of HHs reporting reason why MPCA did not help in meeting HH needs following the May Escalation
r$shock3 <- case_when(r$mpca_no_help_escalation_priorities.value_distributed_insufficient == 1|r$mpca_no_help_escalation_priorities.atm_out_of_service == 1|
                      r$mpca_no_help_escalation_priorities.unable_to_travel == 1 | r$mpca_no_help_escalation_priorities.atm_insecure == 1 |
                      r$mpca_no_help_escalation_priorities.cah_not_the_best_modality == 1 | r$mpca_no_help_escalation_priorities.cash_not_timely == 1 ~ 1,
                      TRUE ~ 0)
# % of households reporting experiencing at least one shock during the period they received MPCA, excluding the May Escalation
r$shock5 <- case_when(r$exp_shock == "yes" ~ 1,
                      TRUE ~ 0)

# % of HHs reporting reason why MPCA did not help in meeting HH needs following the May Escalation by reason
r$shock3_i <- r$mpca_no_help_escalation_priorities.value_distributed_insufficient
r$shock3_ii <- r$mpca_no_help_escalation_priorities.atm_out_of_service
r$shock3_iii <- r$mpca_no_help_escalation_priorities.unable_to_travel
r$shock3_iv <- r$mpca_no_help_escalation_priorities.atm_insecure
r$shock3_v <- r$mpca_no_help_escalation_priorities.cah_not_the_best_modality
r$shock3_vi <- r$mpca_no_help_escalation_priorities.cash_not_timely

# % of household reporting effect of MPCA on ability to withstand shocks:shock of medical expenses


r$shocks6_i <- case_when(r$role_mpca_med %in% c("great_extend","some_extent") ~ 1,
                       r$role_mpca_med %in% c("some_extent","limited_extent") ~ 0,
                       TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of loss of income
r$shocks6_ii <- case_when(r$role_mpca_loss_employ %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_loss_employ %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of loss of inventory or produce
# r$shocks6_iii <- case_when(r$role_mpca_loss_inventory %in% c("great_extend","some_extent") ~ 1,
#                          r$role_mpca_loss_inventory %in% c("some_extent","limited_extent") ~ 0,
#                          TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of arrival of new household members
r$shocks6_iv <- case_when(r$role_mpca_new_member %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_new_member %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of need to repair or replace a household item
r$shocks6_v <- case_when(r$role_mpca_repair_nfi %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_repair_nfi %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of repairing a productive asset
r$shocks6_vi <- case_when(r$role_mpca_repair_asset %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_repair_asset %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of education expenses
r$shocks6_vii <- case_when(r$role_mpca_edu %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_edu %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of shelter expenses
r$shocks6_viii <- case_when(r$role_mpca_shelter %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_shelter %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)

# % of household reporting effect of MPCA on ability to withstand shocks:shock of debt payment
r$shocks6_ix <- case_when(r$role_mpca_debt %in% c("great_extend","some_extent") ~ 1,
                         r$role_mpca_debt %in% c("some_extent","limited_extent") ~ 0,
                         TRUE ~ NA_real_)
# % of household reporting effect of MPCA on ability to withstand shocks

r$shocks6 <- case_when(r$shocks6_i == 1 | r$shocks6_ii== 1| r$shocks6_iv == 1 |r$shocks6_v == 1|r$shocks6_vi == 1|r$shocks6_vii == 1|r$shocks6_viii == 1 ~ 1,
                      r$shocks6_i == 0 | r$shocks6_ii== 0| r$shocks6_iv == 0 |r$shocks6_v == 0|r$shocks6_vi == 0|r$shocks6_vii == 0|r$shocks6_viii == 0 ~ 0,
                      TRUE ~ NA_real_)



# % of HHs by reported income sources
# Salary from regular employment (private sector)
r$hh21_i <- case_when(r$income_sources == "salary_private" ~ 1,
                      TRUE ~ 0)

# Salary from regular employment (public sector)
r$hh21_ii <- case_when(r$income_sources == "salary_public" ~ 1,
                      TRUE ~ 0)

# Salary from regular employment (NGO/UN)
r$hh21_iii <- case_when(r$income_sources == "salary_un" ~ 1,
                      TRUE ~ 0)

# Temporary , seasonal, or daily wages
r$hh21_iv <- case_when(r$income_sources == "temporary_daily" ~ 1,
                      TRUE ~ 0)

# Own business
r$hh21_v <- case_when(r$income_sources == "own_business" ~ 1,
                      TRUE ~ 0)

# Selling household assets
r$hh21_vi <- case_when(r$income_sources == "sell_assets" ~ 1,
                      TRUE ~ 0)

# Retirement fund / pension
r$hh21_vii <- case_when(r$income_sources == "pension" ~ 1,
                      TRUE ~ 0)

# Loans, debt (including store credit)
r$hh21_viii <- case_when(r$income_sources == "loans" ~ 1,
                      TRUE ~ 0)

# MoSD cash assistance
r$hh21_ix <- case_when(r$income_sources == "mosd" ~ 1,
                      TRUE ~ 0)

# UNRWA cash assistance
r$hh21_x <- case_when(r$income_sources == "unrwa" ~ 1,
                      TRUE ~ 0)
# NGO cash assistance
r$hh21_xi <- case_when(r$income_sources == "ngo" ~ 1,
                      TRUE ~ 0)

# Selling in-kind assistance
r$hh21_xii <- case_when(r$income_sources == "sell_assistance" ~ 1,
                      TRUE ~ 0)

# Support from community, friends, or family
r$hh21_xiii <- case_when(r$income_sources == "community" ~ 1,
                      TRUE ~ 0)

# Illegal or socially degrading activities
r$hh21_xiv <- case_when(r$income_sources == "illegal" ~ 1,
                      TRUE ~ 0)

# Labor inside Israel
r$hh21_xv <- case_when(r$income_sources == "israel" ~ 1,
                      TRUE ~ 0)

# % of HHs by reported expenditures
# Food
r$hh22_i <- case_when(r$food_exp > 0 ~ 1,
                      TRUE ~ 0)

# Clothing and shoes 
r$hh22_ii <- case_when(r$clothing_exp > 0 ~ 1,
                       TRUE ~ 0)

# housing
r$hh22_iii <- case_when(r$housing_exp > 0 ~ 1,
                        TRUE ~ 0)

# home appliances 
r$hh22_iv <- case_when(r$appliances_exp > 0 ~ 1,
                       TRUE ~ 0)

# Debt repayment
r$hh22_v <- case_when(r$debt_repayment > 0 ~ 1,
                      TRUE ~ 0)

# house needs 
r$hh22_vi <- case_when(r$house_needs_exp > 0 ~ 1,
                       TRUE ~ 0)

# health care 
r$hh22_vii <- case_when(r$health_exp > 0 ~ 1,
                        TRUE ~ 0)
# transportation
r$hh22_viii <- case_when(r$transp_exp > 0 ~ 1,
                         TRUE ~ 0)

# communication
r$hh22_ix <- case_when(r$communication_exp > 0 ~ 1,
                       TRUE ~ 0)

# cultural and recreational activities 
r$hh22_x <- case_when(r$recreation_exp > 0 ~ 1,
                      TRUE ~ 0)
# personal care 
r$hh22_xi <- case_when(r$personal_exp > 0 ~ 1,
                       TRUE ~ 0)

# cigarettes and tobacco
r$hh22_xii <- case_when(r$cigarettes_exp > 0 ~ 1,
                        TRUE ~ 0)

# electricity including bells, fuels, repairs
r$hh22_xiii <- case_when(r$electricity_exp > 0 ~ 1,
                         TRUE ~ 0)



###Average income
# Food
# r$hh23_i <- round(mean(r$food_exp, na.rm = TRUE),0)
r$food_share <- round((as.numeric(r$food_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_i <- ifelse(r$food_share > 100, NA, 
                 r$food_share)


r$clothing_share <- round((as.numeric(r$clothing_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_ii <- ifelse(r$clothing_share > 100, NA, 
                    r$clothing_share)


r$housing_share <- round((as.numeric(r$housing_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_iii <- ifelse(r$housing_share > 100, NA, 
                    r$housing_share)



r$appliance_share <- round((as.numeric(r$appliances_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_iv <- ifelse(r$appliance_share > 100, NA, 
                    r$appliance_share)



r$debt_share <- round((as.numeric(r$debt_repayment)/ as.numeric(r$total_exp))*100, 0)
r$hh23_v <- ifelse(r$debt_share > 100, NA, 
                    r$debt_share)





r$needs_share <- round((as.numeric(r$house_needs_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_vi <- ifelse(r$needs_share > 100, NA, 
                   r$needs_share)




r$health_share <- round((as.numeric(r$health_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_vii <- ifelse(r$health_share > 100, NA, 
                    r$health_share)




r$trans_share <- round((as.numeric(r$transp_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_viii <- ifelse(r$trans_share > 100, NA, 
                     r$trans_share)




r$comms_share <- round((as.numeric(r$communication_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_ix <- ifelse(r$comms_share > 100, NA, 
                      r$comms_share)




r$recreation_share <- round((as.numeric(r$recreation_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_x <- ifelse(r$recreation_share > 100, NA, 
                    r$recreation_share)




r$personal_share <- round((as.numeric(r$personal_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_xi <- ifelse(r$personal_share > 100, NA, 
                   r$personal_share)



r$cigarettes_share <- round((as.numeric(r$cigarettes_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_xii <- ifelse(r$cigarettes_share > 100, NA, 
                    r$cigarettes_share)



r$electricity_share <- round((as.numeric(r$electricity_exp)/ as.numeric(r$total_exp))*100, 0)
r$hh23_xiii <- ifelse(r$electricity_share > 100, NA, 
                    r$electricity_share)


r$hh24 <- round(mean(r$total_exp, na.rm = TRUE),0)




##% HH relying on stress / crisis / emergency strategies to cope with a lack of food or money to buy it


r$hh25_i <- case_when(r$selling_hh_properties %in% c("no_already_did", "yes") ~ 1,
                      r$selling_hh_properties %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)

r$hh25_ii <- case_when(r$spent_savings %in% c("no_already_did", "yes") ~ 1,
                      r$spent_savings %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_iii <- case_when(r$buying_goods_on_credit %in% c("no_already_did", "yes") ~ 1,
                      r$buying_goods_on_credit %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)

r$hh25_iv <- case_when(r$hh_members_ate_eslewhere %in% c("no_already_did", "yes") ~ 1,
                      r$hh_members_ate_eslewhere %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_v <- case_when(r$selling_productive_assets %in% c("no_already_did", "yes") ~ 1,
                      r$selling_productive_assets %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_vi <- case_when(r$withdrew_children_from_school %in% c("no_already_did", "yes") ~ 1,
                      r$withdrew_children_from_school %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_vii <- case_when(r$school_drop_out %in% c("no_already_did", "yes") ~ 1,
                      r$school_drop_out %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_viii <- case_when(r$reduced_health_expenses %in% c("no_already_did", "yes") ~ 1,
                      r$reduced_health_expenses %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_ix <- case_when(r$engaging_in_risky_behaviour %in% c("no_already_did", "yes") ~ 1,
                      r$engaging_in_risky_behaviour %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)
r$hh25_x <- case_when(r$sold_house_land %in% c("no_already_did", "yes") ~ 1,
                      r$sold_house_land %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)

r$hh25_xi <- case_when(r$hh_migrated %in% c("no_already_did","yes") ~ 1,
                      r$hh_migrated %in% c("not_applicable", "no_no_one_in_HH") ~ 0,
                      TRUE ~ NA_real_)

r$hh25 <- case_when(
  r$hh25_i == 1 |
  r$hh25_ii == 1 |
  r$hh25_iii  == 1 |
  r$hh25_iv  == 1 |
  r$hh25_v  == 1 |
  r$hh25_vi  == 1 |
  r$hh25_vii  == 1 |
  r$hh25_viii  == 1 |
  r$hh25_ix  == 1 |
  r$hh25_x  == 1 |
  r$hh25_xi  == 1 ~ 1,
  r$hh25_i == 0 |
  r$hh25_ii == 0 |
  r$hh25_iii  == 0 |
  r$hh25_iv  == 0 |
  r$hh25_v  == 0 |
  r$hh25_vi  == 0 |
  r$hh25_vii  == 0 |
  r$hh25_viii  == 0 |
  r$hh25_ix  == 0 |
  r$hh25_x  == 0 |
  r$hh25_xi  == 0 ~ 0,
  TRUE ~ NA_real_
)

# % of households reporting how their financial situation compares to before receiving MPCA

r$impact2_improved <- case_when(r$change_financial_situation %in% c("improved_significantly","improved_somewhat")~ 1,
                                TRUE ~ 0)

r$impact2_constant <- case_when(r$change_financial_situation == "same_as_before" ~ 1,
                                TRUE ~ 0)

r$impact2_worsened <- case_when(r$change_financial_situation %in% c("worsened_significantly","worsened_somewhat") ~ 1,
                                TRUE ~ 0)
# % of households attributing their improved financial situation to My Choice 
r$impact3 <- case_when(r$change_financial_situation_mpca %in% c("great_help","some_help") ~ 1,
                     r$change_financial_situation_mpca %in% c("did_not_help_much","did_not_help_at_all") ~ 0,
                     TRUE ~ NA_real_)
# % of households reporting how the mental state of household members compares to before receiving MPCA

r$impact4_improved <- case_when(r$change_psych_situation %in% c("improved_significantly","improved_somewhat")~ 1,
                                r$change_psych_situation == "don_t_know" ~ NA_real_,
                                TRUE ~ 0)

r$impact4_constant <- case_when(r$change_psych_situation == "same_as_before" ~ 1,
                                r$change_psych_situation == "don_t_know" ~ NA_real_,
                                TRUE ~ 0)

r$impact4_worsened <- case_when(r$change_psych_situation %in% c("worsened_significantly","worsened_somewhat") ~ 1,
                                r$change_psych_situation == "don_t_know" ~ NA_real_,
                                TRUE ~ 0)
# % of households attributing their improved mental state to My Choice
r$impact5 <- case_when(r$change_psych_situation_mpca %in% c("great_help","some_help") ~ 1,
                     r$change_psych_situation_mpca %in% c("did_not_help_much","did_not_help_at_all")~0,
                     TRUE ~ NA_real_)

# % of HHs reporting  different changes in inter-HH behavior as a result of the MPCA by type of activity
# Joint decision-making (e.g. more than one HH member) on household expenses
# r$impact6_i <- case_when(r$joint_decision_making %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
#                      r$joint_decision_making == "neither_increased_decreased" ~ "constant",
#                      r$joint_decision_making %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
#                      TRUE ~ NA_character_)

r$p6_i_increased <- case_when(r$joint_decision_making %in% c("increased_to_some_extent","increased_greatly")~ 1,
                                r$joint_decision_making == "prefer_not_to_answer" ~ NA_real_,
                                TRUE ~ 0)

r$p6_i_constant <- case_when(r$joint_decision_making == "neither_increased_decreased" ~ 1,
                                r$joint_decision_making == "prefer_not_to_answer" ~ NA_real_,
                                TRUE ~ 0)

r$p6_i_decreased <- case_when(r$joint_decision_making %in% c("decreased_some_extent","decreased_greatly") ~ 1,
                                r$joint_decision_making == "prefer_not_to_answer" ~ NA_real_,
                                TRUE ~ 0)

# Joint decision-making on whether to take loans
# r$impact6_ii <- case_when(r$joint_loans %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
#                        r$joint_loans == "neither_increased_decreased" ~ "constant",
#                        r$joint_loans %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
#                        TRUE ~ NA_character_)



r$p6_ii_increased <- case_when(r$joint_loans %in% c("increased_to_some_extent","increased_greatly")~ 1,
                              r$joint_loans %in% c ("prefer_not_to_answer","don_t_know") ~ NA_real_,
                              TRUE ~ 0)

r$p6_ii_constant <- case_when(r$joint_loans == "neither_increased_decreased" ~ 1,
                             r$joint_loans %in% c ("prefer_not_to_answer","don_t_know") ~ NA_real_,
                             TRUE ~ 0)

r$p6_ii_decreased <- case_when(r$joint_loans %in% c("decreased_some_extent","decreased_greatly") ~ 1,
                              r$joint_loans %in% c ("prefer_not_to_answer","don_t_know") ~ NA_real_,
                              TRUE ~ 0)
# Joint decision-making on responsibilities of household members (e.g. work, care of the elderly)
# r$impact6_iii <- case_when(r$joint_responsibilities %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
#                        r$joint_responsibilities == "neither_increased_decreased" ~ "constant",
#                        r$joint_responsibilities %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
#                        TRUE ~ NA_character_)

r$p6_iii_increased <- case_when(r$joint_responsibilities %in% c("increased_to_some_extent","increased_greatly")~ 1,
                               r$joint_responsibilities == "don_t_know" ~ NA_real_,
                               is.na(r$joint_responsibilities) ~NA_real_,
                               TRUE ~ 0)

r$p6_iii_constant <- case_when(r$joint_responsibilities == "neither_increased_decreased" ~ 1,
                               r$joint_responsibilities == "don_t_know" ~ NA_real_,
                               is.na(r$joint_responsibilities) ~NA_real_,
                              TRUE ~ 0)

r$p6_iii_decreased <- case_when(r$joint_responsibilities %in% c("decreased_some_extent","decreased_greatly") ~ 1,
                                r$joint_responsibilities == "don_t_know" ~ NA_real_,
                                is.na(r$joint_responsibilities) ~NA_real_,
                               TRUE ~ 0)

# Disputes within household on how to spend money
# r$impact6_iv <- case_when(r$spending_disputes %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
#                        r$spending_disputes == "neither_increased_decreased" ~ "constant",
#                        r$spending_disputes %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
#                        TRUE ~ NA_character_)
r$p6_iv_increased <- case_when(r$spending_disputes %in% c("increased_to_some_extent","increased_greatly")~ 1,
                                r$spending_disputes %in% c ("prefer_not_to_answer","don_t_know") ~ NA_real_,
                                TRUE ~ 0)

r$p6_iv_constant <- case_when(r$spending_disputes == "neither_increased_decreased" ~ 1,
                               r$spending_disputes %in% c ("prefer_not_to_answer","don_t_know") ~ NA_real_,
                               TRUE ~ 0)

r$p6_iv_decreased <- case_when(r$spending_disputes %in% c("decreased_some_extent","decreased_greatly") ~ 1,
                                r$spending_disputes %in% c ("prefer_not_to_answer","don_t_know") ~ NA_real_,
                                TRUE ~ 0)

# Disputes with creditors
# r$impact6_v <- case_when(r$disputes_with_creditors %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
#                        r$disputes_with_creditors == "neither_increased_decreased" ~ "constant",
#                        r$disputes_with_creditors %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
#                        TRUE ~ NA_character_)

r$p6_v_increased <- case_when(r$disputes_with_creditors %in% c("increased_to_some_extent","increased_greatly")~ 1,
                              TRUE ~ 0)

r$p6_v_constant <- case_when(r$disputes_with_creditors == "neither_increased_decreased" ~ 1,
                              TRUE ~ 0)

r$p6_v_decreased <- case_when(r$disputes_with_creditors %in% c("decreased_some_extent","decreased_greatly") ~ 1,
                               TRUE ~ 0)


# % of households reporting an increase in household member dignity as a result of My Choice
r$impact7 <- case_when(r$change_dignity_mpca == "yes" ~ 1,
                       TRUE ~ 0)
# % of households reporting changes with relatives as a result of receiving MPCA through My Choice

r$s1_improved <- case_when(r$change_relations_relatives %in% c("relation_improved_significantly","relation_improved_some_extend") ~ 1,
                             TRUE ~ 0)
r$s1_constant <- case_when(r$change_relations_relatives == "not_changed" ~ 1,
                             TRUE ~ 0)
r$s1_worsened <- case_when(r$change_relations_relatives %in% c("relation_worsened_some_extent","relation_worsened_significantly") ~1,
                             TRUE ~ 0)

# % of households reporting changes with other households or community members since receiving MPCA through My Choice


r$s3_improved <- case_when(r$changes_relations_community %in% c("relation_improved_significantly","relation_improved_some_extend") ~ 1,
                             TRUE ~ 0)
r$s3_constant <- case_when(r$changes_relations_community == "not_changed" ~ 1,
                             TRUE ~ 0)
r$s3_worsened <- case_when(r$changes_relations_community %in% c("relation_worsened_some_extent","relation_worsened_significantly") ~1,
                             TRUE ~ 0)

# % of households who have incurred debts in the past 30 days
r$s5 <- case_when(r$debts == "yes" ~ 1,
                  TRUE ~ 0)
  

# % of HHs filing complaints
r$aap1 <- case_when( r$use_complaint_mechanisms == "yes" ~ 1,
                   r$use_complaint_mechanisms == "no" ~ 0,
                   TRUE~ NA_real_)
# % HHs with access/knowledge of complaint mechanisms 
r$aap <- case_when(r$complaint_mechanisms == "yes" ~ 1,
                 r$complaint_mechanisms == "no" ~ 0,
                 TRUE~ NA_real_)
# % of households reporting time to receive response to complaints
r$aap2_i <- case_when(r$time_resolve_complaint == "less_than_two" ~ 1,
                    is.na(r$time_resolve_complaint) ~ NA_real_,
                    TRUE ~ 0)
r$aap2_ii <- case_when(r$time_resolve_complaint == "less_than_three" ~ 1,
                    is.na(r$time_resolve_complaint) ~ NA_real_,
                    TRUE ~ 0)
r$aap2_iii <- case_when(r$time_resolve_complaint == "three_weeks_or_more" ~ 1,
                    is.na(r$time_resolve_complaint) ~ NA_real_,
                    TRUE ~ 0)
r$aap2_iv <- case_when(r$time_resolve_complaint == "less_than_a_week" ~ 1,
                    is.na(r$time_resolve_complaint) ~ NA_real_,
                    TRUE ~ 0)
r$aap2_v <- case_when(r$time_resolve_complaint == "not_addressed" ~ 1,
                    is.na(r$time_resolve_complaint) ~ NA_real_,
                    TRUE ~ 0)
# % of HHs reporting satisfaction with resolution of complaints
r$aap3_satisfied <- case_when(r$satisfaction_complaint %in% c("satisfied","very_satisfied") ~ 1,
                    is.na(r$satisfaction_complaint) ~ NA_real_,
                    TRUE ~ 0)
r$aap3_constant <- case_when(r$satisfaction_complaint == "neither_satisfied" ~ 1,
                            is.na(r$satisfaction_complaint) ~ NA_real_,
                            TRUE ~ 0)
r$aap3_dissatisfied <- case_when(r$satisfaction_complaint %in% c("dissatisfied","very_dissatisfied") ~ 1,
                            is.na(r$satisfaction_complaint) ~ NA_real_,
                            TRUE ~ 0)

# % of HHs reporting their top three priorities following the May Escalation


r$shock1 <- case_when( r$escalation_priorities.food == 1 |
                       r$escalation_priorities.shelter == 1 |
                       r$escalation_priorities.water == 1 |
                       r$escalation_priorities.hygiene == 1 |
                       r$escalation_priorities.health == 1 |
                       r$escalation_priorities.energy == 1 |
                       r$escalation_priorities.transportation == 1 |
                       r$escalation_priorities.hh_items == 1 |
                       r$escalation_priorities.communication == 1 |
                       r$escalation_priorities.legal_services == 1 ~ 1,
                       TRUE ~ 0)



return(r)
}

