library(readr)
library(tidyverse)
r <-read_csv("Input/datasets/cleaned/mpca_data_clean.csv")
recoding_preliminary <- function(r) {
  
  r=response


rename_variables <- function(df){
  names(df) <- c(gsub("/",".",names(df)))
  return(df)
}
 r <-rename_variables(r)


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
# % of households reporting fear of eviction
r$hh6 <-case_when(r$fear_eviction == "yes" ~ 1,
                  r$fear_eviction == "no" ~ 0,
                  TRUE ~ NA_real_)

# % of HHs reporting that transfer amount has met their basic needs
# % of HHs reporting that transfer amount has met their cash needs
r$amm1_i <- case_when(r$cash %in% c("mostly_able","fully_able")~1,
                    r$cash %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their food needs
r$amm1_ii <- case_when(r$food %in% c("mostly_able","fully_able")~1,
                    r$food %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their shelter needs
r$amm1_iii <- case_when(r$shelter %in% c("mostly_able","fully_able")~1,
                    r$shelter %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their water needs
r$amm1_iv <- case_when(r$water %in% c("mostly_able","fully_able")~1,
                    r$water %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)

# % of HHs reporting that transfer amount has met their hygiene needs
r$amm1_v <- case_when(r$hygiene %in% c("mostly_able","fully_able")~1,
                    r$hygiene %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)

# % of HHs reporting that transfer amount has met their health needs
r$amm1_vi <- case_when(r$health %in% c("mostly_able","fully_able")~1,
                    r$health %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their education needs
r$amm1_vii <- case_when(r$education %in% c("mostly_able","fully_able")~1,
                    r$education %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their energy needs
r$amm1_viii <- case_when(r$energy %in% c("mostly_able","fully_able")~1,
                    r$energy %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their transportation needs
r$amm1_ix <- case_when(r$transportation %in% c("mostly_able","fully_able")~1,
                    r$transportation %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their hh_items needs
r$amm1_x <- case_when(r$hh_items %in% c("mostly_able","fully_able")~1,
                    r$hh_items %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their communication needs
r$amm1_xi <- case_when(r$communication %in% c("mostly_able","fully_able")~1,
                    r$communication %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)
# % of HHs reporting that transfer amount has met their debt needs
r$amm1_xii <- case_when(r$debt %in% c("mostly_able","fully_able")~1,
                    r$debt %in% c("completely_unable","partially_able")~0,
                    TRUE~ NA_real_)


# % of HHs reporting primary barriers to their cash needs
r$amm2_i <- case_when(r$cash_barriers.insufficient_money == 1 | r$cash_barriers.distance == 1 | r$cash_barriers.security_situation == 1 |
                      r$cash_barriers.social_discrimination == 1 | r$cash_barriers.insufficient_goods == 1 | r$cash_barriers.poor_quality == 1 ~ 1,
                      TRUE ~ 0)

# % of HHs reporting primary barriers to their food needs
r$amm2_ii <- case_when(r$food_barriers.insufficient_money == 1 | r$food_barriers.distance == 1 | r$food_barriers.security_situation == 1 |
                      r$food_barriers.social_discrimination == 1 | r$food_barriers.insufficient_goods == 1 | r$food_barriers.poor_quality == 1 ~ 1,
                      TRUE ~ 0)

# % of HHs reporting primary barriers to their shelter needs
r$amm2_iii <- case_when(r$shelter_barriers.insufficient_money == 1 | r$shelter_barriers.distance == 1 | r$shelter_barriers.security_situation == 1 |
                       r$shelter_barriers.social_discrimination == 1 | r$shelter_barriers.insufficient_goods == 1 | r$shelter_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)

# % of HHs reporting primary barriers to their water needs
r$amm2_iv <- case_when(r$water_barriers.insufficient_money == 1 | r$water_barriers.distance == 1 | r$water_barriers.security_situation == 1 |
                       r$water_barriers.social_discrimination == 1 | r$water_barriers.insufficient_goods == 1 | r$water_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)

# % of HHs reporting primary barriers to their hygiene needs
r$amm2_v <- case_when(r$hygiene_barriers.insufficient_money == 1 | r$hygiene_barriers.distance == 1 | r$hygiene_barriers.security_situation == 1 |
                      r$hygiene_barriers.social_discrimination == 1 | r$hygiene_barriers.insufficient_goods == 1 | r$hygiene_barriers.poor_quality == 1 ~ 1,
                      TRUE ~ 0)
# % of HHs reporting primary barriers to their health needs
r$amm2_vi <- case_when(r$health_barriers.insufficient_money == 1 | r$health_barriers.distance == 1 | r$health_barriers.security_situation == 1 |
                       r$health_barriers.social_discrimination == 1 | r$health_barriers.insufficient_goods == 1 | r$health_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)
# % of HHs reporting primary barriers to their education needs
r$amm2_vii <- case_when(r$edu_barriers.insufficient_money == 1 | r$edu_barriers.distance == 1 | r$edu_barriers.security_situation == 1 |
                        r$edu_barriers.social_discrimination == 1 | r$edu_barriers.insufficient_goods == 1 | r$edu_barriers.poor_quality == 1 ~ 1,
                        TRUE ~ 0)
# % of HHs reporting primary barriers to their energy needs
r$amm2_Viii <- case_when(r$energy_barriers.insufficient_money == 1 | r$energy_barriers.distance == 1 | r$energy_barriers.security_situation == 1 |
                       r$energy_barriers.social_discrimination == 1 | r$energy_barriers.insufficient_goods == 1 | r$energy_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)
# % of HHs reporting primary barriers to their transportation needs
r$amm2_ix <- case_when(r$transportation_barriers.insufficient_money == 1 | r$transportation_barriers.distance == 1 | r$transportation_barriers.security_situation == 1 |
                       r$transportation_barriers.social_discrimination == 1 | r$transportation_barriers.insufficient_goods == 1 | r$transportation_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)
# % of HHs reporting primary barriers to their hh_items needs
r$amm2_x <- case_when(r$hh_items_barriers.insufficient_money == 1 | r$hh_items_barriers.distance == 1 | r$hh_items_barriers.security_situation == 1 |
                       r$hh_items_barriers.social_discrimination == 1 | r$hh_items_barriers.insufficient_goods == 1 | r$hh_items_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)
# % of HHs reporting primary barriers to their communication needs
r$amm2_xi <- case_when(r$comms_barriers.insufficient_money == 1 | r$comms_barriers.distance == 1 | r$comms_barriers.security_situation == 1 |
                       r$comms_barriers.social_discrimination == 1 | r$comms_barriers.insufficient_goods == 1 | r$comms_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)

# % of HHs reporting primary barriers to their debt needs
r$amm2_Xii <- case_when(r$debt_barriers.insufficient_money == 1 | r$debt_barriers.distance == 1 | r$debt_barriers.security_situation == 1 |
                       r$debt_barriers.social_discrimination == 1 | r$debt_barriers.insufficient_goods == 1 | r$debt_barriers.poor_quality == 1 ~ 1,
                       TRUE ~ 0)

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
# % of household reporting effect of MPCA on ability to withstand shocks:shock of medical expenses
unique(r$role_mpca_med)

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


# % of households reporting reasons MPCA was ineffective In helping withstand shocks
# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of medical expenses
r$shock7_i <- case_when(r$mpca_shock_ineffective_med.amount_small == 1 |r$mpca_shock_ineffective_med.did_not_receive_cash_intime == 1 |
                        r$mpca_shock_ineffective_med.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_med.no_access_to_atms == 1 |
                        r$mpca_shock_ineffective_med.cash_not_relevant == 1 ~ 1,
                        r$mpca_shock_ineffective_med.amount_small == 0 |r$mpca_shock_ineffective_med.did_not_receive_cash_intime == 0 |
                        r$mpca_shock_ineffective_med.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_med.no_access_to_atms == 0 |
                        r$mpca_shock_ineffective_med.cash_not_relevant == 0 ~ 0,
                        TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of loss of income
r$shock7_ii <- case_when(r$mpca_shock_ineffective_loss_employ.amount_small == 1 |r$mpca_shock_ineffective_loss_employ.did_not_receive_cash_intime == 1 |
                         r$mpca_shock_ineffective_loss_employ.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_loss_employ.no_access_to_atms == 1 |
                         r$mpca_shock_ineffective_loss_employ.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_loss_employ.amount_small == 0 |r$mpca_shock_ineffective_loss_employ.did_not_receive_cash_intime == 0 |
                         r$mpca_shock_ineffective_loss_employ.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_loss_employ.no_access_to_atms == 0 |
                         r$mpca_shock_ineffective_loss_employ.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of loss of inventory or produce
r$shock7_iii <- case_when(r$mpca_shock_ineffective_loss_inventory.amount_small == 1 |r$mpca_shock_ineffective_loss_inventory.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_loss_inventory.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_loss_inventory.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_loss_inventory.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_loss_inventory.amount_small == 0 |r$mpca_shock_ineffective_loss_inventory.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_loss_inventory.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_loss_inventory.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_loss_inventory.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of arrival of new household members
r$shock7_iv <- case_when(r$mpca_shock_ineffective_new_member.amount_small == 1 |r$mpca_shock_ineffective_new_member.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_new_member.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_new_member.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_new_member.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_new_member.amount_small == 0 |r$mpca_shock_ineffective_new_member.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_new_member.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_new_member.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_new_member.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of need to repair or replace a household item
r$shock7_v <- case_when(r$mpca_shock_ineffective_repair_nfi.amount_small == 1 |r$mpca_shock_ineffective_repair_nfi.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_repair_nfi.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_repair_nfi.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_repair_nfi.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_repair_nfi.amount_small == 0 |r$mpca_shock_ineffective_repair_nfi.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_repair_nfi.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_repair_nfi.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_repair_nfi.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of repairing a productive asset
r$shock7_vi <- case_when(r$mpca_shock_ineffective_repair_asset.amount_small == 1 |r$mpca_shock_ineffective_repair_asset.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_repair_asset.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_repair_asset.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_repair_asset.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_repair_asset.amount_small == 0 |r$mpca_shock_ineffective_repair_asset.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_repair_asset.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_repair_asset.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_repair_asset.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of education expenses
r$shock7_vii <- case_when(r$mpca_shock_ineffective_edu.amount_small == 1 |r$mpca_shock_ineffective_edu.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_edu.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_edu.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_edu.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_edu.amount_small == 0 |r$mpca_shock_ineffective_edu.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_edu.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_edu.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_edu.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of shelter expenses
r$shock7_viii <- case_when(r$mpca_shock_ineffective_shelter.amount_small == 1 |r$mpca_shock_ineffective_shelter.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_shelter.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_shelter.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_shelter.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_shelter.amount_small == 0 |r$mpca_shock_ineffective_shelter.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_shelter.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_shelter.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_shelter.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks:shock of debt payment
r$shock7_xi <- case_when(r$mpca_shock_ineffective_debt.amount_small == 1 |r$mpca_shock_ineffective_debt.did_not_receive_cash_intime == 1 |
                           r$mpca_shock_ineffective_debt.did_not_receive_cash_frequently == 1 | r$mpca_shock_ineffective_debt.no_access_to_atms == 1 |
                           r$mpca_shock_ineffective_debt.cash_not_relevant == 1 ~ 1,
                         r$mpca_shock_ineffective_debt.amount_small == 0 |r$mpca_shock_ineffective_debt.did_not_receive_cash_intime == 0 |
                           r$mpca_shock_ineffective_debt.did_not_receive_cash_frequently == 0 | r$mpca_shock_ineffective_debt.no_access_to_atms == 0 |
                           r$mpca_shock_ineffective_debt.cash_not_relevant == 0 ~ 0,
                         TRUE ~ NA_real_)

# % of households reporting reasons MPCA was ineffective In helping withstand shocks

r$shocks7 <- case_when(r$shock7_i == 1 | r$shock7_ii== 1| r$shock7_iv == 1 |r$shock7_v == 1|r$shock7_vi == 1|r$shock7_vii == 1|r$shock7_viii == 1 | r$shock7_xi == 1 ~ 1,
                       r$shock7_i == 0 | r$shock7_ii== 0| r$shock7_iv == 0 |r$shock7_v == 0|r$shock7_vi == 0|r$shock7_vii == 0|r$shock7_viii == 0 | r$shock7_xi == 0 ~ 0,
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

##% HH relying on stress / crisis / emergency strategies to cope with a lack of food or money to buy it
r$stress <-
  ifelse(
    r$selling_hh_properties %in% c("no_already_did", "yes") |
      r$buying_goods_on_credit %in% c("no_already_did", "yes") |
      r$reduced_health_expenses %in% c("no_already_did", "yes"), 
    1,
    0  
  )

r$crisis <-
  ifelse(
    r$selling_productive_assets %in% c("no_already_did", "yes") |
      r$sold_house_land %in% c("no_already_did", "yes") |
      r$withdrew_children_from_school %in% c("no_already_did", "yes"),
    1,
    0
  )

r$emergency <-
  ifelse(
    r$school_drop_out %in% c("no_already_did", "yes") |
      r$engaging_in_risky_behaviour %in% c("no_already_did", "yes") |
      r$hh_migrated %in% c("no_already_did", "yes"),
    1,
    0
  )

# % of households reporting how their financial situation compares to before receiving MPCA
r$impact2 <- case_when(r$change_financial_situation %in% c("improved_significantly","improved_somewhat") ~ "improved",
                     r$change_financial_situation == "same_as_before" ~ "constant",
                     r$change_financial_situation %in% c("worsened_significantly","worsened_somewhat") ~ "worsened",
                     TRUE ~ NA_character_)
# % of households attributing their improved financial situation to My Choice 
r$impact3 <- case_when(r$change_financial_situation_mpca %in% c("great_help","some_help") ~ 1,
                     r$change_financial_situation_mpca %in% c("did_not_help_much","did_not_help_at_all") ~ 0,
                     TRUE ~ NA_real_)
# % of households reporting how the mental state of household members compares to before receiving MPCA
r$impact4 <- case_when(r$change_psych_situation %in% c("improved_significantly","improved_somewhat") ~ "improved",
                     r$change_psych_situation == "same_as_before" ~ "constant",
                     r$change_psych_situation %in% c("worsened_significantly","worsened_somewhat") ~ "worsened",
                     TRUE ~ NA_character_)
# % of households attributing their improved mental state to My Choice
r$impact5 <- case_when(r$change_psych_situation_mpca %in% c("great_help","some_help") ~ 1,
                     r$change_psych_situation_mpca %in% c("did_not_help_much","did_not_help_at_all")~0,
                     TRUE ~ NA_real_)

# % of HHs reporting  different changes in inter-HH behavior as a result of the MPCA by type of activity
# Joint decision-making (e.g. more than one HH member) on household expenses
r$impact6_i <- case_when(r$joint_decision_making %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
                     r$joint_decision_making == "neither_increased_decreased" ~ "constant",
                     r$joint_decision_making %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
                     TRUE ~ NA_character_)

# Joint decision-making on whether to take loans
r$impact6_ii <- case_when(r$joint_loans %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
                       r$joint_loans == "neither_increased_decreased" ~ "constant",
                       r$joint_loans %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
                       TRUE ~ NA_character_)

# Joint decision-making on responsibilities of household members (e.g. work, care of the elderly)
r$impact6_iii <- case_when(r$joint_responsibilities %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
                       r$joint_responsibilities == "neither_increased_decreased" ~ "constant",
                       r$joint_responsibilities %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
                       TRUE ~ NA_character_)
# Disputes within household on how to spend money
r$impact6_iv <- case_when(r$spending_disputes %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
                       r$spending_disputes == "neither_increased_decreased" ~ "constant",
                       r$spending_disputes %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
                       TRUE ~ NA_character_)
# Disputes with creditors
r$impact6_v <- case_when(r$disputes_with_creditors %in% c("increased_greatly","increased_to_some_extent") ~ "increased",
                       r$disputes_with_creditors == "neither_increased_decreased" ~ "constant",
                       r$disputes_with_creditors %in% c("decreased_some_extent","decreased_greatly") ~ "decreased",
                       TRUE ~ NA_character_)


# % of households reporting changes with relatives as a result of receiving MPCA through My Choice

r$sat1 <- case_when(r$change_relations_relatives %in% c("relation_improved_significantly","relation_improved_some_extend") ~ "improved",
                  r$change_relations_relatives == "not_changed" ~ "constant",
                  r$change_relations_relatives %in% c("relation_worsened_some_extent","relation_worsened_significantly") ~ "worsened",
                  TRUE ~ NA_character_)

# % of households reporting changes with other households or community members since receiving MPCA through My Choice
r$sat3 <- case_when(r$changes_relations_community %in% c("relation_improved_significantly","relation_improved_some_extend") ~ "improved",
                    r$changes_relations_community == "not_changed" ~ "constant",
                    r$changes_relations_community %in% c("relation_worsened_some_extent","relation_worsened_significantly") ~ "worsened",
                    TRUE ~ NA_character_)

# % HHs with access/knowledge of complaint mechanisms 
aap <- case_when(r$complaint_mechanisms == "yes" ~ 1,
                 r$complaint_mechanisms == "no" ~ 0,
                 TRUE~ NA_real_)
return(r)
}
