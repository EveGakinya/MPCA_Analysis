#SETUP
rm(list=ls(all=T))
R.version
library(rlang)
library(xlsx)
library(plyr) # rbind.fill
library(dplyr)
library(expss)
library(reshape)
library(data.table)
library(miceadds)
library(questionr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(surveyweights) # calculate weights from samplingframes
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations


source("R/functions/postprocessing_functions.R")
source("R/functions/to_alphanumeric_lowercase.R")
source("R/functions/analysisplan_factory.R")
source("R/functions/Binary_Recoding.R")


#LOAD INPUT FILES 
source("R/1_load_inputs.R",local = T)


#RECODING OF INDICATORS


response_with_composites <- recoding_preliminary(response)




#LOAD ANALYSISPLAN
dap_name <- "mpca_preliminary"

analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F)


analysisplan$independent.variable <-  "refugee_status"

analysisplan$independent.variable.type <- "categorical"


#AGGREGATE ACROSS DISTRICTS OR/AND POPULATION GROUPS
analysisplan <- analysisplan_nationwide(analysisplan)


result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          questionnaire = questionnaire, confidence_level = 0.95)


name <- "oPt_mpca_refugee_disagg"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))


summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
#summary$max <- ifelse(summary$max > 1, 1, summary$max)
#summary$min <- ifelse(summary$min < 0, 0, summary$min)

write.csv(summary, sprintf("output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan)
  write.csv(df, sprintf("output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}

