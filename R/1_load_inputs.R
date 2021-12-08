# LOAD QUESTIONNAIRE
questions <- read.csv("input/questionnaire/kobo_questions.csv", 
                      stringsAsFactors=T, check.names=T)
colnames(questions)[1] <- "type"


choices <- read.csv("input/questionnaire/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=T)
colnames(choices)[1] <- "list_name"

# LOAD DATA AND MERGE REPRESENTATIVE AND INDICATIVE DATA
#data <- xlsform_fill(questions, choices, 500)
response <- read.csv("Input/datasets/cleaned/mpca_data_clean.csv",
                              stringsAsFactors=F, check.names=T,
                              na.strings = c("", " ", "NA", "#N/A", "N/A"))

names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"

# MERGE QUESTIONNAIRES
questionnaire <- load_questionnaire(response,questions,choices, choices.label.column.to.use = "name")

