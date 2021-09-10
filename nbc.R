#
# Install required packages
#
require("tidyverse")

working_dir = "path/to/working/dir"
training_path = paste(working_dir, 'training',sep='/'); if(!file.exists(training_path)) dir.create(training_path)
testing_path = paste(working_dir, 'testing',sep='/'); if(!file.exists(testing_path)) dir.create(testing_path)
results_dir = paste(working_dir, 'results',sep='/'); if(!file.exists(results_dir)) dir.create(results_dir)

# #################################################################
#
#               PART 1: DESCRIPTIVE STATISTICS
#
# #################################################################

#
# Modifications for Partners' data
#

# renaming data files
file_names = list()
file_names['demographics'] <- 'path/to/demographics.txt'
file_names['dx'] <- 'path/to/dx.txt'
file_names['proc'] <- 'path/to/proc.txt'
file_names['meds'] <- 'path/to/meds.txt'
file_names['labs'] <- 'path/to/labs.txt'
file_names['nlp'] <- 'path/to/nlp.txt'


# ##################################################################
#                         TABLE 1
# ##################################################################

# This section does not work right now

setwd(working_dir)
addCountAndPercentageForEachValue <- function(var, total){
  res = data.frame()
  for(l in levels(var)){
    count = sum(!is.na(var) & var == l); prcnt = round(100 * count / total, digits = 1)
    res = rbind(res, data.frame(VAR=l, VAL=paste(count,' (',prcnt,'%)', sep = '')))
  }
  return(res)
}

table1 = data.frame()
table1 = rbind(table1, addCountAndPercentageForEachValue(demographics$gender, nrow(demographics)))
table1 = rbind(table1, data.frame(VAR='age', VAL=paste(mean(demographics$age, na.rm=T), ' SD = ', sd(demographics$age, na.rm=T)) ))
table1 = rbind(table1, addCountAndPercentageForEachValue(demographics$race, nrow(demographics)))
table1 = rbind(table1, addCountAndPercentageForEachValue(demographics$case_any, nrow(demographics)))

saveRDS(table1,  paste(results_dir, 'Table1.RDs',sep='/'))

# #################################################################
#
#     PART 2: MAKING SURE WE HAVE ALL THE REQUIRED DATASETS
#
# #################################################################

setwd(working_dir)
demographics = read_tsv(file_names$demographics)
dx = read_tsv(file_names$dx)
proc = read_tsv(file_names$proc)
meds = read_tsv(file_names$meds)
labs = read_tsv(file_names$labs)
nlp = read_tsv(file_names$nlp) %>% rename(concept_code = concept_id) # NLP concept_id -> concept_code

# Add Case definitions

case_def <- read_csv(paste(working_dir, "suicide_ICDdefinition_20190412.csv", sep='/'))
case_def$ICD_code = gsub("\\.","", case_def$`ICD_code\tconcept_code\tconcept_name\tcategory`)
case_def$ICD_code = gsub("\\\t.*","", case_def$ICD_code) 

dx$concept_code = gsub("\\.","", dx$concept_code)
dx$case_code = (dx$concept_code %in% case_def$ICD_code)

cases = unique(dx$subject_num[dx$case_code == TRUE])
saveRDS(cases, 'cases.RDs')

# Filter subjects by age
addAge <- function(demographics){
  demographics$sbirth_date <- as.Date(demographics$sbirth_date, format = "%m/%d/%Y")
  demographics$sdeath_date <- as.Date(demographics$sdeath_date, format = "%m/%d/%Y")
  
  demographics$age = 0
  death_missing = is.na(demographics$sdeath_date)
  # deceased + valid death date
  demographics[!death_missing, 'age'] = as.numeric(demographics$sdeath_date[!death_missing] - demographics$sbirth_date[!death_missing],
                                                   unit='days') / 366
  # not deceased / missing death date
  last_date = as.Date('12/31/2019', format = "%m/%d/%Y")
  demographics[death_missing, 'age'] = as.numeric(last_date - demographics$sbirth_date[death_missing], 
                                                   unit='days') / 366
  return(demographics)
}

filterByAge <- function(df, filteredSubjects){
  df %>%
    filter(subject_num %in% filteredSubjects)
}

demographics$case_any = (demographics$subject_num %in% cases)
demographics <- addAge(demographics)
filteredSubjects <- demographics$subject_num[demographics$age > 10 & demographics$age < 90]

dx <- filterByAge(dx, filteredSubjects)
proc <- filterByAge(proc, filteredSubjects)
meds <- filterByAge(meds, filteredSubjects)
labs <- filterByAge(labs, filteredSubjects)
nlp <- filterByAge(nlp, filteredSubjects)

# #################################################################
#
#       PART 3: DIVIDE DATA TO TRAINING AND TESTING SETS
#
# #################################################################

# --------------------------------------------------
# PART 3A: Divide subjects into 10 different cohorts
# --------------------------------------------------
num_of_cohorts = 10
demographics$group_num = sample(1:num_of_cohorts, nrow(demographics), replace = T)
  
# --------------------------------------------------
# PART 3B: Split data to training & testing
# --------------------------------------------------
saveTrainingAndTestingSets <- function(df, training_ids, testing_ids, cases, file_name)
{
  df$case_any = df$subject_num %in% cases;
  df$sstart_date = strptime(df$sstart_date, format = "%m/%d/%Y")
  saveRDS(df[df$subject_num %in% training_ids, ], paste(training_path, file_name,sep='/'))
  saveRDS(df[df$subject_num %in% testing_ids, ], paste(testing_path, file_name,sep='/'))
  print(paste(file_name,': cases=',sum(df$case_any), ', controls=',sum(!df$case_any),', training_size=',sum(df$subject_num %in% training_ids), 'testing_size=',sum(df$subject_num %in% testing_ids)))
}

training_ids = demographics$subject_num[demographics$group_num %in% c(1:7)]
testing_ids = demographics$subject_num[demographics$group_num %in% c(8:10)]
print(paste('training=',length(training_ids), ', testing=',length(testing_ids),', cases=',length(cases)))
print(paste('training cases=',sum(training_ids %in% cases), ', testing cases=',sum(testing_ids %in% cases)))
saveRDS(training_ids, 'training_ids.RDs')
saveRDS(testing_ids, 'testing_ids.RDs')


saveTrainingAndTestingSets(dx, training_ids, testing_ids, cases, 'dx.RDs')
rm(dx); gc()

saveTrainingAndTestingSets(proc, training_ids, testing_ids, cases, 'proc.RDs')
rm(proc);gc();

saveTrainingAndTestingSets(meds, training_ids, testing_ids, cases, 'meds.RDs')
rm(meds);gc();

saveTrainingAndTestingSets(nlp, training_ids, testing_ids, cases, 'nlp.RDs')
rm(nlp);gc();

saveTrainingAndTestingSets(labs, training_ids, testing_ids, cases, 'labs.RDs')
rm(labs);gc();

saveTrainingAndTestingSets(demographics, training_ids, testing_ids, cases, 'demographics.RDs')
rm(demographics);gc()

#
# PART 3C: Truncate data from index event onwards
#
# --------------------------------------------------

# Get the date of the first suicidal event for each of the case-subjects
# input: dx, proc, meds datasets
# output: a data.frame with the index_date for each subject (based on case definition for meds & dxproc files)
#

getIndexEvent <- function(dx)
{
  suicidal_events = dx[dx$case_code, c('subject_num', 'sstart_date')]
  suicidal_events = suicidal_events[order(suicidal_events$subject_num, suicidal_events$sstart_date), ]
  suicidal_events = suicidal_events[!duplicated(suicidal_events$subject_num), ]
  colnames(suicidal_events) <- c('subject_num', 'index_date')
  
  return(suicidal_events)
}


suicidal_events <- getIndexEvent(dx)
saveRDS(suicidal_events, 'suicidal_events.RDs')

#
# Remove all data occuring after the index event
# input: df: a data.frame to filter (with a column named 'concept_date'), ref_dates: the dates of the index event
# output: the input data.frame (df) without data occuring after index event
#

filterDataOutsideDates <- function(df, ref_dates)
{
  df$row_num = 1:nrow(df)
  cases = which(df$case_any); controls = which(!df$case_any);
  df_cases = merge(df[cases,], ref_dates, by='subject_num', all.x=T, all.y=F)
  
  excluded_cells = df_cases$row_num[which(is.na(df_cases$index_date) | (df_cases$sstart_date >= df_cases$index_date))]
  print(paste('number of case-subjects excluded: ', sum(!duplicated(df$subject_num)) - sum(!duplicated(df$subject_num[-excluded_cells]))))
  
  return(excluded_cells)
}

#
# Go over training & testing sets and truncate dx, proc, meds, and demographics data after index event.
#
truncateAfterIndexEvent <- function(file){
  for(path_name in c(training_path, testing_path)){
    print(paste(file, path_name, ':'))
    setwd(path_name)
    data = read_rds(paste(file, 'RDs', sep='.'))
    
    data$sstart_date = as.Date(data$sstart_date, format = "%m/%d/%Y")
    suicidal_events = readRDS('suicidal_events.RDs') 
    
    suicidal_events$index_date = as.Date(suicidal_events$index_date, format = "%m/%d/%Y")
    data_idxs = filterDataOutsideDates(data[,c('subject_num','sstart_date','case_any')], suicidal_events)
    data = data[-data_idxs, ]
    saveRDS(data, paste(paste(file, 'trunc', sep='_'), 'RDs', sep='.')); 
    rm(data); gc()
  }
}

setwd(working_dir)
for (file in c('meds', 'dx', 'proc', 'labs', 'nlp')){
  truncateAfterIndexEvent(file)
}

# #################################################################
#                                                                 
#       PART 4: BUILDING THE PREDICTIVE MODEL
#
# #################################################################
getNaiveBayesTable <- function(df)
{
  # code-based scoring
  nbc_code = as.data.frame(t(as.data.frame.matrix(table(df$case_any,df$concept_code))))
  colnames(nbc_code) = c('CONTROLS_CODE', 'CASES_CODE')
  nbc_code[is.na(nbc_code) | nbc_code==0] = 0.001
  total_cases_codes = sum(df$case_any); total_control_codes = sum(!df$case_any);
  nbc_code$OR_CODE = (nbc_code$CASES_CODE / (total_cases_codes - nbc_code$CASES_CODE)) / (nbc_code$CONTROLS_CODE / (total_control_codes - nbc_code$CONTROLS_CODE))
  nbc_code$NBC_CODE = log(nbc_code$OR_CODE)
  
  # subject-based scoring
  df = df[!duplicated(paste(df$subject_num,df$concept_code)), ]
  nbc_subj = as.data.frame(t(as.data.frame.matrix(table(df$case_any,df$concept_code))))
  colnames(nbc_subj) = c('CONTROLS_SUBJ', 'CASES_SUBJ')
  nbc_subj[is.na(nbc_subj) | nbc_subj==0] = 0.001
  cases_ids = unique(df$subject_num[df$case_any]); controls_ids = unique(df$subject_num[!df$case_any])
  total_cases = length(cases_ids); total_controls = length(controls_ids);
  nbc_subj$OR_SUBJ = (nbc_subj$CASES_SUBJ / (total_cases - nbc_subj$CASES_SUBJ)) / (nbc_subj$CONTROLS_SUBJ / (total_controls - nbc_subj$CONTROLS_SUBJ))
  nbc_subj$NBC_SUBJ = log(nbc_subj$OR_SUBJ)
  
  nbc = merge(nbc_code, nbc_subj,by=0,all=T)
  rownames(nbc) = nbc$Row.names
  
  return(nbc)
}


# ----------------------------------------------------------------
# PART 4.1: CALCULATING INDIVIDUAL NBC SCORES FOR EACH CONCEPT
# ----------------------------------------------------------------
train_trunc_path = paste(training_path, 'age_filtered', sep='/')
setwd(train_trunc_path)

dx = readRDS('dx_trunc.RDs')
dx_nbc = getNaiveBayesTable(dx); 
saveRDS(dx_nbc, 'dx_nbc.RDs')
rm(dx); gc()

proc = readRDS('proc_trunc.RDs')
proc_nbc = getNaiveBayesTable(proc); 
saveRDS(proc_nbc, 'proc_nbc.RDs')
rm(proc); gc()

meds = readRDS('meds_trunc.RDs')
meds_nbc = getNaiveBayesTable(meds); 
saveRDS(meds_nbc, 'meds_nbc.RDs')
rm(meds); gc()

labs = readRDS('labs_trunc.RDs')
labs_nbc = getNaiveBayesTable(labs); 
saveRDS(labs_nbc, 'labs_nbc.RDs')
rm(labs); gc()

nlp = readRDS('nlp_trunc.RDs')
nlp_nbc = getNaiveBayesTable(nlp);
saveRDS(nlp_nbc, 'nlp_nbc.RDs')
rm(nlp); gc()

demographics = readRDS('demographics.RDs')
total_cases = sum(demographics$case_any == TRUE); total_controls = sum(demographics$case_any == FALSE)

# ##################################################################
#                         TABLE 2
# ##################################################################
getTopScores <- function(nbc.scores, n=20, min_count=50)
{
  nbc.scores$variable = sapply(rownames(nbc.scores), function(x) substr(x,1,15));
  nbc.scores = nbc.scores[order(nbc.scores$OR_SUBJ, decreasing=T),]
  nbc.scores$total_count = nbc.scores$CASES_SUBJ + nbc.scores$CONTROLS_SUBJ;
  nbc.scores = nbc.scores[nbc.scores$total_count > min_count & nbc.scores$variable != 'isCase', ]
  return(nbc.scores[1:n,c('variable','CASES_SUBJ','CONTROLS_SUBJ','OR_SUBJ')])
}

nbc.res = rbind(getTopScores(labs_nbc), 
                getTopScores(dx_nbc), 
                getTopScores(meds_nbc), 
                getTopScores(proc_nbc),
                getTopScores(nlp_nbc))
saveRDS(nbc.res,  paste(results_dir, 'Table2.RDs',sep='/'))

# ----------------------------------------------------------------
# PART 4.2 CALCULATE CUMULATE RISK SCORE FOR EACH SUBJECT
# ----------------------------------------------------------------
train_trunc_path = paste(training_path, 'age_filtered', sep='/')
nbc_weight_path = paste(training_path, 'nbc', sep='/')
setwd(nbc_weight_path)

dx_nbc = readRDS('dx_nbc.RDs')
proc_nbc = readRDS('proc_nbc.RDs')
meds_nbc = readRDS('meds_nbc.RDs')
labs_nbc = readRDS('labs_nbc.RDs')
nlp_nbc = readRDS('nlp_nbc.RDs')
all_nbc = rbind(dx_nbc, proc_nbc, meds_nbc, 
                labs_nbc, nlp_nbc)

dx_nbc$NBC_SUBJ[dx_nbc$CASES_SUBJ < 10 | dx_nbc$CONTROLS_SUBJ < 10] = 0
proc_nbc$NBC_SUBJ[proc_nbc$CASES_SUBJ < 10 | proc_nbc$CONTROLS_SUBJ < 10] = 0
meds_nbc$NBC_SUBJ[meds_nbc$CASES_SUBJ < 10 | meds_nbc$CONTROLS_SUBJ < 10] = 0
labs_nbc$NBC_SUBJ[labs_nbc$CASES_SUBJ < 10 | labs_nbc$CONTROLS_SUBJ < 10] = 0
nlp_nbc$NBC_SUBJ[nlp_nbc$CASES_SUBJ < 10 | nlp_nbc$CONTROLS_SUBJ < 10] = 0

# 4.2.1) read validation datasets
test_trunc_path = paste(testing_path, 'age_filtered', sep='/')
setwd(test_trunc_path)
dx = readRDS('dx_trunc.RDs')
proc = readRDS('proc_trunc.RDs')
meds = readRDS('meds_trunc.RDs')
labs = readRDS('labs_trunc.RDs')
nlp = readRDS('nlp_trunc.RDs')

demographics = readRDS('demo_trunc.RDs')

# 4.2.2) add NBC scores
dx$NBC_SUBJ = dx_nbc[as.character(dx$concept_code),'NBC_SUBJ'];
proc$NBC_SUBJ = proc_nbc[as.character(proc$concept_code),'NBC_SUBJ'];
meds$NBC_SUBJ = meds_nbc[as.character(meds$concept_code),'NBC_SUBJ'];
labs$NBC_SUBJ = labs_nbc[as.character(labs$concept_code),'NBC_SUBJ'];
nlp$NBC_SUBJ = nlp_nbc[as.character(nlp$concept_code),'NBC_SUBJ'];

# 4.2.3) merge meds with dxproc
cols = c('subject_num', 'sstart_date', 'concept_code', 'NBC_SUBJ')
all_codes = bind_rows(dx[, cols], proc[, cols], meds[, cols], labs[, cols], nlp[, cols])

# 4.2.4) sort by subject and by date and remove duplicate records of the same code - count only once
all_codes = all_codes[order(all_codes$subject_num, all_codes$sstart_date), ]
all_codes = all_codes[!duplicated(paste(all_codes$subject_num, all_codes$concept_code)), ]

# 4.2.5) calculate the score over time for each subject and the maximal value achieved
 
cum_sum = tapply(all_codes$NBC_SUBJ, all_codes$subject_num, cumsum)
max_score_per_subject = sapply(cum_sum, max)
# ----------------------------------------------------------------
# PART 4.3 - EVALUATE MODEL'S OVERALL PERFORMANCE
# ----------------------------------------------------------------
require(pROC)
res = data.frame(subject_num=names(max_score_per_subject), max_score=max_score_per_subject)
res = res %>% mutate(case_any = subject_num %in% cases)

roc.res = roc(res$case_any, res$max_score)
print(paste('AUC=',roc.res$auc))


# ##################################################################
#                         TABLE 3
# ##################################################################
getSummaryStatsTbl <- function(roc.res)
{
  stats_cols = c("spec", "threshold", "accuracy", "sen", "ppv")
  
  summary.stats = data.frame(rbind(
    coords(roc.res, ret=stats_cols),
    coords(roc.res, ret=stats_cols),
    coords(roc.res, ret=stats_cols),
    coords(roc.res, ret=stats_cols),
    coords(roc.res, ret=stats_cols),
    coords(roc.res,  ret=stats_cols)))
  summary.stats$AUC = roc.res$auc
  return(round(summary.stats, digits = 3))
}

stats = getSummaryStatsTbl(roc.res) %>% 
  tibble() %>%
  filter(
    abs(specificity - 0.99) < 0.0001 |
    abs(specificity - 0.95) < 0.0001 |
    abs(specificity - 0.90) < 0.0001 |
    abs(specificity - 0.85) < 0.0001 |
    abs(specificity - 0.80) < 0.0001
) %>% 
  distinct(specificity, .keep_all = T)

saveRDS(getSummaryStatsTbl(roc.res),  paste(results_dir, 'Table3_ehr_nlp.RDs',sep='/'))
# plotROC(roc.res, "ROC")

# ----------------------------------------------------------------
# PART 4.4: CALCULATE MODEL SCORE BY MONTH
# ----------------------------------------------------------------
setwd(working_dir)
require(data.table)
df = all_codes[,c('subject_num','sstart_date')]
df = merge(df, df[!duplicated(df$subject_num),c('subject_num','sstart_date')], by='subject_num',all=T)
df$rel_month = round(as.numeric(difftime(df$sstart_date.x, df$sstart_date.y, units = 'days'))/30.41667, digits = 0)
df$score = round(all_codes$NBC_SUBJ, digits = 1)
df$cum_sum = unlist(cum_sum)
df$case_any = (df$subject_num %in% cases)
df = df[rev(!duplicated(rev(paste(df$subject_num, df$rel_month)))), ]
saveRDS(df[,c('subject_num', 'rel_month', 'cum_sum', 'case_any')],paste(results_dir, 'Score by Month ehr nlp.RDs',sep='/'))

# ##################################################################
#                         TABLE 4
# cumulative risk scores over time for cases vs. controls (automatically
# generated while building the model).
# ##################################################################
max_month = 15*12 # 15 years
cases_scores = df[df$case_any & df$rel_month < max_month, ]
controls_scores = df[!df$case_any & df$rel_month < max_month, ]

median_cases = tapply(cases_scores$cum_sum, cases_scores$rel_month, median, na.rm=T)
sd_cases = tapply(cases_scores$cum_sum, cases_scores$rel_month, sd, na.rm=T)
median_controls = tapply(controls_scores$cum_sum, controls_scores$rel_month, median, na.rm=T)
sd_controls = tapply(controls_scores$cum_sum, controls_scores$rel_month, sd, na.rm=T)
table4 = data.frame(merge(cbind(median_cases, sd_cases), cbind(median_controls, sd_controls), by=0, all=T))
colnames(table4)[1] = 'month'
saveRDS(table4[order(as.numeric(table4$month)), ],paste(results_dir, 'Table4_ehr_nlp.RDs',sep='/'))
