library(tidyr)
library(dplyr)
library(janitor)
library(gt)
library(stargazer)

# Set up working director
wd<-dirname(rstudioapi::getSourceEditorContext()$path)
# Load data
load(paste(wd, "REStatSubmission_FRB.RData", sep = "/"))
#load("~/UMD/ECON 687/Project/REStatSubmission_FRB.RData")

################################################################################
# Table 1 - Summary Statistics
################################################################################

# Named list of labels and variable names
labels <- c("2008 Delinquency Rate", "Center City", "Median House Value", "Median Age",
  "% 65 or Older", "Unemployment Rate", "% Minority", "Owner Occupancy Rate",
  "Vacancy Rate", "2000 Relative Income", "1990 Relative Income", "2001 Credit Score",
  "2004 Credit Score", "% High PTI", "% High Rate", "% Piggyback", "% No Income",
  "% LMI", "% Below Median Income", "2001 Dep Out Share", "2001 Dep In Share",
  "2001 Aff Out Share", "2001 Aff In Share", "2001 Credit Union Share",
  "2001 Independent Share", "2004-6 Dep Out Share", "2004-6 Dep In Share",
  "2004-6 Aff Out Share", "2004-6 Aff In Share", "2004-6 Credit Union Share",
  "2004-6 Independent Share", "2001 Dep Out Purchases", "2001 Dep In Purchases",
  "2001 Aff Out Purchases", "2001 Aff In Purchases", "2001 Credit Union Purchases",
  "2001 Independent Purchases", "2001 GSE Sales", "2004-6 Dep Out Purchases",
  "2004-6 Dep In Purchases", "2004-6 Aff Out Purchases", "2004-6 Aff In Purchases",
  "2004-6 Credit Union Purchases", "2004-6 Independent Purchases", "2004-6 GSE Sales")

varss <- c("delrate", "CENTER", "CUML", "MEDAGE", "OVER65", "UNEMRT", "minpct",
  "ownpct", "vacpct", "relinc", "relinc90", "SCORE2000", "SCORE2004", "hidti_20046",
  "hirate_20046", "pig_20046", "zerinc_20046", "lowinc_20046", "midinc_20046",
  "shr.dep.out_2001", "shr.dep.in_2001", "shr.aff.out_2001", "shr.aff.in_2001",
  "shr.cu_2001", "shr.ind_2001", "shr.dep.out_20046", "shr.dep.in_20046",
  "shr.aff.out_20046", "shr.aff.in_20046", "shr.cu_20046", "shr.ind_20046",
  "pur.dep.out_2001", "pur.dep.in_2001", "pur.aff.out_2001", "pur.aff.in_2001",
  "pur.cu_2001", "pur.ind_2001", "gse_2001", "pur.dep.out_20046", "pur.dep.in_20046",
  "pur.aff.out_20046", "pur.aff.in_20046", "pur.cu_20046", "pur.ind_20046", "gse_20046")

# Creates an empty data fram with two columns called "Variable" and "Mean"
table.1 <- data.frame(Variable = character(), Mean = character())
# Loop that goes through the vector varss and calculates mean for each variable and add to table.1
myData <- myData %>% filter(MINOF3==1 & L11==1)
for(i in 1:length(varss)){
  mean <- weighted.mean(myData[[varss[i]]], w = myData$COUNT) %>% round(2)
  table.1[i,] <- c(labels[i], mean)
}

gt(table.1)
gtsave(gt(table.1), "Table1.docx",wd)


################################################################################
# Table 2 - OLS Regression
################################################################################

# Model in column (1)
table.2.1 <- lm(delrate ~ shr.dep.out_2001 + shr.dep.in_2001 + shr.aff.out_2001 + shr.aff.in_2001 + shr.cu_2001 + # Share of lending 
                pur.dep.out_2001 + pur.dep.in_2001 + pur.aff.out_2001 + pur.aff.in_2001 + pur.cu_2001 + pur.ind_2001 + gse_2001 + # Purchases as share of lending 
                SCORE2000 + relinc + relinc90 + UNEMRT + MEDAGE + OVER65 + CENTER + minpct + CUML + ownpct + vacpct + as.factor(MSA), # Baseline controls 
                myData,
                weights=COUNT)
summary(table.2.1)

# Model in column (2)
table.2.2 <- lm(delrate ~ shr.dep.out_20046 + shr.dep.in_20046 + shr.aff.out_20046 + shr.aff.in_20046 + shr.cu_20046 + # Share of lending
                pur.dep.out_20046 + pur.dep.in_20046 + pur.aff.out_20046 + pur.aff.in_20046 + pur.cu_20046 + pur.ind_20046 + gse_20046 + # Purchases as share of lending
                SCORE2004 + relinc + relinc90 + UNEMRT + MEDAGE + OVER65 + CENTER + minpct + CUML + ownpct + vacpct + as.factor(MSA), # Baseline controls
                myData,
                weights=COUNT)
summary(table.2.2)

# Model in column (3)
table.2.3 <- lm(delrate ~ shr.dep.out_20046 + shr.dep.in_20046 + shr.aff.out_20046 + shr.aff.in_20046 + shr.cu_20046 + # Share of lending
                pur.dep.out_20046 + pur.dep.in_20046 + pur.aff.out_20046 + pur.aff.in_20046 + pur.cu_20046 + pur.ind_20046 + gse_20046 + # Purchases as share of lending
                SCORE2004 + relinc + relinc90 + UNEMRT + MEDAGE + OVER65 + CENTER + minpct + CUML + ownpct + vacpct + as.factor(MSA) + # Baseline controls
                hidti_20046 + hirate_20046 + pig_20046 + lowinc_20046 + midinc_20046 + zerinc_20046, # Additional Controls
                myData,
                weights=COUNT)
summary(table.2.3)

stargazer(table.2.1, table.2.2, table.2.3, 
          type="text", 
          title="Table 1. -- Delinquency Rate Estimations",
          omit = c("SCORE2000", "SCORE2004", "SCORE2004", "relinc", "relinc90", "UNEMRT", "MEDAGE", "OVER65", "CENTER", "minpct", "CUML", "ownpct", "vacpct", "MSA", # Baseline controls
          "hidti_20046", "hirate_20046", "pig_20046", "lowinc_20046", "midinc_20046", "zerinc_20046"))
