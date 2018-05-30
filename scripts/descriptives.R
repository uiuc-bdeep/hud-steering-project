# -------------------------------------------------------------------------------------------------- #
#   Generate descriptive statistics tables for HUD Discrimination paper                              #
#                                                                                                    # 
#   Author: Amanda Ang, Peter Christensen                                                            #
#   Date created: 1/23/2017                                                                          #
#                                                                                                    # 
#   Edited on 03/02/2018 to create descriptives table with updated dataset and additional variables  #
#   Edited on 03/07/2018 to ensure that descriptives table and balance table show the same variables #
#   Edited on 05/30/2018 to create column for all racial groups, add Assaults and Elem Sch Score     #
# -------------------------------------------------------------------------------------------------- #

# Clear workspace

rm(list = ls())

# Set working directory

setwd("/home/bdeep/share/projects/HUD_Discrimination/")

# Define function for loading packages

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Load packages

packages <- c("caret","dplyr", "stargazer")
lapply(packages, pkgTest)

# Input
# Use tester dataset (N = 2260) for tester characteristics 
# Use homes dataset (N = 7033) for home characteristics

testers.path <- "stores/HDS_processeddata/tester_assignment.csv"
homes.path <- "stores/HDS_processeddata/adsprocessed.rds"

# Load data 

testers <- read.csv(testers.path, header = TRUE)
homes <- readRDS(homes.path)

# Output

out <- "views/tables/"



# Descriptive statistics by race ---------------------------------------------------------------

# Homes dataset

# Subset dataset to desired variables 

homes <- homes[,c("APRACE",
                  "SAPPTAM",
                  "AMOVERS",
                  "DPMTEXP",
                  "HHMTYPE",
                  "AdPrice",
                  "Assault_Ad",
                  "Elementary_School_Score_Ad",
                  "w2012pc_Ad",
                  "b2012pc_Ad",
                  "a2012pc_Ad",
                  "hisp2012pc_Ad",
                  "SFcount_Ad",
                  "RSEI_Ad",
                  "povrate_Ad",
                  "college_Ad",
                  "skill_Ad",
                  "RespiratoryHazardIndex_Ad")]

# Testers Dataset 

# Subset dataset to desired variables

testers <- testers[,c("APRACE",
                      "age",
                      "SEQUENCE",
                      "TSEX.y",
                      "TPEGAI",
                      "THIGHEDU",
                      "TCURTENR",
                      "ARENTNOW",
                      "ACAROWN",
                      "AELNG1",
                      "ALEASETP",
                      "ALGNCUR")]

# Convert factors variables into separate indicators -------------------------------------------

# Tester Characteristics (Not Assigned) --------------------------------------------------------

# Personal Annual Income

testers$TPEGAI <- as.factor(as.character(testers$TPEGAI))

inc.dummies <- as.data.frame(predict(dummyVars(~ TPEGAI, data = testers), newdata = testers))
testers <- cbind(testers, inc.dummies)

testers$TPEGAI <- NULL
testers$`TPEGAI.-1` <- NULL

# Highest Level of Education

testers$THIGHEDU <- as.factor(as.character(testers$THIGHEDU))

edu.dummies <- as.data.frame(predict(dummyVars(~ THIGHEDU, data = testers), newdata = testers))
testers <- cbind(testers, edu.dummies)

testers$THIGHEDU <- NULL
testers$`THIGHEDU.-1` <- NULL

# Sequence (First or Second)

testers$SEQUENCE <- as.factor(as.character(testers$SEQUENCE))

seq.dummies <- as.data.frame(predict(dummyVars(~ SEQUENCE, data = testers), newdata = testers))
testers <- cbind(testers, seq.dummies)

testers$SEQUENCE <- NULL
testers$SEQUENCE.2 <- NULL

# Homeowner: Do you presently rent or own your home?
# 1 = rent
# 2 = own
# 3 = other

testers$TCURTENR <- as.factor(as.character(testers$TCURTENR))

own.dummies <- as.data.frame(predict(dummyVars(~ TCURTENR, data = testers), newdata = testers))
testers <- cbind(testers, own.dummies)

testers$TCURTENR <- NULL
testers$`TCURTENR.-1` <- NULL
testers$TCURTENR.3 <- NULL

# Assigned Characteristics ---------------------------------------------------------------------

# Lease Type
# 1 = Month-to-Month
# 2 = Lease

testers$ALEASETP <- as.factor(as.character(testers$ALEASETP))

lease.dummies <- as.data.frame(predict(dummyVars(~ ALEASETP, data = testers), newdata = testers))
testers <- cbind(testers, lease.dummies)

testers$ALEASETP <- NULL

# Home Characteristics -------------------------------------------------------------------------

# Reason for Moving 

homes$AMOVERS <- as.factor(as.character(homes$AMOVERS))

reason.dummies <- as.data.frame(predict(dummyVars(~ AMOVERS, data = homes), newdata = homes))
homes <- cbind(homes, reason.dummies)

homes$AMOVERS <- NULL
homes$`AMOVERS.-1` <- NULL

# Reason for Downpayment

homes$DPMTEXP <- as.factor(as.character(homes$DPMTEXP))

dpreason.dummies <- as.data.frame(predict(dummyVars(~ DPMTEXP, data = homes), newdata = homes))
homes <- cbind(homes, dpreason.dummies)

homes$DPMTEXP <- NULL

# House Type

homes$HHMTYPE <- as.factor(as.character(homes$HHMTYPE))

htype.dummies <- as.data.frame(predict(dummyVars(~ HHMTYPE, data = homes), newdata = homes))
homes <- cbind(homes, htype.dummies)

homes$HHMTYPE <- NULL
homes$`HHMTYPE.-1` <- NULL

# Appointment Time

homes$SAPPTAM <- as.factor(as.character(homes$SAPPTAM))

appt.dummies <- as.data.frame(predict(dummyVars(~ SAPPTAM, data = homes), newdata = homes))
homes <- cbind(homes, appt.dummies)

homes$SAPPTAM <- NULL
homes$`SAPPTAM.-1` <- NULL
homes$SAPPTAM.2 <- NULL


# Generate table -------------------------------------------------------------------------------

# Generate summary statistics for all groups 

homes.all <- summarise_all(homes, funs(mean), na.rm = TRUE)
testers.all <- summarise_all(testers, funs(mean), na.rm = TRUE)

# Generate summary statistics by racial groups

homes <- group_by(homes, APRACE)
homes.summary <- summarise_all(homes, funs(mean), na.rm = TRUE)

testers <- group_by(testers, APRACE)
testers.summary <- summarise_all(testers, funs(mean), na.rm = TRUE)

# Generate LaTeX output ------------------------------------------------------------------------

# Transpose tables

homes.all <- as.data.frame(t(homes.all))
testers.all <- as.data.frame(t(testers.all))

homes.summary <- as.data.frame(t(homes.summary))
testers.summary <- as.data.frame(t(testers.summary))

# Combine tables

homes.summary <- cbind(homes.all, homes.summary)
testers.summary <- cbind(testers.all, testers.summary)

# Remove first row

homes.summary <- homes.summary[-1,]
testers.summary <- testers.summary[-1,]

# Digits

for (i in 1:ncol(homes.summary)){
  homes.summary[,i] <- as.numeric(as.character(homes.summary[,i]))
  homes.summary[,i] <- round(homes.summary[,i], digits = 3)
}

for (i in 1:ncol(testers.summary)){
  testers.summary[,i] <- as.numeric(as.character(testers.summary[,i]))
  testers.summary[,i] <- round(testers.summary[,i], digits = 3)
}

# Format row names

# Tester Characteristics (Not Assigned)

rownames(testers.summary)[which(rownames(testers.summary) == "age")] <- "Age"
rownames(testers.summary)[which(rownames(testers.summary) == "TSEX.y")] <- "Percent Male"
rownames(testers.summary)[which(rownames(testers.summary) == "SEQUENCE.1")] <- "Percent Tester Went First"
rownames(testers.summary)[which(rownames(testers.summary) == "TCURTENR.1")] <- "Percent Rented Home"
rownames(testers.summary)[which(rownames(testers.summary) == "TCURTENR.2")] <- "Percent Owned Home"

rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.1")] <- "Under $10,000"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.2")] <- "$10,000 - $19,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.3")] <- "$20,000 - $29,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.4")] <- "$30,000 - $39,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.5")] <- "$40,000 - $49,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.6")] <- "$50,000 - $74,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.7")] <- "$75,000 - $99,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.8")] <- "$100,000 or more"

rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.1")] <- "Grade school or less"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.2")] <- "Attended high school"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.3")] <- "GED"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.4")] <- "High School diploma"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.5")] <- "Attended vocational / technical school"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.6")] <- "Vocational / technical school diploma"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.7")] <- "Attended college"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.8")] <- "Associate's Degree"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.9")] <- "Bachelor's Degree"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.10")] <- "Attended graduate / professional school"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.11")] <- "Graduate / professional degree"

# Tester Characteristics (Assigned)

rownames(testers.summary)[which(rownames(testers.summary) == "ARENTNOW")] <- "Rent"
rownames(testers.summary)[which(rownames(testers.summary) == "ACAROWN")] <- "Percent Car Owner"
rownames(testers.summary)[which(rownames(testers.summary) == "ALEASETP.1")] <- "Month-to-Month"
rownames(testers.summary)[which(rownames(testers.summary) == "ALEASETP.2")] <- "Lease"
rownames(testers.summary)[which(rownames(testers.summary) == "AELNG1")] <- "Length of Employment (Years)"
rownames(testers.summary)[which(rownames(testers.summary) == "ALGNCUR")] <- "Years at Residence"

# Home Characteristics 

rownames(homes.summary)[which(rownames(homes.summary) == "AdPrice")] <- "Listing Price"
rownames(homes.summary)[which(rownames(homes.summary) == "SAPPTAM.1")] <- "Percent Appointment in AM"

rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.1")] <- "Single family, detached"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.2")] <- "Duplex"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.3")] <- "Rowhouse or Townhouse"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.4")] <- "Multi-family structure"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.5")] <- "Mobile home"

rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.1")] <- "Tired of renting"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.2")] <- "Good time to buy"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.3")] <- "Been renting for long time"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.4")] <- "Tax benefits of homeownership"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.5")] <- "Always wanted to own"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.6")] <- "Freedom to decorate my own home"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.7")] <- "Buying first home"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.8")] <- "Ready for responsibility"
rownames(homes.summary)[which(rownames(homes.summary) == "AMOVERS.9")] <- "Recently relocated"

rownames(homes.summary)[which(rownames(homes.summary) == "DPMTEXP.1")] <- "Savings"
rownames(homes.summary)[which(rownames(homes.summary) == "DPMTEXP.2")] <- "Help from parents"
rownames(homes.summary)[which(rownames(homes.summary) == "DPMTEXP.3")] <- "Inherited money"
rownames(homes.summary)[which(rownames(homes.summary) == "DPMTEXP.4")] <- "Equity from previous home"

rownames(homes.summary)[which(rownames(homes.summary) == "SFcount_Ad")] <- "Superfund Sites"
rownames(homes.summary)[which(rownames(homes.summary) == "RespiratoryHazardIndex_Ad")] <- "Respiratory Hazard Index"
rownames(homes.summary)[which(rownames(homes.summary) == "RSEI_Ad")] <- "Risk-Screening Env. Indicators"

# Neighborhood Characteristics

rownames(homes.summary)[which(rownames(homes.summary) == "Assault_Ad")] <- "Assaults"
rownames(homes.summary)[which(rownames(homes.summary) == "Elementary_School_Score_Ad")] <- "Elementary School Quality"

rownames(homes.summary)[which(rownames(homes.summary) == "w2012pc_Ad")] <- "Percent White (Census Block Group)"
rownames(homes.summary)[which(rownames(homes.summary) == "b2012pc_Ad")] <- "Percent African American (Census Block Group)"
rownames(homes.summary)[which(rownames(homes.summary) == "hisp2012pc_Ad")] <- "Percent Hispanic (Census Block Group)"
rownames(homes.summary)[which(rownames(homes.summary) == "a2012pc_Ad")] <- "Percent Asian (Census Block Group)"

rownames(homes.summary)[which(rownames(homes.summary) == "college_Ad")] <- "Percent College Graduate (Census Block Group)"
rownames(homes.summary)[which(rownames(homes.summary) == "skill_Ad")] <- "Percent High Skill (Census Block Group)"
rownames(homes.summary)[which(rownames(homes.summary) == "povrate_Ad")] <- "Poverty Rate (Census Block Group)"

# ----------------------------------------------------------------------------------------------
# For APRACE variable: 
#   1 = White
#   2 = Black / African American
#   3 = Hispanic
#   4 = Asian
#   5 = Other (specify which)

# Drop descriptive statistics for race category 'Other'

homes.summary$V5 <- NULL
testers.summary$V5 <- NULL

# Output ---------------------------------------------------------------------------------------

# Save summary

saveRDS(homes.summary, paste0(out, "homes-summary.rds"))
saveRDS(testers.summary, paste0(out, "testers-summary.rds"))

# Convert to LaTeX using stargazer

stargazer(homes.summary,
          type = "latex",
          title = "Home and Neighborhood Characteristics",
          out = paste0(out, "homes-desc.tex"),
          summary = FALSE,
          covariate.labels = c("Variable",
                               "All Groups",
                               "White",
                               "African American",
                               "Hispanic",
                               "Asian"))

stargazer(testers.summary,
          type = "latex",
          title = "Tester Characteristics",
          out = paste0(out, "testers-desc.tex"),
          summary = FALSE,
          covariate.labels = c("Variable",
                               "All Groups",
                               "White",
                               "African American",
                               "Hispanic",
                               "Asian"))
