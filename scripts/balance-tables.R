# ---------------------------------------------------------------------------------------------- #
#   Generate balance tables for HUD Discrimination paper                                         #
#                                                                                                # 
#   Author: Amanda Ang, Peter Christensen                                                        #
#   Date created: 1/23/2018                                                                      #
#   Edited on:    3/7/2018                                                                       #
#   Edited on 05/30/2018 to add Assault and Elementary School Score                              #
# ---------------------------------------------------------------------------------------------- #

# Clear workspace

rm(list = ls())

# Set working directory

setwd("/homes/bdeep/share/projects/HUD_Discrimination/")

# Define function for loading packages

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Load packages

packages <- c("caret", "dplyr", "lfe", "stargazer")
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

out <- "views/tables/balance tables/"

# Balance statistics advertised homes by race --------------------------------------------------

# ***This is using self identified racial categories***
# APRACE: 
#   1 = White 
#   2 = Black/African-American 
#   3 = Hispanic 
#   4 = Asian/Pacific Islander 
#   5 = Other (Specify)

# APRACE is the variable of interest

testers$APRACE <- as.factor(testers$APRACE)

# Tester Characteristics -------------------------------------------------------------------

# Test Sequence --------------------------------------------------------------------------------

testers$SEQUENCE <- as.factor(testers$SEQUENCE)

seq.dummies <- as.data.frame(predict(dummyVars(~ SEQUENCE, data = testers), newdata = testers))
testers <- cbind(testers, seq.dummies)


t1 <- felm(SEQUENCE.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t1.coef <- as.data.frame(coef(summary(t1)))
names(t1.coef)[which(names(t1.coef) == "Estimate")] <- "Percent Tester Went First"
t1.coef <- as.data.frame(t(t1.coef))

rm(t1)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t1 <- felm(SEQUENCE.1 ~ APRACE, data = testers)

t1.coef <- as.data.frame(coef(summary(t1)))
names(t1.coef)[which(names(t1.coef) == "Estimate")] <- "Percent Tester Went First"
t1.coef <- as.data.frame(t(t1.coef))


# Generate table

# Drop race category 'Other'

t1.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t1.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t1.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "test-sequence.tex"))

# without CONTROL

stargazer(t1.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Month of Test --------------------------------------------------------------------------------

testers$month <- as.factor(testers$month)

month.dummies <- as.data.frame(predict(dummyVars(~ month, data = testers), newdata = testers))
testers <- cbind(testers, month.dummies)

# Jan

t2.1 <- felm(month.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.1.coef <- as.data.frame(coef(summary(t2.1)))
names(t2.1.coef)[which(names(t2.1.coef) == "Estimate")] <- "Jan"
t2.1.coef <- as.data.frame(t(t2.1.coef))

rm(t2.1)

# Feb

t2.2 <- felm(month.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.2.coef <- as.data.frame(coef(summary(t2.2)))
names(t2.2.coef)[which(names(t2.2.coef) == "Estimate")] <- "Feb"
t2.2.coef <- as.data.frame(t(t2.2.coef))

rm(t2.2)

# Mar

t2.3 <- felm(month.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.3.coef <- as.data.frame(coef(summary(t2.3)))
names(t2.3.coef)[which(names(t2.3.coef) == "Estimate")] <- "Mar"
t2.3.coef <- as.data.frame(t(t2.3.coef))

rm(t2.3)

# Apr

t2.4 <- felm(month.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.4.coef <- as.data.frame(coef(summary(t2.4)))
names(t2.4.coef)[which(names(t2.4.coef) == "Estimate")] <- "Apr"
t2.4.coef <- as.data.frame(t(t2.4.coef))

rm(t2.4)

# May

t2.5 <- felm(month.5 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.5.coef <- as.data.frame(coef(summary(t2.5)))
names(t2.5.coef)[which(names(t2.5.coef) == "Estimate")] <- "May"
t2.5.coef <- as.data.frame(t(t2.5.coef))

rm(t2.5)

# Jun

t2.6 <- felm(month.6 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.6.coef <- as.data.frame(coef(summary(t2.6)))
names(t2.6.coef)[which(names(t2.6.coef) == "Estimate")] <- "Jun"
t2.6.coef <- as.data.frame(t(t2.6.coef))

rm(t2.6)

# Jul

t2.7 <- felm(month.7 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.7.coef <- as.data.frame(coef(summary(t2.7)))
names(t2.7.coef)[which(names(t2.7.coef) == "Estimate")] <- "Jul"
t2.7.coef <- as.data.frame(t(t2.7.coef))

rm(t2.7)

# Aug

t2.8 <- felm(month.8 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.8.coef <- as.data.frame(coef(summary(t2.8)))
names(t2.8.coef)[which(names(t2.8.coef) == "Estimate")] <- "Aug"
t2.8.coef <- as.data.frame(t(t2.8.coef))

rm(t2.8)

# Sept

t2.9 <- felm(month.9 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.9.coef <- as.data.frame(coef(summary(t2.9)))
names(t2.9.coef)[which(names(t2.9.coef) == "Estimate")] <- "Sept"
t2.9.coef <- as.data.frame(t(t2.9.coef))

rm(t2.9)

# Oct

t2.10 <- felm(month.10 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.10.coef <- as.data.frame(coef(summary(t2.10)))
names(t2.10.coef)[which(names(t2.10.coef) == "Estimate")] <- "Oct"
t2.10.coef <- as.data.frame(t(t2.10.coef))

rm(t2.10)

# Nov 

t2.11 <- felm(month.11 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.11.coef <- as.data.frame(coef(summary(t2.11)))
names(t2.11.coef)[which(names(t2.11.coef) == "Estimate")] <- "Nov"
t2.11.coef <- as.data.frame(t(t2.11.coef))

rm(t2.11)

# Dec

t2.12 <- felm(month.12 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t2.12.coef <- as.data.frame(coef(summary(t2.12)))
names(t2.12.coef)[which(names(t2.12.coef) == "Estimate")] <- "Dec"
t2.12.coef <- as.data.frame(t(t2.12.coef))

rm(t2.12)

# Generate table

# Combine into one data frame

t2.coef <- rbind(t2.1.coef,
                 t2.2.coef,
                 t2.3.coef,
                 t2.4.coef,
                 t2.5.coef,
                 t2.6.coef,
                 t2.7.coef,
                 t2.8.coef,
                 t2.9.coef,
                 t2.10.coef,
                 t2.11.coef,
                 t2.12.coef)

# Drop race category 'Other'

t2.coef$APRACE5 <- NULL


# Convert into LaTeX using stargazer

stargazer(t2.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "month.tex"))


# Reason for Moving ----------------------------------------------------------------------------

testers$AMOVERS <- as.factor(testers$AMOVERS)
testers <- testers[which(!is.na(testers$AMOVERS)),]

reason.dummies <- as.data.frame(predict(dummyVars(~ AMOVERS, data = testers), newdata = testers))
testers <- cbind(testers, reason.dummies)

# 1 = Ready For Change

t4.1 <- felm(AMOVERS.1 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.1.coef <- as.data.frame(coef(summary(t4.1)))
names(t4.1.coef)[which(names(t4.1.coef) == "Estimate")] <- "Ready for change"
t4.1.coef <- as.data.frame(t(t4.1.coef))

rm(t4.1)

# 2 = Good Time To Buy

t4.2 <- felm(AMOVERS.2 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.2.coef <- as.data.frame(coef(summary(t4.2)))
names(t4.2.coef)[which(names(t4.2.coef) == "Estimate")] <- "Good time to buy"
t4.2.coef <- as.data.frame(t(t4.2.coef))

rm(t4.2)

# 3 = Renting for Long Time 

t4.3 <- felm(AMOVERS.3 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.3.coef <- as.data.frame(coef(summary(t4.3)))
names(t4.3.coef)[which(names(t4.3.coef) == "Estimate")] <- "Renting for long time"
t4.3.coef <- as.data.frame(t(t4.3.coef))

rm(t4.3)


# 4 = Tax Benefits of Home Ownership

t4.4 <- felm(AMOVERS.4 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.4.coef <- as.data.frame(coef(summary(t4.4)))
names(t4.4.coef)[which(names(t4.4.coef) == "Estimate")] <- "Tax benefits"
t4.4.coef <- as.data.frame(t(t4.4.coef))

rm(t4.4)


# 5 = Now Able To Take Time To Own A Home

t4.5 <- felm(AMOVERS.5 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.5.coef <- as.data.frame(coef(summary(t4.5)))
names(t4.5.coef)[which(names(t4.5.coef) == "Estimate")] <- "Now have the time"
t4.5.coef <- as.data.frame(t(t4.5.coef))

rm(t4.5)


# 6 = Freedom to Decorate My Own Home

t4.6 <- felm(AMOVERS.6 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.6.coef <- as.data.frame(coef(summary(t4.6)))
names(t4.6.coef)[which(names(t4.6.coef) == "Estimate")] <- "Freedom to decorate"
t4.6.coef <- as.data.frame(t(t4.6.coef))

rm(t4.6)


# 7 = Buying First Home

t4.7 <- felm(AMOVERS.7 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.7.coef <- as.data.frame(coef(summary(t4.7)))
names(t4.7.coef)[which(names(t4.7.coef) == "Estimate")] <- "Buying first home"
t4.7.coef <- as.data.frame(t(t4.7.coef))

rm(t4.7)


# 8 = Ready for Responsibility of Home Ownership

t4.8 <- felm(AMOVERS.8 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.8.coef <- as.data.frame(coef(summary(t4.8)))
names(t4.8.coef)[which(names(t4.8.coef) == "Estimate")] <- "Ready for responsibility"
t4.8.coef <- as.data.frame(t(t4.8.coef))

rm(t4.8)


# 9 = Recently Relocated

t4.9 <- felm(AMOVERS.9 ~ ARACE1 | CONTROL | 0 | CONTROL, data = testers)

t4.9.coef <- as.data.frame(coef(summary(t4.9)))
names(t4.9.coef)[which(names(t4.9.coef) == "Estimate")] <- "Recently relocated"
t4.9.coef <- as.data.frame(t(t4.9.coef))

rm(t4.9)

# Generate table

# Combine into one data frame

t4.coef <- rbind(t4.1.coef,
                 t4.2.coef,
                 t4.3.coef,
                 t4.4.coef,
                 t4.5.coef,
                 t4.6.coef,
                 t4.7.coef,
                 t4.8.coef,
                 t4.9.coef)

# Drop race category 'Other'

t4.coef$ARACE15 <- NULL

# Convert into LaTeX using stargazer

stargazer(t4.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "reason.tex"))


# Gender ---------------------------------------------------------------------------------------

testers$TSEX.x <- as.factor(testers$TSEX.x)

gender.dummies <- as.data.frame(predict(dummyVars(~ TSEX.x, data = testers), newdata = testers))
testers <- cbind(testers, gender.dummies)

t5 <- felm(TSEX.x.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t5.coef <- as.data.frame(coef(summary(t5)))
names(t5.coef)[which(names(t5.coef) == "Estimate")] <- "Percent Male"
t5.coef <- as.data.frame(t(t5.coef))

rm(t5)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t5 <- felm(TSEX.x.1 ~ APRACE, data = testers)

t5.coef <- as.data.frame(coef(summary(t5)))
names(t5.coef)[which(names(t5.coef) == "Estimate")] <- "Percent Male"
t5.coef <- as.data.frame(t(t5.coef))

# Generate table

# Drop race category 'Other'

t5.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t5.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t5.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "gender.tex"))

# without CONTROL

stargazer(t5.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Age ------------------------------------------------------------------------------------------

t6 <- felm(age ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t6.coef <- as.data.frame(coef(summary(t6)))
names(t6.coef)[which(names(t6.coef) == "Estimate")] <- "Age"
t6.coef <- as.data.frame(t(t6.coef))

rm(t6)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t6 <- felm(age ~ APRACE, data = testers)

t6.coef <- as.data.frame(coef(summary(t6)))
names(t6.coef)[which(names(t6.coef) == "Estimate")] <- "Age"
t6.coef <- as.data.frame(t(t6.coef))


# Generate table

# Drop race category 'Other'

t6.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t6.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t6.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "age.tex"))


# without CONTROL

stargazer(t6.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))


# Income ---------------------------------------------------------------------------------------

# Run model separately for each income bin

testers$TPEGAI <- as.factor(testers$TPEGAI)

inc.dummies <- as.data.frame(predict(dummyVars(~ TPEGAI, data = testers), newdata = testers))
testers <- cbind(testers, inc.dummies)

# 1 = Under $10,000

t7.1 <- felm(TPEGAI.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.1.coef <- as.data.frame(coef(summary(t7.1)))
names(t7.1.coef)[which(names(t7.1.coef) == "Estimate")] <- "Under $10,000"
t7.1.coef <- as.data.frame(t(t7.1.coef))

rm(t7.1)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.1 <- felm(TPEGAI.1 ~ APRACE, data = testers)

t7.1.coef <- as.data.frame(coef(summary(t7.1)))
names(t7.1.coef)[which(names(t7.1.coef) == "Estimate")] <- "Under $10,000"
t7.1.coef <- as.data.frame(t(t7.1.coef))


# 2 = $10,000 - $19,999

t7.2 <- felm(TPEGAI.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.2.coef <- as.data.frame(coef(summary(t7.2)))
names(t7.2.coef)[which(names(t7.2.coef) == "Estimate")] <- "$10,000 - $19,999"
t7.2.coef <- as.data.frame(t(t7.2.coef))

rm(t7.2)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.2 <- felm(TPEGAI.2 ~ APRACE, data = testers)

t7.2.coef <- as.data.frame(coef(summary(t7.2)))
names(t7.2.coef)[which(names(t7.2.coef) == "Estimate")] <- "$10,000 - $19,999"
t7.2.coef <- as.data.frame(t(t7.2.coef))


# 3 = $20,000 - $29,999

t7.3 <- felm(TPEGAI.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.3.coef <- as.data.frame(coef(summary(t7.3)))
names(t7.3.coef)[which(names(t7.3.coef) == "Estimate")] <- "$20,000 - $29,999"
t7.3.coef <- as.data.frame(t(t7.3.coef))

rm(t7.3)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.3 <- felm(TPEGAI.3 ~ APRACE, data = testers)

t7.3.coef <- as.data.frame(coef(summary(t7.3)))
names(t7.3.coef)[which(names(t7.3.coef) == "Estimate")] <- "$20,000 - $29,999"
t7.3.coef <- as.data.frame(t(t7.3.coef))


# 4 = $30,000 - $39,999

t7.4 <- felm(TPEGAI.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.4.coef <- as.data.frame(coef(summary(t7.4)))
names(t7.4.coef)[which(names(t7.4.coef) == "Estimate")] <- "$30,000 - $39,999"
t7.4.coef <- as.data.frame(t(t7.4.coef))

rm(t7.4)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.4 <- felm(TPEGAI.4 ~ APRACE, data = testers)

t7.4.coef <- as.data.frame(coef(summary(t7.4)))
names(t7.4.coef)[which(names(t7.4.coef) == "Estimate")] <- "$30,000 - $39,999"
t7.4.coef <- as.data.frame(t(t7.4.coef))


# 5 = $40,000 - $49,999

t7.5 <- felm(TPEGAI.5 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.5.coef <- as.data.frame(coef(summary(t7.5)))
names(t7.5.coef)[which(names(t7.5.coef) == "Estimate")] <- "$40,000 - $49,999"
t7.5.coef <- as.data.frame(t(t7.5.coef))

rm(t7.5)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.5 <- felm(TPEGAI.5 ~ APRACE, data = testers)

t7.5.coef <- as.data.frame(coef(summary(t7.5)))
names(t7.5.coef)[which(names(t7.5.coef) == "Estimate")] <- "$40,000 - $49,999"
t7.5.coef <- as.data.frame(t(t7.5.coef))


# 6 = $50,000 - $74,999

t7.6 <- felm(TPEGAI.6 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.6.coef <- as.data.frame(coef(summary(t7.6)))
names(t7.6.coef)[which(names(t7.6.coef) == "Estimate")] <- "$50,000 - $74,999"
t7.6.coef <- as.data.frame(t(t7.6.coef))

rm(t7.6)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.6 <- felm(TPEGAI.6 ~ APRACE, data = testers)

t7.6.coef <- as.data.frame(coef(summary(t7.6)))
names(t7.6.coef)[which(names(t7.6.coef) == "Estimate")] <- "$50,000 - $74,999"
t7.6.coef <- as.data.frame(t(t7.6.coef))


# 7 = $75,000 - 99,999

t7.7 <- felm(TPEGAI.7 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.7.coef <- as.data.frame(coef(summary(t7.7)))
names(t7.7.coef)[which(names(t7.7.coef) == "Estimate")] <- "$75,000 - $99,999"
t7.7.coef <- as.data.frame(t(t7.7.coef))

rm(t7.7)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.7 <- felm(TPEGAI.7 ~ APRACE, data = testers)

t7.7.coef <- as.data.frame(coef(summary(t7.7)))
names(t7.7.coef)[which(names(t7.7.coef) == "Estimate")] <- "$75,000 - $99,999"
t7.7.coef <- as.data.frame(t(t7.7.coef))


# 8 = $100,000 or more

t7.8 <- felm(TPEGAI.8 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.8.coef <- as.data.frame(coef(summary(t7.8)))
names(t7.8.coef)[which(names(t7.8.coef) == "Estimate")] <- "$100,000 or more"
t7.8.coef <- as.data.frame(t(t7.8.coef))

rm(t7.8)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t7.8 <- felm(TPEGAI.8 ~ APRACE, data = testers)

t7.8.coef <- as.data.frame(coef(summary(t7.8)))
names(t7.8.coef)[which(names(t7.8.coef) == "Estimate")] <- "$100,000 or more"
t7.8.coef <- as.data.frame(t(t7.8.coef))


# Generate table

# Combine into one data frame

t7.coef <- rbind(t7.1.coef,
                 t7.2.coef,
                 t7.3.coef,
                 t7.4.coef,
                 t7.5.coef,
                 t7.6.coef,
                 t7.7.coef,
                 t7.8.coef)

# Drop race category 'Other'

t7.coef$APRACE5 <- NULL

# Drop (Intercept)

t7.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t7.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "income.tex"))

# without CONTROL

stargazer(t7.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))


# Household Income -----------------------------------------------------------------------------

testers$THHEGAI <- as.factor(testers$THHEGAI)

hhinc.dummies <- as.data.frame(predict(dummyVars(~ THHEGAI, data = testers), newdata = testers))
testers <- cbind(testers, hhinc.dummies)


# 1 = Under $10,000

t8.1 <- felm(THHEGAI.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.1.coef <- as.data.frame(coef(summary(t8.1)))
names(t8.1.coef)[which(names(t8.1.coef) == "Estimate")] <- "Under $10,000"
t8.1.coef <- as.data.frame(t(t8.1.coef))

rm(t8.1)

# 2 = $10,000 - $19,999

t8.2 <- felm(THHEGAI.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.2.coef <- as.data.frame(coef(summary(t8.2)))
names(t8.2.coef)[which(names(t8.2.coef) == "Estimate")] <- "$10,000 - $19,999"
t8.2.coef <- as.data.frame(t(t8.2.coef))

rm(t8.2)

# 3 = $20,000 - $29,999

t8.3 <- felm(THHEGAI.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.3.coef <- as.data.frame(coef(summary(t8.3)))
names(t8.3.coef)[which(names(t8.3.coef) == "Estimate")] <- "$20,000 - $29,999"
t8.3.coef <- as.data.frame(t(t8.3.coef))

rm(t8.3)


# 4 = $30,000 - $39,999

t8.4 <- felm(THHEGAI.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.4.coef <- as.data.frame(coef(summary(t8.4)))
names(t8.4.coef)[which(names(t8.4.coef) == "Estimate")] <- "$30,000 - $39,999"
t8.4.coef <- as.data.frame(t(t8.4.coef))

rm(t8.4)


# 5 = $40,000 - $49,999

t8.5 <- felm(THHEGAI.5 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.5.coef <- as.data.frame(coef(summary(t8.5)))
names(t8.5.coef)[which(names(t8.5.coef) == "Estimate")] <- "$40,000 - $49,999"
t8.5.coef <- as.data.frame(t(t8.5.coef))

rm(t8.5)


# 6 = $50,000 - $59,999

t8.6 <- felm(THHEGAI.6 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.6.coef <- as.data.frame(coef(summary(t8.6)))
names(t8.6.coef)[which(names(t8.6.coef) == "Estimate")] <- "$50,000 - $59,999"
t8.6.coef <- as.data.frame(t(t8.6.coef))

rm(t8.6)


# 7 = $60,000 - $69,999

t8.7 <- felm(THHEGAI.7 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.7.coef <- as.data.frame(coef(summary(t8.7)))
names(t8.7.coef)[which(names(t8.7.coef) == "Estimate")] <- "$60,000 - $69,999"
t8.7.coef <- as.data.frame(t(t8.7.coef))

rm(t8.7)


# 8 = $70,000 - $79,999

t8.8 <- felm(THHEGAI.8 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.8.coef <- as.data.frame(coef(summary(t8.8)))
names(t8.8.coef)[which(names(t8.8.coef) == "Estimate")] <- "$70,000 - $79,999"
t8.8.coef <- as.data.frame(t(t8.8.coef))

rm(t8.8)


# 9 = $80,000 - $99,999

t8.9 <- felm(THHEGAI.9 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.9.coef <- as.data.frame(coef(summary(t8.9)))
names(t8.9.coef)[which(names(t8.9.coef) == "Estimate")] <- "$80,000 - $99,999"
t8.9.coef <- as.data.frame(t(t8.9.coef))

rm(t8.9)


# 10 = $100,000 - $149,999

t8.10 <- felm(THHEGAI.10 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.10.coef <- as.data.frame(coef(summary(t8.10)))
names(t8.10.coef)[which(names(t8.10.coef) == "Estimate")] <- "$100,000 - $149,999"
t8.10.coef <- as.data.frame(t(t8.10.coef))

rm(t8.10)


# 11 = $150,000 or more

t8.11 <- felm(THHEGAI.11 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t8.11.coef <- as.data.frame(coef(summary(t8.11)))
names(t8.11.coef)[which(names(t8.11.coef) == "Estimate")] <- "$150,000 or more"
t8.11.coef <- as.data.frame(t(t8.11.coef))

rm(t8.11)

# Generate table

# Combine into one data frame

t8.coef <- rbind(t8.1.coef,
                 t8.2.coef,
                 t8.3.coef,
                 t8.4.coef,
                 t8.5.coef,
                 t8.6.coef,
                 t8.7.coef,
                 t8.8.coef,
                 t8.9.coef,
                 t8.10.coef,
                 t8.11.coef)


# Drop race category 'Other'

t8.coef$APRACE5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t8.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "hh-income.tex"))



# Education ------------------------------------------------------------------------------------

testers$THIGHEDU <- as.factor(testers$THIGHEDU)

edu.dummies <- as.data.frame(predict(dummyVars(~ THIGHEDU, data = testers), newdata = testers))
testers <- cbind(testers, edu.dummies)

# 1 = Grade School Or Less

t9.1 <- felm(THIGHEDU.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.1.coef <- as.data.frame(coef(summary(t9.1)))
names(t9.1.coef)[which(names(t9.1.coef) == "Estimate")] <- "Grade School Or Less"
t9.1.coef <- as.data.frame(t(t9.1.coef))

rm(t9.1)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.1 <- felm(THIGHEDU.1 ~ APRACE, data = testers)

t9.1.coef <- as.data.frame(coef(summary(t9.1)))
names(t9.1.coef)[which(names(t9.1.coef) == "Estimate")] <- "Grade School Or Less"
t9.1.coef <- as.data.frame(t(t9.1.coef))

# 2 = Attended High School

t9.2 <- felm(THIGHEDU.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.2.coef <- as.data.frame(coef(summary(t9.2)))
names(t9.2.coef)[which(names(t9.2.coef) == "Estimate")] <- "Attended High School"
t9.2.coef <- as.data.frame(t(t9.2.coef))

rm(t9.2)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.2 <- felm(THIGHEDU.2 ~ APRACE, data = testers)

t9.2.coef <- as.data.frame(coef(summary(t9.2)))
names(t9.2.coef)[which(names(t9.2.coef) == "Estimate")] <- "Attended High School"
t9.2.coef <- as.data.frame(t(t9.2.coef))


# 3 = GED

t9.3 <- felm(THIGHEDU.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.3.coef <- as.data.frame(coef(summary(t9.3)))
names(t9.3.coef)[which(names(t9.3.coef) == "Estimate")] <- "GED"
t9.3.coef <- as.data.frame(t(t9.3.coef))

rm(t9.3)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.3 <- felm(THIGHEDU.3 ~ APRACE, data = testers)

t9.3.coef <- as.data.frame(coef(summary(t9.3)))
names(t9.3.coef)[which(names(t9.3.coef) == "Estimate")] <- "GED"
t9.3.coef <- as.data.frame(t(t9.3.coef))


# 4 = High School Diploma

t9.4 <- felm(THIGHEDU.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.4.coef <- as.data.frame(coef(summary(t9.4)))
names(t9.4.coef)[which(names(t9.4.coef) == "Estimate")] <- "High School Diploma"
t9.4.coef <- as.data.frame(t(t9.4.coef))

rm(t9.4)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.4 <- felm(THIGHEDU.4 ~ APRACE, data = testers)

t9.4.coef <- as.data.frame(coef(summary(t9.4)))
names(t9.4.coef)[which(names(t9.4.coef) == "Estimate")] <- "High School Diploma"
t9.4.coef <- as.data.frame(t(t9.4.coef))


# 5 = Attended Vocational / Technical School

t9.5 <- felm(THIGHEDU.5 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.5.coef <- as.data.frame(coef(summary(t9.5)))
names(t9.5.coef)[which(names(t9.5.coef) == "Estimate")] <- "Attended Vocational / Technical School"
t9.5.coef <- as.data.frame(t(t9.5.coef))

rm(t9.5)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.5 <- felm(THIGHEDU.5 ~ APRACE, data = testers)

t9.5.coef <- as.data.frame(coef(summary(t9.5)))
names(t9.5.coef)[which(names(t9.5.coef) == "Estimate")] <- "Attended Vocational / Technical School"
t9.5.coef <- as.data.frame(t(t9.5.coef))


# 6 = Vocational / Technical School Diploma

t9.6 <- felm(THIGHEDU.6 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.6.coef <- as.data.frame(coef(summary(t9.6)))
names(t9.6.coef)[which(names(t9.6.coef) == "Estimate")] <- "Vocational / Technical School Diploma"
t9.6.coef <- as.data.frame(t(t9.6.coef))

rm(t9.6)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.6 <- felm(THIGHEDU.6 ~ APRACE, data = testers)

t9.6.coef <- as.data.frame(coef(summary(t9.6)))
names(t9.6.coef)[which(names(t9.6.coef) == "Estimate")] <- "Vocational / Technical School Diploma"
t9.6.coef <- as.data.frame(t(t9.6.coef))


# 7 = Attended College

t9.7 <- felm(THIGHEDU.7 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.7.coef <- as.data.frame(coef(summary(t9.7)))
names(t9.7.coef)[which(names(t9.7.coef) == "Estimate")] <- "Attended College"
t9.7.coef <- as.data.frame(t(t9.7.coef))

rm(t9.7)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.7 <- felm(THIGHEDU.7 ~ APRACE, data = testers)

t9.7.coef <- as.data.frame(coef(summary(t9.7)))
names(t9.7.coef)[which(names(t9.7.coef) == "Estimate")] <- "Attended College"
t9.7.coef <- as.data.frame(t(t9.7.coef))


# 8 = Associate's Degree

t9.8 <- felm(THIGHEDU.8 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.8.coef <- as.data.frame(coef(summary(t9.8)))
names(t9.8.coef)[which(names(t9.8.coef) == "Estimate")] <- "Associate's Degree"
t9.8.coef <- as.data.frame(t(t9.8.coef))

rm(t9.8)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.8 <- felm(THIGHEDU.8 ~ APRACE, data = testers)

t9.8.coef <- as.data.frame(coef(summary(t9.8)))
names(t9.8.coef)[which(names(t9.8.coef) == "Estimate")] <- "Associate's Degree"
t9.8.coef <- as.data.frame(t(t9.8.coef))


# 9 = Bachelor's Degree 

t9.9 <- felm(THIGHEDU.9 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.9.coef <- as.data.frame(coef(summary(t9.9)))
names(t9.9.coef)[which(names(t9.9.coef) == "Estimate")] <- "Bachelor's Degree"
t9.9.coef <- as.data.frame(t(t9.9.coef))

rm(t9.9)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.9 <- felm(THIGHEDU.9 ~ APRACE, data = testers)

t9.9.coef <- as.data.frame(coef(summary(t9.9)))
names(t9.9.coef)[which(names(t9.9.coef) == "Estimate")] <- "Bachelor's Degree"
t9.9.coef <- as.data.frame(t(t9.9.coef))


# 10 = Attended Graduate / Professional School

t9.10 <- felm(THIGHEDU.10 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.10.coef <- as.data.frame(coef(summary(t9.10)))
names(t9.10.coef)[which(names(t9.10.coef) == "Estimate")] <- "Attended Graduate / Professional School"
t9.10.coef <- as.data.frame(t(t9.10.coef))

rm(t9.10)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.10 <- felm(THIGHEDU.10 ~ APRACE, data = testers)

t9.10.coef <- as.data.frame(coef(summary(t9.10)))
names(t9.10.coef)[which(names(t9.10.coef) == "Estimate")] <- "Attended Graduate / Professional School"
t9.10.coef <- as.data.frame(t(t9.10.coef))


# 11 = Graduate / Professional Degree

t9.11 <- felm(THIGHEDU.11 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.11.coef <- as.data.frame(coef(summary(t9.11)))
names(t9.11.coef)[which(names(t9.11.coef) == "Estimate")] <- "Graduate / Professional Degree"
t9.11.coef <- as.data.frame(t(t9.11.coef))

rm(t9.11)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t9.11 <- felm(THIGHEDU.11 ~ APRACE, data = testers)

t9.11.coef <- as.data.frame(coef(summary(t9.11)))
names(t9.11.coef)[which(names(t9.11.coef) == "Estimate")] <- "Graduate / Professional Degree"
t9.11.coef <- as.data.frame(t(t9.11.coef))


# Generate table

# Combine into one data frame

t9.coef <- rbind(t9.2.coef,
                 t9.3.coef,
                 t9.4.coef,
                 t9.5.coef,
                 t9.6.coef,
                 t9.7.coef,
                 t9.8.coef,
                 t9.9.coef,
                 t9.10.coef,
                 t9.11.coef)

# Drop race category 'Other'

t9.coef$APRACE5 <- NULL

# Drop (Intercept)

t9.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t9.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "edu.tex"))

# without CONTROL

stargazer(t9.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Homeowner ------------------------------------------------------------------------------------

# Do you presently rent or own your home? 
# 1 = rent
# 2 = own
# 3 = other (specify)

testers$TCURTENR <- as.factor(testers$TCURTENR)

own.dummies <- as.data.frame(predict(dummyVars(~ TCURTENR, data = testers), newdata = testers))
testers <- cbind(testers, own.dummies)

# rent home

t10.1 <- felm(TCURTENR.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t10.1.coef <- as.data.frame(coef(summary(t10.1)))
names(t10.1.coef)[which(names(t10.1.coef) == "Estimate")] <- "Percent Rented Home"
t10.1.coef <- as.data.frame(t(t10.1.coef))

rm(t10.1)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t10.1 <- felm(TCURTENR.1 ~ APRACE, data = testers)

t10.1.coef <- as.data.frame(coef(summary(t10.1)))
names(t10.1.coef)[which(names(t10.1.coef) == "Estimate")] <- "Percent Rented Home"
t10.1.coef <- as.data.frame(t(t10.1.coef))


# own home

t10.2 <- felm(TCURTENR.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t10.2.coef <- as.data.frame(coef(summary(t10.2)))
names(t10.2.coef)[which(names(t10.2.coef) == "Estimate")] <- "Percent Owned Home"
t10.2.coef <- as.data.frame(t(t10.2.coef))

rm(t10.2)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t10.2 <- felm(TCURTENR.2 ~ APRACE, data = testers)

t10.2.coef <- as.data.frame(coef(summary(t10.2)))
names(t10.2.coef)[which(names(t10.2.coef) == "Estimate")] <- "Percent Owned Home"
t10.2.coef <- as.data.frame(t(t10.2.coef))


# Generate table

# Combine into one dataframe

t10.coef <- rbind(t10.1.coef, t10.2.coef)

# Drop race category 'Other'

t10.coef$APRACE5 <- NULL

# Drop (Intercept)

t10.coef$`(Intercept)` <- NULL 

# Convert into LaTeX using stargazer

stargazer(t10.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "homeowner.tex"))


# without CONTROL

stargazer(t10.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))


# Length of Employment (Assigned) -------------------------------------------------------------------------

t11 <- felm(AELNG1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t11.coef <- as.data.frame(coef(summary(t11)))
names(t11.coef)[which(names(t11.coef) == "Estimate")] <- "Length of Employment (Years)"
t11.coef <- as.data.frame(t(t11.coef))

rm(t11)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t11 <- felm(AELNG1 ~ APRACE, data = testers)

t11.coef <- as.data.frame(coef(summary(t11)))
names(t11.coef)[which(names(t11.coef) == "Estimate")] <- "Length of Employment (Years)"
t11.coef <- as.data.frame(t(t11.coef))


# Generate table

# Drop race category 'Other'

t11.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t11.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t11.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "length_emp.tex"))

# without CONTROL

stargazer(t11.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))


# Down Payment Reason --------------------------------------------------------------------------

testers$DPMTEXP <- as.factor(testers$DPMTEXP)

dpreason.dummies <- as.data.frame(predict(dummyVars(~ DPMTEXP, data = testers), newdata = testers))
testers <- cbind(testers, dpreason.dummies)

# 1 = I've been saving for quite a while

t12.1 <- felm(DPMTEXP.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t12.1.coef <- as.data.frame(coef(summary(t12.1)))
names(t12.1.coef)[which(names(t12.1.coef) == "Estimate")] <- "Personal Savings"
t12.1.coef <- as.data.frame(t(t12.1.coef))

rm(t12.1)


# 2 = My / our parents are helping me/us

t12.2 <- felm(DPMTEXP.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t12.2.coef <- as.data.frame(coef(summary(t12.2)))
names(t12.2.coef)[which(names(t12.2.coef) == "Estimate")] <- "Help From Parents"
t12.2.coef <- as.data.frame(t(t12.2.coef))

rm(t12.2)


# 3 = I inherited money from a relative

t12.3 <- felm(DPMTEXP.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t12.3.coef <- as.data.frame(coef(summary(t12.3)))
names(t12.3.coef)[which(names(t12.3.coef) == "Estimate")] <- "Inherited Money"
t12.3.coef <- as.data.frame(t(t12.3.coef))

rm(t12.3)


# 4 = I/we had equity in a previously owned home 

t12.4 <- felm(DPMTEXP.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t12.4.coef <- as.data.frame(coef(summary(t12.4)))
names(t12.4.coef)[which(names(t12.4.coef) == "Estimate")] <- "Equity From Previous Home"
t12.4.coef <- as.data.frame(t(t12.4.coef))

rm(t12.4)

# Generate table

# Combine into one data frame

t12.coef <- rbind(t12.1.coef,
                  t12.2.coef,
                  t12.3.coef,
                  t12.4.coef)

# Drop race category 'Other'

t12.coef$APRACE5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t12.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "dp_reason.tex"))



# Years at Residence (Assigned) ---------------------------------------------------------------------------

t13 <- felm(ALGNCUR ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t13.coef <- as.data.frame(coef(summary(t13)))
names(t13.coef)[which(names(t13.coef) == "Estimate")] <- "Years at Residence"
t13.coef <- as.data.frame(t(t13.coef))

rm(t13)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t13 <- felm(ALGNCUR ~ APRACE, data = testers)

t13.coef <- as.data.frame(coef(summary(t13)))
names(t13.coef)[which(names(t13.coef) == "Estimate")] <- "Years at Residence"
t13.coef <- as.data.frame(t(t13.coef))


# Generate table

# Drop race category 'Other'

t13.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t13.coef$`(Intercept)` <- NULL


# Convert into LaTeX using stargazer

stargazer(t13.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "length_res.tex"))

# without CONTROL

stargazer(t13.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))

# Lease Type -----------------------------------------------------------------------------------

testers$ALEASETP <- as.factor(testers$ALEASETP)

leasetp.dummies <- as.data.frame(predict(dummyVars(~ ALEASETP, data = testers), newdata = testers))
testers <- cbind(testers, leasetp.dummies)

# 1 = Month - to - Month

t14.1 <- felm(ALEASETP.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t14.1.coef <- as.data.frame(coef(summary(t14.1)))
names(t14.1.coef)[which(names(t14.1.coef) == "Estimate")] <- "Month-to-Month"
t14.1.coef <- as.data.frame(t(t14.1.coef))

rm(t14.1)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t14.1 <- felm(ALEASETP.1 ~ APRACE, data = testers)

t14.1.coef <- as.data.frame(coef(summary(t14.1)))
names(t14.1.coef)[which(names(t14.1.coef) == "Estimate")] <- "Month-to-Month"
t14.1.coef <- as.data.frame(t(t14.1.coef))


# 2 = Lease 

t14.2 <- felm(ALEASETP.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t14.2.coef <- as.data.frame(coef(summary(t14.2)))
names(t14.2.coef)[which(names(t14.2.coef) == "Estimate")] <- "Lease"
t14.2.coef <- as.data.frame(t(t14.2.coef))

rm(t14.2)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t14.2 <- felm(ALEASETP.2 ~ APRACE, data = testers)

t14.2.coef <- as.data.frame(coef(summary(t14.2)))
names(t14.2.coef)[which(names(t14.2.coef) == "Estimate")] <- "Lease"
t14.2.coef <- as.data.frame(t(t14.2.coef))


# Generate table

# Combine into one dataframe

t14.coef <- rbind(t14.1.coef, t14.2.coef)

# Drop race category 'Other'

t14.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t14.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t14.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "lease.tex"))


# without CONTROL

stargazer(t14.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Car Owner ------------------------------------------------------------------------------------

# Tester owns a car?
# 1 = Yes 
# 0 = No

testers$ACAROWN <- as.factor(testers$ACAROWN)

t15 <- felm(ACAROWN ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t15.coef <- as.data.frame(coef(summary(t15)))
names(t15.coef)[which(names(t15.coef) == "Estimate")] <- "Percent Car Owner"
t15.coef <- as.data.frame(t(t15.coef))

rm(t15)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t15 <- felm(ACAROWN ~ APRACE, data = testers)

t15.coef <- as.data.frame(coef(summary(t15)))
names(t15.coef)[which(names(t15.coef) == "Estimate")] <- "Percent Car Owner"
t15.coef <- as.data.frame(t(t15.coef))

# Generate table

# Drop race category 'Other'

t15.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t15.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t15.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "carown.tex"))

# without CONTROL

stargazer(t15.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Monthly Rent ---------------------------------------------------------------------------------

t16 <- felm(ARENTNOW ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t16.coef <- as.data.frame(coef(summary(t16)))
names(t16.coef)[which(names(t16.coef) == "Estimate")] <- "Monthly Rent"
t16.coef <- as.data.frame(t(t16.coef))

rm(t16)

# without CONTROL to demonstrate the effect of tester characteristics without assigning tester pairs

t16 <- felm(ARENTNOW ~ APRACE, data = testers)

t16.coef <- as.data.frame(coef(summary(t16)))
names(t16.coef)[which(names(t16.coef) == "Estimate")] <- "Monthly Rent"
t16.coef <- as.data.frame(t(t16.coef))

# Generate table

# Drop race category 'Other'

t16.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t16.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

stargazer(t16.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "rent.tex"))

# without CONTROL

stargazer(t16.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Home and Neighborhood Characteristics --------------------------------------------------------

homes$APRACE <- as.factor(homes$APRACE)

# Listing Price --------------------------------------------------------------------------------

h1 <- felm(AdPrice ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h1.coef <- as.data.frame(coef(summary(h1)))
names(h1.coef)[which(names(h1.coef) == "Estimate")] <- "Listing Price"
h1.coef <- as.data.frame(t(h1.coef))

rm(h1)


# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h1 <- felm(AdPrice ~ APRACE, data = homes)

h1.coef <- as.data.frame(coef(summary(h1)))
names(h1.coef)[which(names(h1.coef) == "Estimate")] <- "Listing Price"
h1.coef <- as.data.frame(t(h1.coef))

# without CONTROL
# Exclude (Intercept)

h1.coef$`(Intercept)` <- NULL

# Drop race category 'Other'

h1.coef$APRACE5 <- NULL

# Generate table

# Convert into LaTeX using stargazer

stargazer(h1.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "adprice.tex"))


# without CONTROL

stargazer(h1.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))


# Home Type ------------------------------------------------------------------------------------

# What type of building is it? 

homes$HHMTYPE <- as.factor(homes$HHMTYPE)

htype.dummies <- as.data.frame(predict(dummyVars(~ HHMTYPE, data = homes), newdata = homes))
homes <- cbind(homes, htype.dummies)


# 1 = Single-family Detached

h2.1 <- felm(HHMTYPE.1 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h2.1.coef <- as.data.frame(coef(summary(h2.1)))
names(h2.1.coef)[which(names(h2.1.coef) == "Estimate")] <- "Single-family Detached"
h2.1.coef <- as.data.frame(t(h2.1.coef))

rm(h2.1)

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h2.1 <- felm(HHMTYPE.1 ~ APRACE, data = homes)

h2.1.coef <- as.data.frame(coef(summary(h2.1)))
names(h2.1.coef)[which(names(h2.1.coef) == "Estimate")] <- "Single-family Detached"
h2.1.coef <- as.data.frame(t(h2.1.coef))


# 2 = Duplex

h2.2 <- felm(HHMTYPE.2 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h2.2.coef <- as.data.frame(coef(summary(h2.2)))
names(h2.2.coef)[which(names(h2.2.coef) == "Estimate")] <- "Duplex"
h2.2.coef <- as.data.frame(t(h2.2.coef))

rm(h2.2)

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h2.2 <- felm(HHMTYPE.2 ~ APRACE, data = homes)

h2.2.coef <- as.data.frame(coef(summary(h2.2)))
names(h2.2.coef)[which(names(h2.2.coef) == "Estimate")] <- "Duplex"
h2.2.coef <- as.data.frame(t(h2.2.coef))


# 3 = Rowhouse or Townhouse

h2.3 <- felm(HHMTYPE.3 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h2.3.coef <- as.data.frame(coef(summary(h2.3)))
names(h2.3.coef)[which(names(h2.3.coef) == "Estimate")] <- "Rowhouse or Townhouse"
h2.3.coef <- as.data.frame(t(h2.3.coef))

rm(h2.3)

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h2.3 <- felm(HHMTYPE.3 ~ APRACE, data = homes)

h2.3.coef <- as.data.frame(coef(summary(h2.3)))
names(h2.3.coef)[which(names(h2.3.coef) == "Estimate")] <- "Rowhouse or Townhouse"
h2.3.coef <- as.data.frame(t(h2.3.coef))

# 4 = Multi-family Structure

h2.4 <- felm(HHMTYPE.4 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h2.4.coef <- as.data.frame(coef(summary(h2.4)))
names(h2.4.coef)[which(names(h2.4.coef) == "Estimate")] <- "Multi-family Structure"
h2.4.coef <- as.data.frame(t(h2.4.coef))

rm(h2.4)

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h2.4 <- felm(HHMTYPE.4 ~ APRACE, data = homes)

h2.4.coef <- as.data.frame(coef(summary(h2.4)))
names(h2.4.coef)[which(names(h2.4.coef) == "Estimate")] <- "Multi-family Structure"
h2.4.coef <- as.data.frame(t(h2.4.coef))


# 5 = Mobile Home 

h2.5 <- felm(HHMTYPE.5 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h2.5.coef <- as.data.frame(coef(summary(h2.5)))
names(h2.5.coef)[which(names(h2.5.coef) == "Estimate")] <- "Mobile Home"
h2.5.coef <- as.data.frame(t(h2.5.coef))

rm(h2.5)

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h2.5 <- felm(HHMTYPE.5 ~ APRACE, data = homes)

h2.5.coef <- as.data.frame(coef(summary(h2.5)))
names(h2.5.coef)[which(names(h2.5.coef) == "Estimate")] <- "Mobile Home"
h2.5.coef <- as.data.frame(t(h2.5.coef))


# Generate table

# Combine into one data frame 

h2.coef <- rbind(h2.1.coef,
                 h2.2.coef,
                 h2.3.coef,
                 h2.4.coef,
                 h2.5.coef)

# Drop race category 'Other'

h2.coef$APRACE5 <- NULL

# without CONTROL
# Exclude (Intercept)

h2.coef$`(Intercept)` <- NULL


# Convert into LaTeX using stargazer

stargazer(h2.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "htype.tex"))

# without CONTROL

stargazer(h2.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))



# Time of Test (AM) ----------------------------------------------------------------------------

homes$SAPPTAM <- as.factor(homes$SAPPTAM)

appt.dummies <- as.data.frame(predict(dummyVars(~ SAPPTAM, data = homes), newdata = homes))
homes <- cbind(homes, appt.dummies)


h3 <- felm(SAPPTAM.1 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h3.coef <- as.data.frame(coef(summary(h3)))
names(h3.coef)[which(names(h3.coef) == "Estimate")] <- "Time of Test (AM)"
h3.coef <- as.data.frame(t(h3.coef))

rm(h3)

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h3 <- felm(SAPPTAM.1 ~ APRACE, data = homes)

h3.coef <- as.data.frame(coef(summary(h3)))
names(h3.coef)[which(names(h3.coef) == "Estimate")] <- "Time of Test (AM)"
h3.coef <- as.data.frame(t(h3.coef))


# Generate table

# Drop race category 'Other'

h3.coef$APRACE5 <- NULL

# without CONTROL
# Exclude (Intercept)

h3.coef$`(Intercept)` <- NULL


# Convert into LaTeX using stargazer

stargazer(h3.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "SAPPTAM.tex"))

# without CONTROL

stargazer(h3.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"))




# Pollution Measurements in Advertised Homes ---------------------------------------------------

# Superfund Sites 

p1 <- felm(SFcount_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

p1.coef <- as.data.frame(coef(summary(p1)))
names(p1.coef)[which(names(p1.coef) == "Estimate")] <- "Superfund Sites"
p1.coef <- as.data.frame(t(p1.coef))

rm(p1)

# Respiratory Hazard Index

p2 <- felm(RespiratoryHazardIndex_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

p2.coef <- as.data.frame(coef(summary(p2)))
names(p2.coef)[which(names(p2.coef) == "Estimate")] <- "Respiratory Hazard Index"
p2.coef <- as.data.frame(t(p2.coef))

rm(p2)

# Risk-Screening Environmental Indicators

p3 <- felm(RSEI_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

p3.coef <- as.data.frame(coef(summary(p3)))
names(p3.coef)[which(names(p3.coef) == "Estimate")] <- "Risk-Screening Env. Indicator"
p3.coef <- as.data.frame(t(p3.coef))

rm(p3)


# Generate table -------------------------------------------------------------------------------

p.coef <- rbind(p1.coef,
                p2.coef,
                p3.coef)

# Drop race category 'Other'

p.coef$APRACE5 <- NULL


# Convert into LaTeX using stargazer

stargazer(p.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "pollution.tex"))


# Save output

saveRDS(p.coef, paste0(out, "balance-pollution.rds"))

# Census Block Group Characteristics -----------------------------------------------------------

# Percent White

cbg1 <- felm(w2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg1.coef <- as.data.frame(coef(summary(cbg1)))
names(cbg1.coef)[which(names(cbg1.coef) == "Estimate")] <- "Percent White"
cbg1.coef <- as.data.frame(t(cbg1.coef))

rm(cbg1)

# Percent African American

cbg2 <- felm(b2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg2.coef <- as.data.frame(coef(summary(cbg2)))
names(cbg2.coef)[which(names(cbg2.coef) == "Estimate")] <- "Percent African American"
cbg2.coef <- as.data.frame(t(cbg2.coef))

rm(cbg2)


# Percent Asian

cbg3 <- felm(a2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg3.coef <- as.data.frame(coef(summary(cbg3)))
names(cbg3.coef)[which(names(cbg3.coef) == "Estimate")] <- "Percent Asian"
cbg3.coef <- as.data.frame(t(cbg3.coef))

rm(cbg3)


# Percent Hispanic

cbg4 <- felm(hisp2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg4.coef <- as.data.frame(coef(summary(cbg4)))
names(cbg4.coef)[which(names(cbg4.coef) == "Estimate")] <- "Percent Hispanic"
cbg4.coef <- as.data.frame(t(cbg4.coef))

rm(cbg4)


# Poverty Rate

cbg5 <- felm(povrate_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg5.coef <- as.data.frame(coef(summary(cbg5)))
names(cbg5.coef)[which(names(cbg5.coef) == "Estimate")] <- "Poverty Rate"
cbg5.coef <- as.data.frame(t(cbg5.coef))

rm(cbg5)


# Percent College Graduates

cbg6 <- felm(college_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg6.coef <- as.data.frame(coef(summary(cbg6)))
names(cbg6.coef)[which(names(cbg6.coef) == "Estimate")] <- "Percent College Educated"
cbg6.coef <- as.data.frame(t(cbg6.coef))

rm(cbg6)


# Percent High Skill

cbg7 <- felm(skill_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg7.coef <- as.data.frame(coef(summary(cbg7)))
names(cbg7.coef)[which(names(cbg7.coef) == "Estimate")] <- "Percent High Skill"
cbg7.coef <- as.data.frame(t(cbg7.coef))

rm(cbg7)

# Assaults

cbg8 <- felm(Assault_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg8.coef <- as.data.frame(coef(summary(cbg8)))
names(cbg8.coef)[which(names(cbg8.coef) == "Estimate")] <- "Assaults"
cbg8.coef <- as.data.frame(t(cbg8.coef))

rm(cbg8)

# Elementary School Score

cbg9 <- felm(Elementary_School_Score_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg9.coef <- as.data.frame(coef(summary(cbg9)))
names(cbg9.coef)[which(names(cbg9.coef) == "Estimate")] <- "Assaults"
cbg9.coef <- as.data.frame(t(cbg9.coef))

rm(cbg9)

# Generate table -------------------------------------------------------------------------------

cbg.coef <- rbind(cbg1.coef,
                cbg2.coef,
                cbg3.coef,
                cbg4.coef,
                cbg5.coef,
                cbg6.coef,
                cbg7.coef,
                cbg8.coef,
                cbg9.coef)

# Drocbg race category 'Other'

cbg.coef$APRACE5 <- NULL


# Convert into LaTeX using stargazer

stargazer(cbg.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "census-block-group.tex"))


# Save output

saveRDS(cbg.coef, paste0(out, "balance-cbg.rds"))

# ----------------------------------------------------------------------------------------------

# Balance statistics advertised homes by race --------------------------------------------------

# ***This is using other-designated identified racial categories***

# minority: 
#   1 = White 
#   2 = African American/African-American 
#   3 = Hispanic 
#   4 = Asian/Pacific Islander 
#   5 = Other (Specify)

# minority is the variable of interest

# Assignment Characteristics -------------------------------------------------------------------

# Test Sequence --------------------------------------------------------------------------------

seq.dummies <- as.data.frame(predict(dummyVars(~ SEQUENCE.y, data = ads), newdata = ads))
ads <- cbind(ads, seq.dummies)


t1 <- felm(SEQUENCE.y.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t1.coef <- as.data.frame(coef(summary(t1)))
names(t1.coef)[which(names(t1.coef) == "Estimate")] <- "Test Sequence (First)"
t1.coef <- as.data.frame(t(t1.coef))

rm(t1)

# Generate table

# Drop race category 'Other'

t1.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t1.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "test-sequence (min).tex"))



# Month of Test --------------------------------------------------------------------------------

ads$month <- as.factor(ads$month)

month.dummies <- as.data.frame(predict(dummyVars(~ month, data = ads), newdata = ads))
ads <- cbind(ads, month.dummies)

# Jan

t2.1 <- felm(month.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.1.coef <- as.data.frame(coef(summary(t2.1)))
names(t2.1.coef)[which(names(t2.1.coef) == "Estimate")] <- "Jan"
t2.1.coef <- as.data.frame(t(t2.1.coef))

rm(t2.1)

# Feb

t2.2 <- felm(month.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.2.coef <- as.data.frame(coef(summary(t2.2)))
names(t2.2.coef)[which(names(t2.2.coef) == "Estimate")] <- "Feb"
t2.2.coef <- as.data.frame(t(t2.2.coef))

rm(t2.2)

# Mar

t2.3 <- felm(month.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.3.coef <- as.data.frame(coef(summary(t2.3)))
names(t2.3.coef)[which(names(t2.3.coef) == "Estimate")] <- "Mar"
t2.3.coef <- as.data.frame(t(t2.3.coef))

rm(t2.3)

# Apr

t2.4 <- felm(month.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.4.coef <- as.data.frame(coef(summary(t2.4)))
names(t2.4.coef)[which(names(t2.4.coef) == "Estimate")] <- "Apr"
t2.4.coef <- as.data.frame(t(t2.4.coef))

rm(t2.4)

# May

t2.5 <- felm(month.5 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.5.coef <- as.data.frame(coef(summary(t2.5)))
names(t2.5.coef)[which(names(t2.5.coef) == "Estimate")] <- "May"
t2.5.coef <- as.data.frame(t(t2.5.coef))

rm(t2.5)

# Jun

t2.6 <- felm(month.6 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.6.coef <- as.data.frame(coef(summary(t2.6)))
names(t2.6.coef)[which(names(t2.6.coef) == "Estimate")] <- "Jun"
t2.6.coef <- as.data.frame(t(t2.6.coef))

rm(t2.6)

# Jul

t2.7 <- felm(month.7 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.7.coef <- as.data.frame(coef(summary(t2.7)))
names(t2.7.coef)[which(names(t2.7.coef) == "Estimate")] <- "Jul"
t2.7.coef <- as.data.frame(t(t2.7.coef))

rm(t2.7)

# Aug

t2.8 <- felm(month.8 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.8.coef <- as.data.frame(coef(summary(t2.8)))
names(t2.8.coef)[which(names(t2.8.coef) == "Estimate")] <- "Aug"
t2.8.coef <- as.data.frame(t(t2.8.coef))

rm(t2.8)

# Sept

t2.9 <- felm(month.9 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.9.coef <- as.data.frame(coef(summary(t2.9)))
names(t2.9.coef)[which(names(t2.9.coef) == "Estimate")] <- "Sept"
t2.9.coef <- as.data.frame(t(t2.9.coef))

rm(t2.9)

# Oct

t2.10 <- felm(month.10 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.10.coef <- as.data.frame(coef(summary(t2.10)))
names(t2.10.coef)[which(names(t2.10.coef) == "Estimate")] <- "Oct"
t2.10.coef <- as.data.frame(t(t2.10.coef))

rm(t2.10)

# Nov 

t2.11 <- felm(month.11 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.11.coef <- as.data.frame(coef(summary(t2.11)))
names(t2.11.coef)[which(names(t2.11.coef) == "Estimate")] <- "Nov"
t2.11.coef <- as.data.frame(t(t2.11.coef))

rm(t2.11)

# Dec

t2.12 <- felm(month.12 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t2.12.coef <- as.data.frame(coef(summary(t2.12)))
names(t2.12.coef)[which(names(t2.12.coef) == "Estimate")] <- "Dec"
t2.12.coef <- as.data.frame(t(t2.12.coef))

rm(t2.12)

# Generate table

# Combine into one data frame

t2.coef <- rbind(t2.1.coef,
                 t2.2.coef,
                 t2.3.coef,
                 t2.4.coef,
                 t2.5.coef,
                 t2.6.coef,
                 t2.7.coef,
                 t2.8.coef,
                 t2.9.coef,
                 t2.10.coef,
                 t2.11.coef,
                 t2.12.coef)

# Drop race category 'Other'

t2.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t2.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "month (min).tex"))

# Time of Test (AM) ----------------------------------------------------------------------------

appt.dummies <- as.data.frame(predict(dummyVars(~ SAPPTAM, data = ads), newdata = ads))
ads <- cbind(ads, appt.dummies)


t3 <- felm(SAPPTAM.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t3.coef <- as.data.frame(coef(summary(t3)))
names(t3.coef)[which(names(t3.coef) == "Estimate")] <- "Time of Test (AM)"
t3.coef <- as.data.frame(t(t3.coef))

rm(t3)

# Generate table

# Drop race category 'Other'

t3.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t3.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "SAPPTAM (min).tex"))


# Reason for Moving ----------------------------------------------------------------------------

reason.dummies <- as.data.frame(predict(dummyVars(~ AMOVERS, data = ads), newdata = ads))
ads <- cbind(ads, reason.dummies)

# 1 = Ready For Change 

t4.1 <- felm(AMOVERS.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.1.coef <- as.data.frame(coef(summary(t4.1)))
names(t4.1.coef)[which(names(t4.1.coef) == "Estimate")] <- "Ready for change"
t4.1.coef <- as.data.frame(t(t4.1.coef))

rm(t4.1)

# 2 = Good Time To Buy

t4.2 <- felm(AMOVERS.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.2.coef <- as.data.frame(coef(summary(t4.2)))
names(t4.2.coef)[which(names(t4.2.coef) == "Estimate")] <- "Good time to buy"
t4.2.coef <- as.data.frame(t(t4.2.coef))

rm(t4.2)

# 3 = Renting for Long Time 

t4.3 <- felm(AMOVERS.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.3.coef <- as.data.frame(coef(summary(t4.3)))
names(t4.3.coef)[which(names(t4.3.coef) == "Estimate")] <- "Renting for long time"
t4.3.coef <- as.data.frame(t(t4.3.coef))

rm(t4.3)


# 4 = Tax Benefits of Home Ownership

t4.4 <- felm(AMOVERS.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.4.coef <- as.data.frame(coef(summary(t4.4)))
names(t4.4.coef)[which(names(t4.4.coef) == "Estimate")] <- "Tax benefits"
t4.4.coef <- as.data.frame(t(t4.4.coef))

rm(t4.4)


# 5 = Now Able To Take Time To Own A Home

t4.5 <- felm(AMOVERS.5 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.5.coef <- as.data.frame(coef(summary(t4.5)))
names(t4.5.coef)[which(names(t4.5.coef) == "Estimate")] <- "Now have the time"
t4.5.coef <- as.data.frame(t(t4.5.coef))

rm(t4.5)


# 6 = Freedom to Decorate My Own Home

t4.6 <- felm(AMOVERS.6 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.6.coef <- as.data.frame(coef(summary(t4.6)))
names(t4.6.coef)[which(names(t4.6.coef) == "Estimate")] <- "Freedom to decorate"
t4.6.coef <- as.data.frame(t(t4.6.coef))

rm(t4.6)


# 7 = Buying First Home

t4.7 <- felm(AMOVERS.7 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.7.coef <- as.data.frame(coef(summary(t4.7)))
names(t4.7.coef)[which(names(t4.7.coef) == "Estimate")] <- "Buying first home"
t4.7.coef <- as.data.frame(t(t4.7.coef))

rm(t4.7)


# 8 = Ready for Responsibility of Home Ownership

t4.8 <- felm(AMOVERS.8 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.8.coef <- as.data.frame(coef(summary(t4.8)))
names(t4.8.coef)[which(names(t4.8.coef) == "Estimate")] <- "Ready for responsibility"
t4.8.coef <- as.data.frame(t(t4.8.coef))

rm(t4.8)


# 9 = Recently Relocated

t4.9 <- felm(AMOVERS.9 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t4.9.coef <- as.data.frame(coef(summary(t4.9)))
names(t4.9.coef)[which(names(t4.9.coef) == "Estimate")] <- "Recently relocated"
t4.9.coef <- as.data.frame(t(t4.9.coef))

rm(t4.9)

# Generate table

# Combine into one data frame

t4.coef <- rbind(t4.1.coef,
                 t4.2.coef,
                 t4.3.coef,
                 t4.4.coef,
                 t4.5.coef,
                 t4.6.coef,
                 t4.7.coef,
                 t4.8.coef,
                 t4.9.coef)

# Drop race category 'Other'

t4.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t4.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "reason (min).tex"))


# Gender ---------------------------------------------------------------------------------------

ads$TSEX.x <- as.factor(ads$TSEX.x)

gender.dummies <- as.data.frame(predict(dummyVars(~ TSEX.x, data = ads), newdata = ads))
ads <- cbind(ads, gender.dummies)

t5 <- felm(TSEX.x.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t5.coef <- as.data.frame(coef(summary(t5)))
names(t5.coef)[which(names(t5.coef) == "Estimate")] <- "Gender (Male)"
t5.coef <- as.data.frame(t(t5.coef))

rm(t5)

# Generate table

# Drop race category 'Other'

t5.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t5.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "gender (min).tex"))


# Age ------------------------------------------------------------------------------------------

t6 <- felm(age ~ minority | CONTROL | 0 | CONTROL, data = ads)

t6.coef <- as.data.frame(coef(summary(t6)))
names(t6.coef)[which(names(t6.coef) == "Estimate")] <- "Age"
t6.coef <- as.data.frame(t(t6.coef))

rm(t6)

# Generate table

# Drop race category 'Other'

t6.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t6.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "age (min).tex"))


# Income ---------------------------------------------------------------------------------------

# Run model separately for each income bin

inc.dummies <- as.data.frame(predict(dummyVars(~ TPEGAI, data = ads), newdata = ads))
ads <- cbind(ads, inc.dummies)

# 1 = Under $10,000

t7.1 <- felm(TPEGAI.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.1.coef <- as.data.frame(coef(summary(t7.1)))
names(t7.1.coef)[which(names(t7.1.coef) == "Estimate")] <- "Under $10,000"
t7.1.coef <- as.data.frame(t(t7.1.coef))

rm(t7.1)

# 2 = $10,000 - $19,999

t7.2 <- felm(TPEGAI.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.2.coef <- as.data.frame(coef(summary(t7.2)))
names(t7.2.coef)[which(names(t7.2.coef) == "Estimate")] <- "$10,000 - $19,999"
t7.2.coef <- as.data.frame(t(t7.2.coef))

rm(t7.2)

# 3 = $20,000 - $29,999

t7.3 <- felm(TPEGAI.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.3.coef <- as.data.frame(coef(summary(t7.3)))
names(t7.3.coef)[which(names(t7.3.coef) == "Estimate")] <- "$20,000 - $29,999"
t7.3.coef <- as.data.frame(t(t7.3.coef))

rm(t7.3)

# 4 = $30,000 - $39,999

t7.4 <- felm(TPEGAI.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.4.coef <- as.data.frame(coef(summary(t7.4)))
names(t7.4.coef)[which(names(t7.4.coef) == "Estimate")] <- "$30,000 - $39,999"
t7.4.coef <- as.data.frame(t(t7.4.coef))

rm(t7.4)

# 5 = $40,000 - $49,999

t7.5 <- felm(TPEGAI.5 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.5.coef <- as.data.frame(coef(summary(t7.5)))
names(t7.5.coef)[which(names(t7.5.coef) == "Estimate")] <- "$40,000 - $49,999"
t7.5.coef <- as.data.frame(t(t7.5.coef))

rm(t7.5)

# 6 = $50,000 - $74,999

t7.6 <- felm(TPEGAI.6 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.6.coef <- as.data.frame(coef(summary(t7.6)))
names(t7.6.coef)[which(names(t7.6.coef) == "Estimate")] <- "$50,000 - $74,999"
t7.6.coef <- as.data.frame(t(t7.6.coef))

rm(t7.6)

# 7 = $75,000 - 99,999

t7.7 <- felm(TPEGAI.7 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.7.coef <- as.data.frame(coef(summary(t7.7)))
names(t7.7.coef)[which(names(t7.7.coef) == "Estimate")] <- "$75,000 - $99,999"
t7.7.coef <- as.data.frame(t(t7.7.coef))

rm(t7.7)

# 8 = $100,000 or more

t7.8 <- felm(TPEGAI.8 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t7.8.coef <- as.data.frame(coef(summary(t7.8)))
names(t7.8.coef)[which(names(t7.8.coef) == "Estimate")] <- "$100,000 or more"
t7.8.coef <- as.data.frame(t(t7.8.coef))

rm(t7.8)

# Generate table

# Combine into one data frame

t7.coef <- rbind(t7.1.coef,
                 t7.2.coef,
                 t7.3.coef,
                 t7.4.coef,
                 t7.5.coef,
                 t7.6.coef,
                 t7.7.coef,
                 t7.8.coef)

# Drop race category 'Other'

t7.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t7.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "income (min).tex"))


# Household Income -----------------------------------------------------------------------------

hhinc.dummies <- as.data.frame(predict(dummyVars(~ THHEGAI, data = ads), newdata = ads))
ads <- cbind(ads, hhinc.dummies)


# 1 = Under $10,000

t8.1 <- felm(THHEGAI.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.1.coef <- as.data.frame(coef(summary(t8.1)))
names(t8.1.coef)[which(names(t8.1.coef) == "Estimate")] <- "Under $10,000"
t8.1.coef <- as.data.frame(t(t8.1.coef))

rm(t8.1)

# 2 = $10,000 - $19,999

t8.2 <- felm(THHEGAI.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.2.coef <- as.data.frame(coef(summary(t8.2)))
names(t8.2.coef)[which(names(t8.2.coef) == "Estimate")] <- "$10,000 - $19,999"
t8.2.coef <- as.data.frame(t(t8.2.coef))

rm(t8.2)

# 3 = $20,000 - $29,999

t8.3 <- felm(THHEGAI.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.3.coef <- as.data.frame(coef(summary(t8.3)))
names(t8.3.coef)[which(names(t8.3.coef) == "Estimate")] <- "$20,000 - $29,999"
t8.3.coef <- as.data.frame(t(t8.3.coef))

rm(t8.3)


# 4 = $30,000 - $39,999

t8.4 <- felm(THHEGAI.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.4.coef <- as.data.frame(coef(summary(t8.4)))
names(t8.4.coef)[which(names(t8.4.coef) == "Estimate")] <- "$30,000 - $39,999"
t8.4.coef <- as.data.frame(t(t8.4.coef))

rm(t8.4)


# 5 = $40,000 - $49,999

t8.5 <- felm(THHEGAI.5 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.5.coef <- as.data.frame(coef(summary(t8.5)))
names(t8.5.coef)[which(names(t8.5.coef) == "Estimate")] <- "$40,000 - $49,999"
t8.5.coef <- as.data.frame(t(t8.5.coef))

rm(t8.5)


# 6 = $50,000 - $59,999

t8.6 <- felm(THHEGAI.6 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.6.coef <- as.data.frame(coef(summary(t8.6)))
names(t8.6.coef)[which(names(t8.6.coef) == "Estimate")] <- "$50,000 - $59,999"
t8.6.coef <- as.data.frame(t(t8.6.coef))

rm(t8.6)


# 7 = $60,000 - $69,999

t8.7 <- felm(THHEGAI.7 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.7.coef <- as.data.frame(coef(summary(t8.7)))
names(t8.7.coef)[which(names(t8.7.coef) == "Estimate")] <- "$60,000 - $69,999"
t8.7.coef <- as.data.frame(t(t8.7.coef))

rm(t8.7)


# 8 = $70,000 - $79,999

t8.8 <- felm(THHEGAI.8 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.8.coef <- as.data.frame(coef(summary(t8.8)))
names(t8.8.coef)[which(names(t8.8.coef) == "Estimate")] <- "$70,000 - $79,999"
t8.8.coef <- as.data.frame(t(t8.8.coef))

rm(t8.8)


# 9 = $80,000 - $99,999

t8.9 <- felm(THHEGAI.9 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.9.coef <- as.data.frame(coef(summary(t8.9)))
names(t8.9.coef)[which(names(t8.9.coef) == "Estimate")] <- "$80,000 - $99,999"
t8.9.coef <- as.data.frame(t(t8.9.coef))

rm(t8.9)


# 10 = $100,000 - $149,999

t8.10 <- felm(THHEGAI.10 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.10.coef <- as.data.frame(coef(summary(t8.10)))
names(t8.10.coef)[which(names(t8.10.coef) == "Estimate")] <- "$100,000 - $149,999"
t8.10.coef <- as.data.frame(t(t8.10.coef))

rm(t8.10)


# 11 = $150,000 or more

t8.11 <- felm(THHEGAI.11 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t8.11.coef <- as.data.frame(coef(summary(t8.11)))
names(t8.11.coef)[which(names(t8.11.coef) == "Estimate")] <- "$150,000 or more"
t8.11.coef <- as.data.frame(t(t8.11.coef))

rm(t8.11)

# Generate table

# Combine into one data frame

t8.coef <- rbind(t8.1.coef,
                 t8.2.coef,
                 t8.3.coef,
                 t8.4.coef,
                 t8.5.coef,
                 t8.6.coef,
                 t8.7.coef,
                 t8.8.coef,
                 t8.9.coef,
                 t8.10.coef,
                 t8.11.coef)


# Drop race category 'Other'

t8.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t8.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "hh-income (min).tex"))



# Education ------------------------------------------------------------------------------------

edu.dummies <- as.data.frame(predict(dummyVars(~ THIGHEDU, data = ads), newdata = ads))
ads <- cbind(ads, edu.dummies)

# 1 = Grade School Or Less

t9.1 <- felm(THIGHEDU.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.1.coef <- as.data.frame(coef(summary(t9.1)))
names(t9.1.coef)[which(names(t9.1.coef) == "Estimate")] <- "Grade School Or Less"
t9.1.coef <- as.data.frame(t(t9.1.coef))

rm(t9.1)

# 2 = Attended High School

t9.2 <- felm(THIGHEDU.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.2.coef <- as.data.frame(coef(summary(t9.2)))
names(t9.2.coef)[which(names(t9.2.coef) == "Estimate")] <- "Attended High School"
t9.2.coef <- as.data.frame(t(t9.2.coef))

rm(t9.2)

# 3 = GED

t9.3 <- felm(THIGHEDU.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.3.coef <- as.data.frame(coef(summary(t9.3)))
names(t9.3.coef)[which(names(t9.3.coef) == "Estimate")] <- "GED"
t9.3.coef <- as.data.frame(t(t9.3.coef))

rm(t9.3)

# 4 = High School Diploma

t9.4 <- felm(THIGHEDU.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.4.coef <- as.data.frame(coef(summary(t9.4)))
names(t9.4.coef)[which(names(t9.4.coef) == "Estimate")] <- "High School Diploma"
t9.4.coef <- as.data.frame(t(t9.4.coef))

rm(t9.4)

# 5 = Attended Vocational / Technical School

t9.5 <- felm(THIGHEDU.5 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.5.coef <- as.data.frame(coef(summary(t9.5)))
names(t9.5.coef)[which(names(t9.5.coef) == "Estimate")] <- "Attended Vocational / Technical School"
t9.5.coef <- as.data.frame(t(t9.5.coef))

rm(t9.5)

# 6 = Vocational / Technical School Diploma

t9.6 <- felm(THIGHEDU.6 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.6.coef <- as.data.frame(coef(summary(t9.6)))
names(t9.6.coef)[which(names(t9.6.coef) == "Estimate")] <- "Vocational / Technical School Diploma"
t9.6.coef <- as.data.frame(t(t9.6.coef))

rm(t9.6)

# 7 = Attended College

t9.7 <- felm(THIGHEDU.7 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.7.coef <- as.data.frame(coef(summary(t9.7)))
names(t9.7.coef)[which(names(t9.7.coef) == "Estimate")] <- "Attended College"
t9.7.coef <- as.data.frame(t(t9.7.coef))

rm(t9.7)

# 8 = Associate's Degree

t9.8 <- felm(THIGHEDU.8 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.8.coef <- as.data.frame(coef(summary(t9.8)))
names(t9.8.coef)[which(names(t9.8.coef) == "Estimate")] <- "Associate's Degree"
t9.8.coef <- as.data.frame(t(t9.8.coef))

rm(t9.8)

# 9 = Bachelor's Degree 

t9.9 <- felm(THIGHEDU.9.9 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.9.coef <- as.data.frame(coef(summary(t9.9)))
names(t9.9.coef)[which(names(t9.9.coef) == "Estimate")] <- "Bachelor's Degree"
t9.9.coef <- as.data.frame(t(t9.9.coef))

rm(t9.9)

# 10 = Attended Graduate / Professional School

t9.10 <- felm(THIGHEDU.10 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.10.coef <- as.data.frame(coef(summary(t9.10)))
names(t9.10.coef)[which(names(t9.10.coef) == "Estimate")] <- "Attended Graduate / Professional School"
t9.10.coef <- as.data.frame(t(t9.10.coef))

rm(t9.10)

# 11 = Graduate / Professional Degree

t9.11 <- felm(THIGHEDU.11 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t9.11.coef <- as.data.frame(coef(summary(t9.11)))
names(t9.11.coef)[which(names(t9.11.coef) == "Estimate")] <- "Graduate / Professional Degree"
t9.11.coef <- as.data.frame(t(t9.11.coef))

rm(t9.11)

# Generate table

# Combine into one data frame

t9.coef <- rbind(t9.1.coef,
                 t9.2.coef,
                 t9.3.coef,
                 t9.4.coef,
                 t9.5.coef,
                 t9.6.coef,
                 t9.7.coef,
                 t9.8.coef,
                 t9.9.coef,
                 t9.10.coef,
                 t9.11.coef)

# Drop race category 'Other'

t9.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t9.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "edu (min).tex"))


# Homeowner ------------------------------------------------------------------------------------

# Do you presently rent or own your home? 
# 1 = rent
# 2 = own

own.dummies <- as.data.frame(predict(dummyVars(~ TCURTENR, data = ads), newdata = ads))
ads <- cbind(ads, own.dummies)

t10 <- felm(TCURTENR.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t10.coef <- as.data.frame(coef(summary(t10)))
names(t10.coef)[which(names(t10.coef) == "Estimate")] <- "Homeowner"
t10.coef <- as.data.frame(t(t10.coef))

rm(t10)

# Generate table

# Drop race category 'Other'

t10.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t10.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "homeowner (min).tex"))


# Length of Employment -------------------------------------------------------------------------

t11 <- felm(AELNG1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t11.coef <- as.data.frame(coef(summary(t11)))
names(t11.coef)[which(names(t11.coef) == "Estimate")] <- "Length of Employment (Years)"
t11.coef <- as.data.frame(t(t11.coef))

rm(t11)

# Generate table

# Drop race category 'Other'

t11.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t11.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "length_emp (min).tex"))


# Down Payment Reason --------------------------------------------------------------------------

dpreason.dummies <- as.data.frame(predict(dummyVars(~ DPMTEXP, data = ads), newdata = ads))
ads <- cbind(ads, dpreason.dummies)

# 1 = I've been saving for quite a while

t12.1 <- felm(DPMTEXP.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t12.1.coef <- as.data.frame(coef(summary(t12.1)))
names(t12.1.coef)[which(names(t12.1.coef) == "Estimate")] <- "Personal Savings"
t12.1.coef <- as.data.frame(t(t12.1.coef))

rm(t12.1)


# 2 = My / our parents are helping me/us

t12.2 <- felm(DPMTEXP.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t12.2.coef <- as.data.frame(coef(summary(t12.2)))
names(t12.2.coef)[which(names(t12.2.coef) == "Estimate")] <- "Help From Parents"
t12.2.coef <- as.data.frame(t(t12.2.coef))

rm(t12.2)


# 3 = I inherited money from a relative

t12.3 <- felm(DPMTEXP.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t12.3.coef <- as.data.frame(coef(summary(t12.3)))
names(t12.3.coef)[which(names(t12.3.coef) == "Estimate")] <- "Inherited Money"
t12.3.coef <- as.data.frame(t(t12.3.coef))

rm(t12.3)


# 4 = I/we had equity in a previously owned home 

t12.4 <- felm(DPMTEXP.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t12.4.coef <- as.data.frame(coef(summary(t12.4)))
names(t12.4.coef)[which(names(t12.4.coef) == "Estimate")] <- "Equity From Previous Home"
t12.4.coef <- as.data.frame(t(t12.4.coef))

rm(t12.4)

# Generate table

# Combine into one data frame

t12.coef <- rbind(t12.1.coef,
                  t12.2.coef,
                  t12.3.coef,
                  t12.4.coef)

# Drop race category 'Other'

t12.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t12.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "dp_reason (min).tex"))



# Years at Residence ---------------------------------------------------------------------------

t13 <- felm(ALGNCUR ~ minority | CONTROL | 0 | CONTROL, data = ads)

t13.coef <- as.data.frame(coef(summary(t13)))
names(t13.coef)[which(names(t13.coef) == "Estimate")] <- "Years at Residence"
t13.coef <- as.data.frame(t(t13.coef))

rm(t13)

# Generate table

# Drop race category 'Other'

t13.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t13.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "length_res (min).tex"))



# Lease Type -----------------------------------------------------------------------------------

leasetp.dummies <- as.data.frame(predict(dummyVars(~ ALEASETP, data = ads), newdata = ads))
ads <- cbind(ads, leasetp.dummies)

# 1 = Month - to - Month
# 2 = Lease 

t14 <- felm(ALEASETP.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

t14.coef <- as.data.frame(coef(summary(t14)))
names(t14.coef)[which(names(t14.coef) == "Estimate")] <- "Lease Type"
t14.coef <- as.data.frame(t(t14.coef))

rm(t14)

# Generate table

# Drop race category 'Other'

t14.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t14.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "lease (min).tex"))



# Car Owner ------------------------------------------------------------------------------------

# Tester owns a car?
# 1 = Yes 
# 0 = No

t15 <- felm(ACAROWN ~ minority | CONTROL | 0 | CONTROL, data = ads)

t15.coef <- as.data.frame(coef(summary(t15)))
names(t15.coef)[which(names(t15.coef) == "Estimate")] <- "Car Owner"
t15.coef <- as.data.frame(t(t15.coef))

rm(t15)

# Generate table

# Drop race category 'Other'

t15.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(t15.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "carown (min).tex"))





# Home and Neighborhood Characteristics --------------------------------------------------------

# Listing Price --------------------------------------------------------------------------------

h1 <- felm(AdPrice ~ minority | CONTROL | 0 | CONTROL, data = ads)

h1.coef <- as.data.frame(coef(summary(h1)))
names(h1.coef)[which(names(h1.coef) == "Estimate")] <- "Listing Price"
h1.coef <- as.data.frame(t(h1.coef))

rm(h1)

# Generate table

# Drop race category 'Other'

h1.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(h1.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "adprice (min).tex"))



# Home Type ------------------------------------------------------------------------------------

# What type of building is it? 

htype.dummies <- as.data.frame(predict(dummyVars(~ HHMTYPE, data = ads), newdata = ads))
ads <- cbind(ads, htype.dummies)


# 1 = Single-family Detached

h2.1 <- felm(HHMTYPE.1 ~ minority | CONTROL | 0 | CONTROL, data = ads)

h2.1.coef <- as.data.frame(coef(summary(h2.1)))
names(h2.1.coef)[which(names(h2.1.coef) == "Estimate")] <- "Single-family Detached"
h2.1.coef <- as.data.frame(t(h2.1.coef))

rm(h2.1)

# 2 = Duplex

h2.2 <- felm(HHMTYPE.2 ~ minority | CONTROL | 0 | CONTROL, data = ads)

h2.2.coef <- as.data.frame(coef(summary(h2.2)))
names(h2.2.coef)[which(names(h2.2.coef) == "Estimate")] <- "Duplex"
h2.2.coef <- as.data.frame(t(h2.2.coef))

rm(h2.2)

# 3 = Rowhouse or Townhouse

h2.3 <- felm(HHMTYPE.3 ~ minority | CONTROL | 0 | CONTROL, data = ads)

h2.3.coef <- as.data.frame(coef(summary(h2.3)))
names(h2.3.coef)[which(names(h2.3.coef) == "Estimate")] <- "Rowhouse or Townhouse"
h2.3.coef <- as.data.frame(t(h2.3.coef))

rm(h2.3)

# 4 = Multi-family Structure

h2.4 <- felm(HHMTYPE.4 ~ minority | CONTROL | 0 | CONTROL, data = ads)

h2.4.coef <- as.data.frame(coef(summary(h2.4)))
names(h2.4.coef)[which(names(h2.4.coef) == "Estimate")] <- "Multi-family Structure"
h2.4.coef <- as.data.frame(t(h2.4.coef))

rm(h2.4)

# 5 = Mobile Home 

h2.5 <- felm(HHMTYPE.5 ~ minority | CONTROL | 0 | CONTROL, data = ads)

h2.5.coef <- as.data.frame(coef(summary(h2.5)))
names(h2.5.coef)[which(names(h2.5.coef) == "Estimate")] <- "Mobile Home"
h2.5.coef <- as.data.frame(t(h2.5.coef))

rm(h2.5)

# Generate table

# Combine into one data frame 

h2.coef <- rbind(h2.1.coef,
                 h2.2.coef,
                 h2.3.coef,
                 h2.4.coef,
                 h2.5.coef)

# Drop race category 'Other'

h2.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(h2.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "htype (min).tex"))



# Minority Population --------------------------------------------------------------------------

h3 <- felm(Minority_Population_Ad ~ minority | TASIANG + THISPUBG + CONTROL | 0 | CONTROL, data = ads)

h3.coef <- as.data.frame(coef(summary(h3)))
names(h3.coef)[which(names(h3.coef) == "Estimate")] <- "Minority Population"
h3.coef <- as.data.frame(t(h3.coef))

rm(h3)

# Generate table

# Drop race category 'Other'

h3.coef$minority5 <- NULL

# Convert into LaTeX using stargazer

stargazer(h3.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "min_pop (min).tex"))



# Pollution Measurements in Advertised Homes ---------------------------------------------------

# Superfund Proximity 

p1 <- felm(Superfund_Proximity_Ad ~ minority | TASIANG + THISPUBG + CONTROL | 0 | CONTROL, data = ads)

p1.coef <- as.data.frame(coef(summary(p1)))
names(p1.coef)[which(names(p1.coef) == "Estimate")] <- "Superfund Proximity"
p1.coef <- as.data.frame(t(p1.coef))

rm(p1)

# Diesel PM 

p2 <- felm(Diesel_PM_Ad ~ minority | TASIANG + THISPUBG + CONTROL | 0 | CONTROL, data = ads)

p2.coef <- as.data.frame(coef(summary(p2)))
View(p2.coef)

p2.coef <- as.data.frame(coef(summary(p2))[,1:2])
names(p2.coef)[which(names(p2.coef) == "Estimate")] <- "Diesel PM"
p2.coef <- as.data.frame(t(p2.coef))

rm(p2)

# Air Toxics Cancer Risk

p3<- felm(Air_Toxics_Cancer_Risk_Ad ~ minority | TASIANG + THISPUBG + CONTROL | 0 | CONTROL, data = ads)

p3.coef <- as.data.frame(coef(summary(p3)))
View(p3.coef)

p3.coef <- as.data.frame(coef(summary(p3))[,1:2])
names(p3.coef)[which(names(p3.coef) == "Estimate")] <- "Air Toxics Cancer Risk"
p3.coef <- as.data.frame(t(p3.coef))

rm(p3)

# Respiratory Hazard Index

p4 <- felm(RespiratoryHazardIndex_Ad ~ minority | TASIANG + THISPUBG + CONTROL | 0 | CONTROL, data = ads)

p4.coef <- as.data.frame(coef(summary(p4)))

p4.coef <- as.data.frame(coef(summary(p4))[,1:2])
names(p4.coef)[which(names(p4.coef) == "Estimate")] <- "Respiratory Hazard Index"
p4.coef <- as.data.frame(t(p4.coef))

rm(p4)

# Generate table -------------------------------------------------------------------------------

p.coef <- rbind(p1.coef,
                p2.coef,
                p3.coef,
                p4.coef)

# Drop race category 'Other'

p.coef$minority5 <- NULL


# Convert into LaTeX using stargazer

stargazer(p.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "pollution (min).tex"))


# Save output

saveRDS(p.coef, paste0(out, "balance-pollution.rds"))
