#### Clear environment and console ####
rm(list = ls())
cat("\014")

#### Load all needed packages ####
library(tidyverse)
library(sas7bdat)

#### Import the BLSA data files from paper 1 ####
load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 1 Analyses/Data/blsa.RData")
rm(vo2max_sample)

#### Clean variables ####

#### UW150 = LDCW: Usual pace walk - completion flag ####
# This should be dichotomous
# There's only values for uw150meters where uw150 = 1, so I assume all other values indicate
# the reason the walk wasn't completed and can be recoded to 0
table(first_vo2_surv$uw150, exclude = NULL)
table(first_vo2_surv$uw150meters[first_vo2_surv$uw150 !=1], exclude = NULL)

first_vo2_surv$uw150[first_vo2_surv$uw150 != 1] <- 0
first_vo2_surv$uw150[is.na(first_vo2_surv$uw150meters)] <- 0

table(first_vo2_surv$uw150, exclude = NULL)
table(first_vo2_surv$uw150meters, first_vo2_surv$uw150, exclude = NULL)
# Now those with NA in uw150meters have 0 in uw150

#### 400 meter walk variables ####
table(first_vo2_surv$did400m, exclude = NULL) # Did 400 meter walk
table(first_vo2_surv$c400secs[first_vo2_surv$did400m != 1], exclude = NULL) # No data for anything other than
# did400m == 1

# Make anything that's not a 1 set to 0
first_vo2_surv$did400m[first_vo2_surv$did400m != 1] <- 0
first_vo2_surv$did400m[is.na(first_vo2_surv$did400m)] <- 0
table(first_vo2_surv$did400m, exclude = NULL)

# Now look at LDCW47B, "LDCW: LongCorrWalk - 400m walking aid"
table(first_vo2_surv$ldcw47b, exclude = NULL) # Lots of missingness, see if it correlates to did400m flag
table(first_vo2_surv$did400m, first_vo2_surv$ldcw47b, exclude = NULL)

# For those that did not do the 400m walk, set their ldcw47b to 0
first_vo2_surv$ldcw47b[first_vo2_surv$did400m == 0] <- 0
table(first_vo2_surv$did400m, first_vo2_surv$ldcw47b, exclude = NULL)

# For those that did do the walk and have NA in ldcw47b, set to 0
# This assumes everyone with NA in walking aid didn't use
first_vo2_surv$ldcw47b <- ifelse(is.na(first_vo2_surv$ldcw47b) & first_vo2_surv$did400m == 1,
                       0, first_vo2_surv$ldcw47b)
table(first_vo2_surv$did400m, first_vo2_surv$ldcw47b, exclude = NULL)

# betablocker has 1s and NAs; set NA to 0
table(first_vo2_surv$betablocker, exclude = NULL)
first_vo2_surv$betablocker[is.na(first_vo2_surv$betablocker)] <- 0
table(first_vo2_surv$betablocker, exclude = NULL)

# mdhx16b derives from mdhx16
# mdhx16 asks, "Are you currently using prescribed medication(s) or therapies 
# to treat your diabetes?" 
table(first_vo2_surv$mdhx16, exclude = NULL) # 0 = No, # 1 = Yes, # 8 = Don't know, # 7 = Refused

# Recoding the "Don't knows" to NAs
first_vo2_surv$mdhx16[first_vo2_surv$mdhx16 == 8] <- NA

# mdhx16b, "Do you still have high blood sugar?", was asked only of those that answered 
# "Yes" to mdhx16 and has the same coding structure as mdhx16 above
table(first_vo2_surv$mdhx16b, exclude = NULL)

# Begin by recoding 8s to NAs and NAs as 0s
first_vo2_surv$mdhx16b[first_vo2_surv$mdhx16b == 8] <- NA
first_vo2_surv$mdhx16b[is.na(first_vo2_surv$mdhx16b)] <- 0

table(first_vo2_surv$mdhx16b, exclude = NULL)
table(first_vo2_surv$mdhx16, first_vo2_surv$mdhx16b, exclude = NULL)

# Those with NA in TRM01, "Treadmill - test performed" should be 0
table(first_vo2_surv$trm01, exclude = NULL)
table(first_vo2_surv$trm01, first_vo2_surv$trmborg, exclude = NULL) 
# There seems to be 2 people that have Borg scores that didn't take the test, let's make them NA
first_vo2_surv$trmborg[first_vo2_surv$trm01 == 0] <- NA

first_vo2_surv$trm01[is.na(first_vo2_surv$trm01)] <- 0

# Clean education variable
first_vo2_surv$educat_ori <- first_vo2_surv$educat
first_vo2_surv$educat <- NA
first_vo2_surv$educat[first_vo2_surv$educat_ori == 0] <- "non-college graduate"
first_vo2_surv$educat[first_vo2_surv$educat_ori == 1] <- "non-college graduate"
first_vo2_surv$educat[first_vo2_surv$educat_ori == 2] <- "non-college graduate"
first_vo2_surv$educat[first_vo2_surv$educat_ori == 3] <- "College"
first_vo2_surv$educat[first_vo2_surv$educat_ori == 4] <- "Post college"
table(first_vo2_surv$educat, exclude = NULL)
table(first_vo2_surv$educat_ori, first_vo2_surv$educat, exclude = NULL)

# Clean hispanic origin variable
table(first_vo2_surv$demo08, exclude = NULL)
# 0 = No
# 1 = Yes
# 7 = Refused
# 8 = Don't know
first_vo2_surv$demo08_ori <- first_vo2_surv$demo08
first_vo2_surv$demo08[first_vo2_surv$demo08_ori == 0] <- "No"
first_vo2_surv$demo08[first_vo2_surv$demo08_ori == 1] <- "Yes"
first_vo2_surv$demo08[first_vo2_surv$demo08_ori == 7] <- NA
first_vo2_surv$demo08[first_vo2_surv$demo08_ori == 8] <- NA
table(first_vo2_surv$demo08_ori, first_vo2_surv$demo08, exclude = NULL)

# Clean stroke variable
table(first_vo2_surv$mdhx12, exclude = NULL)
first_vo2_surv$mdhx12_ori <- first_vo2_surv$mdhx12
first_vo2_surv$mdhx12[first_vo2_surv$mdhx12_ori == 0] <- "No"
first_vo2_surv$mdhx12[first_vo2_surv$mdhx12_ori == 1] <- "Yes"
first_vo2_surv$mdhx12[first_vo2_surv$mdhx12_ori == 8] <- NA
table(first_vo2_surv$mdhx12_ori, first_vo2_surv$mdhx12, exclude = NULL)

# Clean heart attack/MI variable
table(first_vo2_surv$mdhx3, exclude = NULL)
first_vo2_surv$mdhx3_ori <- first_vo2_surv$mdhx3
first_vo2_surv$mdhx3[first_vo2_surv$mdhx3_ori == 0] <- "No"
first_vo2_surv$mdhx3[first_vo2_surv$mdhx3_ori == 1] <- "Yes"
first_vo2_surv$mdhx3[first_vo2_surv$mdhx3_ori == 8] <- NA

# Clean Smoking History
first_vo2_surv$smokehx_ori <- first_vo2_surv$smokehx

first_vo2_surv$smokehx[first_vo2_surv$smokehx_ori == 0] <- "Never Smoker"
first_vo2_surv$smokehx[first_vo2_surv$smokehx_ori == 1] <- "Former Smoker"
first_vo2_surv$smokehx[first_vo2_surv$smokehx_ori == 2] <- "Former Smoker"
first_vo2_surv$smokehx[first_vo2_surv$smokehx_ori == 3] <- "Current Smoker"

table(first_vo2_surv$smokehx_ori, first_vo2_surv$smokehx, exclude = NULL)

# Clean race/ethnicity
table(first_vo2_surv$racecd2, first_vo2_surv$black, exclude = NULL)
first_vo2_surv$racecd2[first_vo2_surv$racecd2 == "Other/not classifiable" & first_vo2_surv$black == 1] <- "Black"
table(first_vo2_surv$racecd2, first_vo2_surv$black, exclude = NULL)

table(first_vo2_surv$racecd2, first_vo2_surv$demo08, exclude = NULL)
first_vo2_surv$racecd2[first_vo2_surv$demo08 == "Yes"] <- "Hispanic"
table(first_vo2_surv$racecd2, first_vo2_surv$demo08, exclude = NULL)
addmargins(table(first_vo2_surv$racecd2, exclude = NULL))

first_vo2_surv$racecd2[first_vo2_surv$racecd2 == "Asian/Other Pacific Islander"] <- "non-Hispanic, Asian/Other Pacific Islander"
first_vo2_surv$racecd2[first_vo2_surv$racecd2 == "Black"] <- "non-Hispanic, Black"
first_vo2_surv$racecd2[first_vo2_surv$racecd2 == "Other/not classifiable"] <- "non-Hispanic, Other/not classifiable"
first_vo2_surv$racecd2[first_vo2_surv$racecd2 == "White"] <- "non-Hispanic, White"
addmargins(table(first_vo2_surv$racecd2, exclude = NULL))

# Clean blood pressure
first_vo2_surv <- first_vo2_surv %>%
  rowwise() %>%
  mutate(sbp_final = min(rasbp, lasbp),
         dbp_final = min(lasbp, ladbp))

summary(first_vo2_surv$sbp_final)
summary(first_vo2_surv$dbp_final)

#### Drop all unnecessary columns ####
# These have no predictive power 
colnames(first_vo2_surv)

drops <- c("visit", "dov", "home_visit", "uw150", "ldcw47b", "did400m", "homevisit", "walkaid", "demo08", 
           "pe67sr1", "pe67dr1", "pe67sr2", "pe67dr2", "pe67sl1", "pe67dl1", "pe67sl2", "pe67dl2", "pe67sr3",
           "pe67dr3", "pe67sl3", "pe67dl3", "sbp", "dbp", "rasbp", "radbp", "lasbp", "ladbp", "pe67hrt",
           "sbpx" , "dbpx", "vo2kg_25", "co2exp_25", "hrstart", 
           "vo2kg_400", "co2exp_400", "ldcw03c", "hrsit", "trm01", "trm04", "trm03", "trmss1", "trmss2",
           "tssborg", "trm02", "trm01a", "max_hr", "sex", "racecd", "black",
           "dod", "censor_status", "censor_status_txt", "surv_time",
           "mets", "exercise0", "exercise1", "exercise2", "exercise3", "jurca_nasa_vo2",
           "jurca_acls_vo2", "jurca_adnfs_vo2", "brad_vo2", "jackson1990_vo2", "age_sqd",
           "htm","matthews1999_vo2", "sloan2013HR_vo2", "sloan2013noHR_vo2", "sex_desouza",
           "htin", "wtlb", "desouza_vo2", "baynard_sex", "baynard_vo2", "smokehx2","jang_vo2",
           "sex_myers","myers_vo2", "jurca_recal_vo2", "bradshaw_recal_vo2", 
           "jackson_recal_vo2", "matthews_recal_vo2", "sloan_HR_recal_vo2",
           "sloan_noHR_recal_vo2", "desouza_recal_vo2", "baynard_recal_vo2", "jang_recal_vo2",
           "myers_recal_vo2", "educat_ori", "demo08_ori", "mdhx12_ori", "mdhx3_ori",
           "smokehx_ori", "smokehx_jang", "sfhealth2_brad")

first_vo2_surv <- first_vo2_surv[!colnames(first_vo2_surv) %in% drops]
rm(drops)

colnames(first_vo2_surv)

#### Establish correct strs() for all variables ####
cont_vars <- c("age",	"wtkg",	"htcm",	"bmi", "waist", "sf12_pcscore", "sf12_mcscore", "sfhealth",
               "grmul",	"grmur",	"cs5pace", "totsbtime", "ugspeed", "sppb", "rgspeed",
               "uw150meters",	"c400secs", "wkindex", "uw150speed", "radpulse", "mdcw21", "mdcw26", 
               "hrend", "totkkwk", "totkcal", "hakcal", "walktime", "bwtime", "highxmin",
               "vo2_max", "sbp_final", "dbp_final", "trmborg", "rer_peak", "perc_max_age_pred_hr")

cat_vars <- c("idno", "smokehx","mdhx3", "mdhx4", "mdhx12",
              "mdhx16",	"mdhx16b", "mdh19a3", "exercise", "betablocker", "racecd2", "educat",
              "sex_num_m1f0")

first_vo2_surv[cont_vars] <- lapply(first_vo2_surv[cont_vars], as.character)
first_vo2_surv[cont_vars] <- lapply(first_vo2_surv[cont_vars], as.numeric)

first_vo2_surv[cat_vars] <- lapply(first_vo2_surv[cat_vars], as.character)
first_vo2_surv[cat_vars] <- lapply(first_vo2_surv[cat_vars], as.factor)

first_vo2_surv <- as.data.frame(first_vo2_surv)

#### Clean environment ####
rm(list=setdiff(ls(), "first_vo2_surv"))

#### Save the image ####
save.image("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Data/blsa.RData")

#### I kept some variables in this version of the dataset that get removed in the 2_Descriptive Statistics and Table 1.R
# script so I can table them

#### Clear environment and console ####
rm(list = ls())
cat("\014")
