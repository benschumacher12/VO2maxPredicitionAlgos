#### Clear environment and console ####
rm(list = ls())
cat("\014")

#### Load all needed packages ####
library(tidyverse)

#### Load BLSA data ####
load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Data/blsa.RData")

# Load the mortality data
load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/mortality.Rda")
first_vo2_surv <- merge(first_vo2_surv, mortality, by = c("idno"))

# I use the below code to format and clean my tables
# Formats simply takes a number, multiples by 100, and rounds to 1 decimal for a table
# The paste_fun function puts that value in quotes and forces it to have .0 if its a round number
formats <- function(x) round(x*100, 1)
paste_fun <- function(x) paste0("(",trimws(format(x, nsmall = 1)),")")

# Make sex specific quartiles of vo2max to stratify Table 1
first_vo2_surv_men <- first_vo2_surv[first_vo2_surv$sex_num_m1f0 == 1, ]
first_vo2_surv_women <- first_vo2_surv[first_vo2_surv$sex_num_m1f0 == 0, ]

summary(first_vo2_surv_men$vo2_max)
summary(first_vo2_surv_women$vo2_max)
sd(first_vo2_surv_men$vo2_max)
sd(first_vo2_surv_women$vo2_max)

# Check the restriction worked correctly
table(first_vo2_surv_men$sex_num_m1f0, exclude = NULL)
table(first_vo2_surv_women$sex_num_m1f0, exclude = NULL)

# Check the summaries of the sex-specific vo2max variables
summary(first_vo2_surv_men$vo2_max)
summary(first_vo2_surv_women$vo2_max)

Q1_men <- summary(first_vo2_surv_men$vo2_max)[2]
Q2_men <- summary(first_vo2_surv_men$vo2_max)[3]
Q3_men <- summary(first_vo2_surv_men$vo2_max)[5]

Q1_women <- summary(first_vo2_surv_women$vo2_max)[2]
Q2_women <- summary(first_vo2_surv_women$vo2_max)[3]
Q3_women <- summary(first_vo2_surv_women$vo2_max)[5]
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# First create quaritles in each sex-specific dataframe to check mortality counts in each, then 
# make quartiles in entire dataframe
first_vo2_surv_men$vo2_max_quart <- NA
first_vo2_surv_men$vo2_max_quart <- ifelse(first_vo2_surv_men$vo2_max < Q1_men,
                                       1, first_vo2_surv_men$vo2_max_quart)
first_vo2_surv_men$vo2_max_quart <- ifelse((first_vo2_surv_men$vo2_max >= Q1_men & first_vo2_surv_men$vo2_max <= Q2_men),
                                       2, first_vo2_surv_men$vo2_max_quart)
first_vo2_surv_men$vo2_max_quart <- ifelse((first_vo2_surv_men$vo2_max > Q2_men & first_vo2_surv_men$vo2_max <= Q3_men),
                                       3, first_vo2_surv_men$vo2_max_quart)
first_vo2_surv_men$vo2_max_quart <- ifelse((first_vo2_surv_men$vo2_max > Q3_men),
                                       4, first_vo2_surv_men$vo2_max_quart)                               
addmargins(table(first_vo2_surv_men$vo2_max_quart, exclude = NULL))

# censor_status_txt
table(first_vo2_surv_men$censor_status_txt, first_vo2_surv_men$vo2_max_quart, exclude = NULL)
levels <- 2
Characteristic <- "Censor Status, n (%)"
level1 <- "Censored"
level2 <- "Dead"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv_men$censor_status_txt, first_vo2_surv_men$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv_men$censor_status_txt, first_vo2_surv_men$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv_men$censor_status_txt, first_vo2_surv_men$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
censor_status_txt <- as.data.frame(m); rm(m); rm(i)
censor_status_txt <- rbind(rep(NA, ncol(censor_status_txt)), censor_status_txt)
censor_status_txt <- cbind(rep(NA, nrow(censor_status_txt)), censor_status_txt)
colnames(censor_status_txt) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

censor_status_txt$Characteristic[1] <- Characteristic
censor_status_txt$Characteristic[2] <- level1
censor_status_txt$Characteristic[3] <- level2

censor_status_txt$Pvalue[1] <- censor_status_txt$Pvalue[2]
censor_status_txt$Pvalue[2] <- NA
censor_status_txt$Pvalue[1] <- ifelse(as.numeric(censor_status_txt$Pvalue[1]) < 0.01, "< 0.01",
                                      round(as.numeric(censor_status_txt$Pvalue[1], 3)))
censor_status_txt_men <- censor_status_txt

first_vo2_surv_women$vo2_max_quart <- NA
first_vo2_surv_women$vo2_max_quart <- ifelse(first_vo2_surv_women$vo2_max < Q1_women,
                                           1, first_vo2_surv_women$vo2_max_quart)
first_vo2_surv_women$vo2_max_quart <- ifelse((first_vo2_surv_women$vo2_max >= Q1_women & first_vo2_surv_women$vo2_max <= Q2_women),
                                           2, first_vo2_surv_women$vo2_max_quart)
first_vo2_surv_women$vo2_max_quart <- ifelse((first_vo2_surv_women$vo2_max > Q2_women & first_vo2_surv_women$vo2_max <= Q3_women),
                                           3, first_vo2_surv_women$vo2_max_quart)
first_vo2_surv_women$vo2_max_quart <- ifelse((first_vo2_surv_women$vo2_max > Q3_women),
                                           4, first_vo2_surv_women$vo2_max_quart)                               
addmargins(table(first_vo2_surv_women$vo2_max_quart, exclude = NULL))

# censor_status_txt
table(first_vo2_surv_women$censor_status_txt, first_vo2_surv_women$vo2_max_quart, exclude = NULL)
levels <- 2
Characteristic <- "Censor Status, n (%)"
level1 <- "Censored"
level2 <- "Dead"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv_women$censor_status_txt, first_vo2_surv_women$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv_women$censor_status_txt, first_vo2_surv_women$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv_women$censor_status_txt, first_vo2_surv_women$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
censor_status_txt <- as.data.frame(m); rm(m); rm(i)
censor_status_txt <- rbind(rep(NA, ncol(censor_status_txt)), censor_status_txt)
censor_status_txt <- cbind(rep(NA, nrow(censor_status_txt)), censor_status_txt)
colnames(censor_status_txt) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

censor_status_txt$Characteristic[1] <- Characteristic
censor_status_txt$Characteristic[2] <- level1
censor_status_txt$Characteristic[3] <- level2

censor_status_txt$Pvalue[1] <- censor_status_txt$Pvalue[2]
censor_status_txt$Pvalue[2] <- NA
censor_status_txt$Pvalue[1] <- ifelse(as.numeric(censor_status_txt$Pvalue[1]) < 0.01, "< 0.01",
                                      round(as.numeric(censor_status_txt$Pvalue[1], 3)))
censor_status_txt_women <- censor_status_txt
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# Now do it for the whole dataframe
first_vo2_surv$vo2_max_quart <- NA
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 1 & (first_vo2_surv$vo2_max < Q1_men)),
                                        1, first_vo2_surv$vo2_max_quart)
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 1 & (first_vo2_surv$vo2_max >= Q1_men & first_vo2_surv$vo2_max <= Q2_men)),
                                       2, first_vo2_surv$vo2_max_quart)
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 1 & (first_vo2_surv$vo2_max > Q2_men & first_vo2_surv$vo2_max <= Q3_men)),
                                       3, first_vo2_surv$vo2_max_quart)
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 1 & (first_vo2_surv$vo2_max > Q3_men)),
                                       4, first_vo2_surv$vo2_max_quart)                               
addmargins(table(first_vo2_surv$sex_num_m1f0, first_vo2_surv$vo2_max_quart, exclude = NULL))

first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 0 & (first_vo2_surv$vo2_max < Q1_women)),
                                       1, first_vo2_surv$vo2_max_quart)
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 0 & (first_vo2_surv$vo2_max >= Q1_women & first_vo2_surv$vo2_max <= Q2_women)),
                                       2, first_vo2_surv$vo2_max_quart)
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 0 & (first_vo2_surv$vo2_max > Q2_women & first_vo2_surv$vo2_max <= Q3_women)),
                                       3, first_vo2_surv$vo2_max_quart)
first_vo2_surv$vo2_max_quart <- ifelse((first_vo2_surv$sex_num_m1f0 == 0 & (first_vo2_surv$vo2_max > Q3_women)),
                                       4, first_vo2_surv$vo2_max_quart)                               
addmargins(table(first_vo2_surv$sex_num_m1f0, first_vo2_surv$vo2_max_quart, exclude = NULL))

addmargins(table(first_vo2_surv$vo2_max_quart, exclude = NULL))
prop.table(table(first_vo2_surv$vo2_max_quart, exclude = NULL))
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#### Table 1 ####
mean(first_vo2_surv$vo2_max)
sd(first_vo2_surv$vo2_max)

# censor_status_txt
table(first_vo2_surv$censor_status_txt, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 2
Characteristic <- "Censor Status, n (%)"
level1 <- "Censored"
level2 <- "Dead"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$censor_status_txt, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$censor_status_txt, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$censor_status_txt, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
censor_status_txt <- as.data.frame(m); rm(m); rm(i)
censor_status_txt <- rbind(rep(NA, ncol(censor_status_txt)), censor_status_txt)
censor_status_txt <- cbind(rep(NA, nrow(censor_status_txt)), censor_status_txt)
colnames(censor_status_txt) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

censor_status_txt$Characteristic[1] <- Characteristic
censor_status_txt$Characteristic[2] <- level1
censor_status_txt$Characteristic[3] <- level2

censor_status_txt$Pvalue[1] <- censor_status_txt$Pvalue[2]
censor_status_txt$Pvalue[2] <- NA
censor_status_txt$Pvalue[1] <- ifelse(as.numeric(censor_status_txt$Pvalue[1]) < 0.01, "< 0.01",
                           round(as.numeric(censor_status_txt$Pvalue[1], 3)))

# educat
table(first_vo2_surv$educat, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 4
Characteristic <- "Highest attained education, n (%)"
level1 <- "College"
level2 <- "non-college graduate"
level3 <- "Post college"
level4 <- "Unknown"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$educat, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$educat, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$educat, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
educat <- as.data.frame(m); rm(m); rm(i)
educat <- rbind(rep(NA, ncol(educat)), educat)
educat <- cbind(rep(NA, nrow(educat)), educat)
colnames(educat) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

educat$Characteristic[1] <- Characteristic
educat$Characteristic[2] <- level1
educat$Characteristic[3] <- level2
educat$Characteristic[4] <- level3
educat$Characteristic[5] <- level4

educat$Pvalue[1] <- educat$Pvalue[2]
educat$Pvalue[2] <- NA
educat$Pvalue[1] <- ifelse(as.numeric(educat$Pvalue[1]) < 0.01, "< 0.01",
                          round(as.numeric(educat$Pvalue[1], 3)))

# betablocker
table(first_vo2_surv$betablocker, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 2
Characteristic <- "Beta Blocker Use, n (%)"
level1 <- "No"
level2 <- "Yes"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$betablocker, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$betablocker, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$betablocker, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
betablocker <- as.data.frame(m); rm(m); rm(i)
betablocker <- rbind(rep(NA, ncol(betablocker)), betablocker)
betablocker <- cbind(rep(NA, nrow(betablocker)), betablocker)
colnames(betablocker) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

betablocker$Characteristic[1] <- Characteristic
betablocker$Characteristic[2] <- level1
betablocker$Characteristic[3] <- level2

betablocker$Pvalue[1] <- betablocker$Pvalue[2]
betablocker$Pvalue[2] <- NA
betablocker$Pvalue[1] <- ifelse(as.numeric(betablocker$Pvalue[1]) < 0.01, "< 0.01",
                           round(as.numeric(betablocker$Pvalue[1], 3)))

# sfhealth
table(first_vo2_surv$sfhealth, exclude = NULL)

first_vo2_surv$sfhealth2 <- NA
first_vo2_surv$sfhealth2[first_vo2_surv$sfhealth == 1] <- "Excellent"
first_vo2_surv$sfhealth2[first_vo2_surv$sfhealth == 2] <- "Very Good/Good"
first_vo2_surv$sfhealth2[first_vo2_surv$sfhealth == 3] <- "Very Good/Good"
first_vo2_surv$sfhealth2[first_vo2_surv$sfhealth == 4] <- "Fair/Poor"
first_vo2_surv$sfhealth2[first_vo2_surv$sfhealth == 7] <- NA
first_vo2_surv$sfhealth2[first_vo2_surv$sfhealth == 8] <- NA
table(first_vo2_surv$sfhealth2, exclude = NULL)
table(first_vo2_surv$sfhealth, first_vo2_surv$sfhealth2, exclude = NULL)

table(first_vo2_surv$sfhealth2, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 4
Characteristic <- "Self Rated Health, n (%)"
level1 <- "Excellent"
level2 <- "Fair/Poor"
level3 <- "Very Good/Good"
level4 <- "Missing"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$sfhealth2, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$sfhealth2, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$sfhealth2, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
sfhealth2 <- as.data.frame(m); rm(m); rm(i)
sfhealth2 <- rbind(rep(NA, ncol(sfhealth2)), sfhealth2)
sfhealth2 <- cbind(rep(NA, nrow(sfhealth2)), sfhealth2)
colnames(sfhealth2) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

sfhealth2$Characteristic[1] <- Characteristic
sfhealth2$Characteristic[2] <- level1
sfhealth2$Characteristic[3] <- level2
sfhealth2$Characteristic[4] <- level3
sfhealth2$Characteristic[5] <- level4

sfhealth2$Pvalue[1] <- sfhealth2$Pvalue[2]
sfhealth2$Pvalue[2] <- NA
sfhealth2$Pvalue[1] <- ifelse(as.numeric(sfhealth2$Pvalue[1]) < 0.01, "< 0.01",
                           round(as.numeric(sfhealth2$Pvalue[1], 3)))

# smokehx
table(first_vo2_surv$smokehx, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 4
Characteristic <- "Smoking Status, n (%)"
level1 <- "Current"
level2 <- "Former"
level3 <- "Never"
level4 <- "Unknown"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$smokehx, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$smokehx, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$smokehx, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
smokehx <- as.data.frame(m); rm(m); rm(i)
smokehx <- rbind(rep(NA, ncol(smokehx)), smokehx)
smokehx <- cbind(rep(NA, nrow(smokehx)), smokehx)
colnames(smokehx) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

smokehx$Characteristic[1] <- Characteristic
smokehx$Characteristic[2] <- level1
smokehx$Characteristic[3] <- level2
smokehx$Characteristic[4] <- level3
smokehx$Characteristic[5] <- level4

smokehx$Pvalue[1] <- smokehx$Pvalue[2]
smokehx$Pvalue[2] <- NA
smokehx$Pvalue[1] <- ifelse(as.numeric(smokehx$Pvalue[1]) < 0.01, "< 0.01",
                             round(as.numeric(smokehx$Pvalue[1], 3)))

# exercise
table(first_vo2_surv$exercise, exclude = NULL)
first_vo2_surv$exercise_recode <- NA
first_vo2_surv$exercise_recode[first_vo2_surv$exercise == 0] <- "0-29"
first_vo2_surv$exercise_recode[first_vo2_surv$exercise == 1] <- "30 -74"
first_vo2_surv$exercise_recode[first_vo2_surv$exercise == 2] <- "75 - 149"
first_vo2_surv$exercise_recode[first_vo2_surv$exercise == 3] <- "150+"
table(first_vo2_surv$exercise_recode, exclude = NULL)
table(first_vo2_surv$exercise, first_vo2_surv$exercise_recode, exclude = NULL)

table(first_vo2_surv$exercise_recode, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 5
Characteristic <- "Minutes of Exercise, n (%)"
level1 <- "0-29"
level2 <- "150+"
level3 <- "30 - 74"
level4 <- "75 - 149"
level5 <- "Unknown"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$exercise_recode, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$exercise_recode, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$exercise_recode, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
exercise_recode <- as.data.frame(m); rm(m); rm(i)
exercise_recode <- rbind(rep(NA, ncol(exercise_recode)), exercise_recode)
exercise_recode <- cbind(rep(NA, nrow(exercise_recode)), exercise_recode)
colnames(exercise_recode) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

exercise_recode$Characteristic[1] <- Characteristic
exercise_recode$Characteristic[2] <- level1
exercise_recode$Characteristic[3] <- level2
exercise_recode$Characteristic[4] <- level3
exercise_recode$Characteristic[5] <- level4
exercise_recode$Characteristic[6] <- level5

exercise_recode$Pvalue[1] <- exercise_recode$Pvalue[2]
exercise_recode$Pvalue[2] <- NA
exercise_recode$Pvalue[1] <- ifelse(as.numeric(exercise_recode$Pvalue[1]) < 0.01, "< 0.01",
                            round(as.numeric(exercise_recode$Pvalue[1], 3)))

# racecd2
table(first_vo2_surv$racecd2, first_vo2_surv$vo2_max_quart, exclude = NULL)
levels <- 5
Characteristic <- "Race and Ethnicity, n (%)"
level1 <- "Hispanic"
level2 <- "non-Hispanic, Asian/Other Pacific Islander"
level3 <- "non-Hispanic, Black"
level4 <- "non-Hispanic, Other/not classifiable"
level5 <- "non-Hispanic, White"

m <- matrix(nrow = levels, ncol = 11)

m[1:levels, 2:5] <- table(first_vo2_surv$racecd2, first_vo2_surv$vo2_max_quart, exclude = NULL)
m[1:levels, 7:10] <- prop.table(table(first_vo2_surv$racecd2, first_vo2_surv$vo2_max_quart, exclude = NULL), 2)
m[1:levels, 1] <- m[1:levels, 2] + m[1:levels, 3] + m[1:levels, 4] + m[1:levels, 5]
m[1:levels, 6] <- m[1:levels, 1] / sum(m[,1])
m[1, 11] <- chisq.test(first_vo2_surv$racecd2, first_vo2_surv$vo2_max_quart)$p.value

m[, 6:10] <- apply(m[, 6:10], 2, formats)
m[, 6:10] <- apply(m[, 6:10], 2, paste_fun)

for (i in seq(1, 5)){
  m[,i] <- paste0(m[, i]," ",m[, i + 5])
}

m <- m[, -(6:10)]
racecd2 <- as.data.frame(m); rm(m); rm(i)
racecd2 <- rbind(rep(NA, ncol(racecd2)), racecd2)
racecd2 <- cbind(rep(NA, nrow(racecd2)), racecd2)
colnames(racecd2) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

racecd2$Characteristic[1] <- Characteristic
racecd2$Characteristic[2] <- level1
racecd2$Characteristic[3] <- level2
racecd2$Characteristic[4] <- level3
racecd2$Characteristic[5] <- level4
racecd2$Characteristic[6] <- level5

racecd2$Pvalue[1] <- racecd2$Pvalue[2]
racecd2$Pvalue[2] <- NA
racecd2$Pvalue[1] <- ifelse(as.numeric(racecd2$Pvalue[1]) < 0.01, "< 0.01",
                             round(as.numeric(racecd2$Pvalue[1], 3)))

# age
age <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$age, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$age, na.rm = TRUE)

age$mean <- round(age$mean, 1)
age$sd <- round(age$sd, 1)
age$mean_sd <- paste0(age$mean, " (", age$sd, ")")

age <- age[ , -(1:3)]
age <- as.data.frame(t(age))
age <- cbind(rep(NA, nrow(age)), age)
age[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

age.aov <- aov(age ~ vo2_max_quart, data = first_vo2_surv)
age$p_value <- as.numeric(summary(age.aov)[[1]][["Pr(>F)"]][[1]])
age$p_value <- ifelse(age$p_value < 0.01, "< 0.01", age$p_value)
age$Characteristic <- "Age, Mean (SD)"
age <- age[ , c(7, 1:6)]
colnames(age) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# sbp
first_vo2_surv$sbp_final <- as.numeric(first_vo2_surv$sbp_final)

sbp <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(sbp_final, na.rm = TRUE),
    sd = sd(sbp_final, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$sbp_final, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$sbp_final, na.rm = TRUE)

sbp$mean <- round(sbp$mean, 1)
sbp$sd <- round(sbp$sd, 1)
sbp$mean_sd <- paste0(sbp$mean, " (", sbp$sd, ")")

sbp <- sbp[ , -(1:3)]
sbp <- as.data.frame(t(sbp))
sbp <- cbind(rep(NA, nrow(sbp)), sbp)
sbp[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

sbp.aov <- aov(sbp_final ~ vo2_max_quart, data = first_vo2_surv, na.action = na.omit)
sbp$p_value <- as.numeric(summary(sbp.aov)[[1]][["Pr(>F)"]][[1]])
sbp$p_value <- ifelse(sbp$p_value < 0.01, "< 0.01", sbp$p_value)
sbp$Characteristic <- "Systolic BP, Mean (SD)"
sbp <- sbp[ , c(7, 1:6)]
colnames(sbp) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# dbp
dbp <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(dbp_final, na.rm = TRUE),
    sd = sd(dbp_final, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$dbp_final, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$dbp_final, na.rm = TRUE)

dbp$mean <- round(dbp$mean, 1)
dbp$sd <- round(dbp$sd, 1)
dbp$mean_sd <- paste0(dbp$mean, " (", dbp$sd, ")")

dbp <- dbp[ , -(1:3)]
dbp <- as.data.frame(t(dbp))
dbp <- cbind(rep(NA, nrow(dbp)), dbp)
dbp[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

dbp.aov <- aov(dbp_final ~ vo2_max_quart, data = first_vo2_surv, na.action = na.omit)
dbp$p_value <- as.numeric(summary(dbp.aov)[[1]][["Pr(>F)"]][[1]])
dbp$p_value <- ifelse(dbp$p_value < 0.01, "< 0.01", dbp$p_value)
dbp$Characteristic <- "Diastolic BP, Mean (SD)"
dbp <- dbp[ , c(7, 1:6)]
colnames(dbp) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# bmi
bmi <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(bmi, na.rm = TRUE),
    sd = sd(bmi, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$bmi, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$bmi, na.rm = TRUE)

bmi$mean <- round(bmi$mean, 1)
bmi$sd <- round(bmi$sd, 1)
bmi$mean_sd <- paste0(bmi$mean, " (", bmi$sd, ")")

bmi <- bmi[ , -(1:3)]
bmi <- as.data.frame(t(bmi))
bmi <- cbind(rep(NA, nrow(bmi)), bmi)
bmi[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

bmi.aov <- aov(bmi ~ vo2_max_quart, data = first_vo2_surv)
bmi$p_value <- as.numeric(summary(bmi.aov)[[1]][["Pr(>F)"]][[1]])
bmi$p_value <- ifelse(bmi$p_value < 0.01, "< 0.01", bmi$p_value)
bmi$Characteristic <- "BMI, Mean (SD)"
bmi <- bmi[ , c(7, 1:6)]
colnames(bmi) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# sf12_pcscore
sf12_pcscore <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(sf12_pcscore, na.rm = TRUE),
    sd = sd(sf12_pcscore, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$sf12_pcscore, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$sf12_pcscore, na.rm = TRUE)

sf12_pcscore$mean <- round(sf12_pcscore$mean, 1)
sf12_pcscore$sd <- round(sf12_pcscore$sd, 1)
sf12_pcscore$mean_sd <- paste0(sf12_pcscore$mean, " (", sf12_pcscore$sd, ")")

sf12_pcscore <- sf12_pcscore[ , -(1:3)]
sf12_pcscore <- as.data.frame(t(sf12_pcscore))
sf12_pcscore <- cbind(rep(NA, nrow(sf12_pcscore)), sf12_pcscore)
sf12_pcscore[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

sf12_pcscore.aov <- aov(sf12_pcscore ~ vo2_max_quart, data = first_vo2_surv, na.action = na.omit)
sf12_pcscore$p_value <- as.numeric(summary(sf12_pcscore.aov)[[1]][["Pr(>F)"]][[1]])
sf12_pcscore$p_value <- ifelse(sf12_pcscore$p_value < 0.01, "< 0.01", sf12_pcscore$p_value)
sf12_pcscore$Characteristic <- "sf12_pcscore, Mean (SD)"
sf12_pcscore <- sf12_pcscore[ , c(7, 1:6)]
colnames(sf12_pcscore) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# sf12_mcscore
sf12_mcscore <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(sf12_mcscore, na.rm = TRUE),
    sd = sd(sf12_mcscore, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$sf12_mcscore, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$sf12_mcscore, na.rm = TRUE)

sf12_mcscore$mean <- round(sf12_mcscore$mean, 1)
sf12_mcscore$sd <- round(sf12_mcscore$sd, 1)
sf12_mcscore$mean_sd <- paste0(sf12_mcscore$mean, " (", sf12_mcscore$sd, ")")

sf12_mcscore <- sf12_mcscore[ , -(1:3)]
sf12_mcscore <- as.data.frame(t(sf12_mcscore))
sf12_mcscore <- cbind(rep(NA, nrow(sf12_mcscore)), sf12_mcscore)
sf12_mcscore[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

sf12_mcscore.aov <- aov(sf12_mcscore ~ vo2_max_quart, data = first_vo2_surv, na.action = na.omit)
sf12_mcscore$p_value <- as.numeric(summary(sf12_mcscore.aov)[[1]][["Pr(>F)"]][[1]])
sf12_mcscore$p_value <- ifelse(sf12_mcscore$p_value < 0.01, "< 0.01", sf12_mcscore$p_value)
sf12_mcscore$Characteristic <- "sf12_mcscore, Mean (SD)"
sf12_mcscore <- sf12_mcscore[ , c(7, 1:6)]
colnames(sf12_mcscore) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# vo2_max
vo2_max <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    median = median(vo2_max, na.rm = TRUE),
    sd = sd(vo2_max, na.rm = TRUE)
  )
tot_mean <- median(first_vo2_surv$vo2_max, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$vo2_max, na.rm = TRUE)

vo2_max$median <- round(vo2_max$median, 1)
vo2_max$sd <- round(vo2_max$sd, 1)
vo2_max$median_sd <- paste0(vo2_max$median, " (", vo2_max$sd, ")")

vo2_max <- vo2_max[ , -(1:3)]
vo2_max <- as.data.frame(t(vo2_max))
vo2_max <- cbind(rep(NA, nrow(vo2_max)), vo2_max)
vo2_max[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

vo2_max.aov <- aov(vo2_max ~ vo2_max_quart, data = first_vo2_surv)
vo2_max$p_value <- as.numeric(summary(vo2_max.aov)[[1]][["Pr(>F)"]][[1]])
vo2_max$p_value <- ifelse(vo2_max$p_value < 0.01, "< 0.01", vo2_max$p_value)
vo2_max$Characteristic <- "VO2max, Median (SD)"
vo2_max <- vo2_max[ , c(7, 1:6)]
colnames(vo2_max) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# rer_peak
rer_peak <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(rer_peak, na.rm = TRUE),
    sd = sd(rer_peak, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$rer_peak, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$rer_peak, na.rm = TRUE)

rer_peak$mean <- round(rer_peak$mean, 1)
rer_peak$sd <- round(rer_peak$sd, 1)
rer_peak$mean_sd <- paste0(rer_peak$mean, " (", rer_peak$sd, ")")

rer_peak <- rer_peak[ , -(1:3)]
rer_peak <- as.data.frame(t(rer_peak))
rer_peak <- cbind(rep(NA, nrow(rer_peak)), rer_peak)
rer_peak[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

rer_peak.aov <- aov(rer_peak ~ vo2_max_quart, data = first_vo2_surv)
rer_peak$p_value <- as.numeric(summary(rer_peak.aov)[[1]][["Pr(>F)"]][[1]])
rer_peak$p_value <- ifelse(rer_peak$p_value < 0.01, "< 0.01", rer_peak$p_value)
rer_peak$Characteristic <- "RER, Mean (SD)"
rer_peak <- rer_peak[ , c(7, 1:6)]
colnames(rer_peak) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# trmborg
trmborg <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(trmborg, na.rm = TRUE),
    sd = sd(trmborg, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$trmborg, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$trmborg, na.rm = TRUE)

trmborg$mean <- round(trmborg$mean, 1)
trmborg$sd <- round(trmborg$sd, 1)
trmborg$mean_sd <- paste0(trmborg$mean, " (", trmborg$sd, ")")

trmborg <- trmborg[ , -(1:3)]
trmborg <- as.data.frame(t(trmborg))
trmborg <- cbind(rep(NA, nrow(trmborg)), trmborg)
trmborg[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

trmborg.aov <- aov(trmborg ~ vo2_max_quart, data = first_vo2_surv, na.action = na.omit)
trmborg$p_value <- as.numeric(summary(trmborg.aov)[[1]][["Pr(>F)"]][[1]])
trmborg$p_value <- ifelse(trmborg$p_value < 0.01, "< 0.01", trmborg$p_value)
trmborg$Characteristic <- "Borg, Mean (SD)"
trmborg <- trmborg[ , c(7, 1:6)]
colnames(trmborg) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

# perc_max_age_pred_hr
perc_max_age_pred_hr <- group_by(first_vo2_surv, vo2_max_quart) %>%
  summarise(
    mean = mean(perc_max_age_pred_hr, na.rm = TRUE),
    sd = sd(perc_max_age_pred_hr, na.rm = TRUE)
  )
tot_mean <- mean(first_vo2_surv$perc_max_age_pred_hr, na.rm = TRUE)
tot_sd <- sd(first_vo2_surv$perc_max_age_pred_hr, na.rm = TRUE)

perc_max_age_pred_hr$mean <- round(perc_max_age_pred_hr$mean, 1)
perc_max_age_pred_hr$sd <- round(perc_max_age_pred_hr$sd, 1)
perc_max_age_pred_hr$mean_sd <- paste0(perc_max_age_pred_hr$mean, " (", perc_max_age_pred_hr$sd, ")")

perc_max_age_pred_hr <- perc_max_age_pred_hr[ , -(1:3)]
perc_max_age_pred_hr <- as.data.frame(t(perc_max_age_pred_hr))
perc_max_age_pred_hr <- cbind(rep(NA, nrow(perc_max_age_pred_hr)), perc_max_age_pred_hr)
perc_max_age_pred_hr[1, 1] <- paste0(round(tot_mean, 1), " (", round(tot_sd, 1),")")

perc_max_age_pred_hr.aov <- aov(perc_max_age_pred_hr ~ vo2_max_quart, data = first_vo2_surv)
perc_max_age_pred_hr$p_value <- as.numeric(summary(perc_max_age_pred_hr.aov)[[1]][["Pr(>F)"]][[1]])
perc_max_age_pred_hr$p_value <- ifelse(perc_max_age_pred_hr$p_value < 0.01, "< 0.01", perc_max_age_pred_hr$p_value)
perc_max_age_pred_hr$Characteristic <- "% max age pred hr, Mean (SD)"
perc_max_age_pred_hr <- perc_max_age_pred_hr[ , c(7, 1:6)]
colnames(perc_max_age_pred_hr) <- c("Characteristic", "Total" ,"Q1", "Q2", "Q3", "Q4", "Pvalue")

#### Merge all Table 1 DFs and save as CSV ####
Table1 <- rbind(age,
                racecd2,
                educat,
                bmi,
                betablocker,
                exercise_recode,
                sfhealth2,
                sbp,
                dbp,
                smokehx,
                vo2_max,
                rer_peak,
                trmborg,
                perc_max_age_pred_hr)

row.names(Table1) <- NULL

write.csv(Table1, "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Table1.csv",
          row.names = FALSE)

# Drop some variables and save the image
drops <- c("rer_peak", "trmborg", "perc_max_age_pred_hr", "sfhealth", "exercise", "vo2_max_quart")
first_vo2_surv <- first_vo2_surv[!colnames(first_vo2_surv) %in% drops]
rm(drops)

first_vo2_surv$sfhealth2 <- as.factor(as.character(first_vo2_surv$sfhealth2))
first_vo2_surv$exercise_recode <- as.factor(as.character(first_vo2_surv$exercise_recode))

#### Clean environment ####
rm(list=setdiff(ls(), "first_vo2_surv"))

#### Save the image ####
save.image("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Data/blsa.RData")

#### Clear environment and console ####
rm(list = ls())
cat("\014")
