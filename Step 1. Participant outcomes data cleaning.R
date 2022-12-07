## HPRC EBP Participant Outcomes
## 11/28/2022


# clear work space of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
  
}


# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

# Load the other packages, installing as needed.
pacman::p_load(readr, dplyr, tidyr, lubridate, table1, stringr)

#set working directory 
# setwd("~/Documents/Git/HPRC EBP")

# read in data
df_outcomes <- read_csv("RemoteEBPCLEANPartic_DATA_2022-11-30_1445.csv") ## 1907 obs. 205 var. (downloaded data 12/2) 

#3 Create a copy of data for testing
df_test <- df_outcomes

table(df_outcomes$redcap_event_name, useNA = "always") 
## N = 496 admin arm 
## N = 277 outreach arm 1
## N = 252 post arm 1
## N = 496 pre arm 1

# filter out record id with z - indicates duplicate
df_outcomes <- df_outcomes %>%
  filter(!str_detect(record_id, "z")) # N = 1871

##filter out pre/post arm for summary scales and descriptives

df_outcomes <- df_outcomes %>%
  filter(str_detect(redcap_event_name, "pre|post")) # N = 890

## look at just post data
df_post <- df_outcomes %>%
  filter(str_detect(redcap_event_name, "post")) # N =285 but a lot of these look missing

df_pre <- df_outcomes %>%
  filter(str_detect(redcap_event_name, "pre"))

## filter out missing post data
# df_outcomes <- df_outcomes %>% 
#   filter(is.na(program_survey_time))

###### Summary scales #######

### Loneliness (sum of items: sc1- sc3) ###
# check one of the variables
table(df_outcomes$sc1, useNA = "always") # range 1-3

## create loneliness measure (range is 3-9)
df_outcomes <- df_outcomes %>%
  mutate(loneliness = sc1 + sc2 + sc3)

#check variable
table(df_outcomes$loneliness, useNA = "always")

### Social isolation (sum of items: sc4- sc8) ###
# check one of the variables
table(df_outcomes$sc4, useNA = "always") # range 1-5

## create social isolation measure (range is 3-25)
df_outcomes <- df_outcomes %>%
  mutate(soc_isolation = sc4 + sc5 + sc6 + sc7 + sc8)

#check variable
table(df_outcomes$soc_isolation, useNA = "always")

### Anxiety (sum of items: emo1- emo2)
# check one of the variables
table(df_outcomes$emo1, useNA = "always") # range 0-3

## create anxiety measure (range is 0-6)
df_outcomes <- df_outcomes %>%
  mutate(anxiety = emo1 + emo2)

#check variable
table(df_outcomes$anxiety, useNA = "always")

### Depression (sum of items: emo3- emo10
# check one of the variables
table(df_outcomes$emo3, useNA = "always") # range 0-3

## create depression measure (range is 0-24)
df_outcomes <- df_outcomes %>%
  mutate(depression = emo3 + emo4 + emo5 + emo6 + emo7 + emo8 + emo9 + emo10)

#check variable
table(df_outcomes$depression, useNA = "always")

### Physical activity (Multiple # of days by # of minutes: pa1 and pa2)
# check one of the variables
table(df_outcomes$pa1, useNA = "always") # range 0-7
table(df_outcomes$pa2, useNA = "always") # range 0-7

## recode pa2 to minutes - using the lower range number to be conservative 
## for 1 = less than 15 min, using 1
df_outcomes <- df_outcomes %>%
  mutate(pa2_recoded = 
           case_when(pa2 == 1 ~ "7.5",
                     pa2 == 2 ~ "22.5",
                     pa2 == 3 ~ "37",
                     pa2 == 4 ~ "52",
                     pa2 == 5 ~ "67",
                     pa2 == 6 ~ "82.5",
                     pa2 == 7 ~ "90"))

#check variable
table(df_outcomes$pa2_recoded, useNA = "always")

## make new pa2_recoded variable numeric
df_outcomes$pa2_recoded <- as.numeric(df_outcomes$pa2_recoded)

## create physical activity measure (range is 1-630, so we get days * minutes)
df_outcomes <- df_outcomes %>%
  mutate(phys_activity =
           pa1*pa2_recoded)


#check variable
table(df_outcomes$phys_activity, useNA = "always")


### Social needs (Add up number of responses- including other: resources & resources_other
# check one of the variables
table(df_outcomes$resources___1, useNA = "always") # range 0/1
table(df_outcomes$resources___0, useNA = "always") # range 0/1, not including _0 (I do not need help meeting any basic needs)

## social needs cleaning of resources variable

# copy of data to refer back to)
write.csv(df_outcomes,"df_outcomes.csv", row.names = FALSE)

## check resources___other
table(df_outcomes$resources_other)

## recoding resources_other responses N=45
## if response fit into existing choices, then recode, otherwise code as an ADL
# Create ADL option
df_outcomes <- df_outcomes %>%
  mutate(resources_ADL = 0)

# Create other option to capture anything else
df_outcomes <- df_outcomes %>%
  mutate(resources_other = 0)

#record id 3018 resources_other --> social support (resources_10)
df_outcomes$resources___10[df_outcomes$record_id == 3018 & df_outcomes$redcap_event_name == "pre_arm_3"] <- 1

#record id 1339 resources_other --> transportation (resources_11) (already selected won't change count)
#df_outcomes$resources___11[df_outcomes$record_id == 1339 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1204 resources_other --> caregiver support (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1204 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1277 resources_other --> caregiver support (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1277 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 3003 resources_other --> caregiver support (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 3003 & df_outcomes$redcap_event_name == "pre_arm_3"] <- 1

#record id 1295 resources_other --> ADL (resources_ADL)
df_outcomes$resources_ADL[df_outcomes$record_id == 1295 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1264 resources_other --> caregiver support (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1264 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 2507 resources_other --> healthcare (resources_5)
df_outcomes$resources___5[df_outcomes$record_id == 2507 & df_outcomes$redcap_event_name == "post_arm_2"] <- 1

#record id 1295 resources_other --> healthcare (resources_5) & caregiving (resouces_1) & transportation (resources_11)
df_outcomes$resources___5[df_outcomes$record_id == 1295 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1
df_outcomes$resources___1[df_outcomes$record_id == 1295 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1
df_outcomes$resources___11[df_outcomes$record_id == 1295 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1437 resources_other --> other (resources_other)
df_outcomes$resources_other[df_outcomes$record_id == 1437 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1142 resources_other --> social support (resources_10)
df_outcomes$resources___10[df_outcomes$record_id == 1142 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1457 resources_other --> financial (resources_3) already selected
#df_outcomes$resources___3[df_outcomes$record_id == 1457 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1386 resources_other --> social support (resources_10) already selected
#df_outcomes$resources___10[df_outcomes$record_id == 1386 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1344 resources_other --> food (resources_4)
df_outcomes$resources___4[df_outcomes$record_id == 1344 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1244 resources_other --> o (resources_0)
df_outcomes$resources___0[df_outcomes$record_id == 1244 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1299 resources_other --> 0 (resources_0)
df_outcomes$resources___0[df_outcomes$record_id == 1299 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1392 resources_other --> 0 but don't recode to resources___0 because selected other things

#record id 1265 resources_other --> housing (resources_6) but already selected won't affect count 
#df_outcomes$resources___6[df_outcomes$record_id == 1265 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 4015 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 4015 & df_outcomes$redcap_event_name == "pre_arm_4"] <- 1

#record id 3062 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 3062 & df_outcomes$redcap_event_name == "pre_arm_3"] <- 1

#record id 1474 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1474 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1473 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1473 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1429 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1429 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1237 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1237 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 4010 resources_other --> transportation (resources_11)
df_outcomes$resources___5[df_outcomes$record_id == 4010 & df_outcomes$redcap_event_name == "pre_arm_4"] <- 1

#record id 1427 resources_other --> 0 (resources_0)
df_outcomes$resources___0[df_outcomes$record_id == 1427 & df_outcomes$redcap_event_name == "post_arm_1"] <- 1

#record id 1005 resources_other --> social support (resources_10)
df_outcomes$resources___10[df_outcomes$record_id == 1005 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 3052 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 3052 & df_outcomes$redcap_event_name == "pre_arm_3"] <- 1

#record id 1385 resources_other --> caregiving (resources_1)
df_outcomes$resources___1[df_outcomes$record_id == 1385 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1

#record id 1053 resources_other --> financial (resources_3)
df_outcomes$resources___3[df_outcomes$record_id == 1053 & df_outcomes$redcap_event_name == "pre_arm_1"] <- 1


## check counts initially and as I go
table(df_outcomes$resources___0) #N = 484 -> 485 -> 486 -> 487
table(df_outcomes$resources___1) #N = 36 -> 37 -> 38 -> 39 -> 40 -> 41 -> 42 -> 47 -> 48 ->49
table(df_outcomes$resources___2) #N = 14
table(df_outcomes$resources___3) #N = 95 -> 96
table(df_outcomes$resources___4) #N = 71 -> 72
table(df_outcomes$resources___5) #N = 54 -> 55 -> 56
table(df_outcomes$resources___6) #N = 44
table(df_outcomes$resources___7) #N = 37
table(df_outcomes$resources___8) #N = 26
table(df_outcomes$resources___9) #N = 3
table(df_outcomes$resources___10) #N = 85 -> 86 -> 87 -> 88
table(df_outcomes$resources___11) #N = 78 -> 79
table(df_outcomes$resources_ADL) # N = 0 -> 1
table(df_outcomes$resources_other) # N = 0 -> 1

## create social needs measure (range is 0-14)
df_outcomes <- df_outcomes %>%
  mutate(social_needs = resources___1 + resources___2 + resources___3 + resources___4 
         + resources___5 + resources___6 + resources___7 + resources___8 + resources___9 
         + resources___10 + resources___11 + resources___12 + resources_ADL + resources_other)

#check variable
table(df_outcomes$social_needs, useNA = "always")

### Self-efficacy (sum of items: cc1-cc6)
# check one of the variables
table(df_outcomes$cc1, useNA = "always") # range 1-10
table(df_outcomes$cc2, useNA = "always") # range 1-10

## create self efficacy measure (range is 6-60)
df_outcomes <- df_outcomes %>%
  mutate(self_efficacy = cc1 + cc2 + cc3 + cc4 + cc5 + cc6 )

#check variable
table(df_outcomes$self_efficacy, useNA = "always")

### Pain interference (sum of items: pain1- pain6)
# check one of the variables
table(df_outcomes$pain1, useNA = "always") # range 1-5
table(df_outcomes$pain2, useNA = "always") # range 1-5

## create pain interference measure (range is 6-30)
df_outcomes <- df_outcomes %>%
  mutate(pain_interference = pain1 + pain2 + pain3 + pain4 + pain5 + pain6)

#check variable
table(df_outcomes$pain_interference, useNA = "always")

### Diabetes symptoms (sum of items: dia1- dia7)
# check one of the variables
table(df_outcomes$dia1, useNA = "always") # range 0-2
table(df_outcomes$dia2, useNA = "always") # range 0-2

## create diabetes measure (range is 0-14)
df_outcomes <- df_outcomes %>%
  mutate(diabetes = dia1 + dia2 + dia3 + dia4 + dia5 + dia6 + dia7)

#check variable
table(df_outcomes$diabetes, useNA = "always")

## create clean pre/post variable
df_outcomes <- df_outcomes %>%
  mutate(pre_post = 
           case_when(str_detect(redcap_event_name, "pre") ~ "pre",
                     str_detect(redcap_event_name, "post") ~ "post"))


table(df_outcomes$pre_post) # Pre N = 605, post N = 285

df_outcomes$pre_post <- 
  factor(df_outcomes$pre_post, levels=c("pre", "post"),
         labels=c("Pre", 
                  "Post"))


table1( ~ loneliness + soc_isolation + anxiety + depression +
           phys_activity + social_needs + self_efficacy + pain_interference + diabetes| redcap_event_name, data = df_outcomes)

table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + social_needs + self_efficacy + pain_interference + diabetes| pre_post, data = df_outcomes)

## Q10- Which program are you currently taking?
df_outcomes$q10 <- 
  factor(df_outcomes$q10, levels=c(1, 2, 3, 4, 5, 6, 7),
         labels=c("CDSMP", 
                  "CPSMP",
                  "DSMP", 
                  "EnhanceFitness",
                  "Healthy IDEAS",
                  "HomeMeds",
                  "Walk with Ease"))

table(df_outcomes$q10)

table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + social_needs + self_efficacy + pain_interference + diabetes| q10, data = df_outcomes)


table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + social_needs + self_efficacy + pain_interference + diabetes| pre_post + q10, data = df_outcomes)
 
table(df_outcomes$pre_post, df_outcomes$q10)

## filter out pre/post folks
df <- df_outcomes %>% group_by(record_id) %>% filter(n() == 2)

df <- df %>% group_by(record_id) %>%
  mutate(across(q10, ~coalesce(.x, first(.x))))

df_outcomes <- df_outcomes %>% group_by(record_id) %>%
  mutate(across(q10, ~coalesce(.x, first(.x))))

table(df$pre_post, df$q10)

table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + social_needs + self_efficacy + pain_interference + diabetes| q10 + pre_post, data = df)


table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + social_needs + self_efficacy + pain_interference + diabetes| q10 + pre_post, data = df)

# copy of data to refer back to)
write.csv(df,"df_outcomescheck.csv", row.names = FALSE)
