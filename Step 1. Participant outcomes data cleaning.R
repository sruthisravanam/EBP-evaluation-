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
pacman::p_load(readr, dplyr, tidyr, lubridate, table1, stringr, ggplot2)

# read in data
df_outcomes <- read_csv("RemoteEBPCLEANPartic_DATA_2022-11-30_1445.csv") ## 1907 obs. 205 var. (downloaded data 12/2) 

# Create a copy of data for testing
df_test <- df_outcomes

#copying NA of demographics and 
df_outcomes<-df_outcomes %>%
  group_by(record_id) %>% 
  fill(zip:q11_other, .direction = "down")
#copying rural info from datamanagment rows to pre and post
df_outcomes<-df_outcomes %>%
  group_by(record_id) %>% 
  fill(cleaned_rural, .direction = "up")

##filter out pre/post arm for summary scales and descriptives
df_outcomes <- df_outcomes %>%
  filter(str_detect(redcap_event_name, "pre|post")) # N = 908

## filter out Q10- Which program are you currently taking? == 5 (Healthy IDEAS) and ==6 (HomeMeds)
table(df_outcomes$q10) # filter out 5 (N=12) and 6 (N=29)

df_outcomes <- df_outcomes %>%
  filter(q10 != "5") # N = 888

df_outcomes <- df_outcomes %>%
  filter(q10 != "6") # N = 875

## Filter out missing data N = 15 missing all outcome data
df_missing <- df_outcomes %>% 
  filter(is.na(health)) %>% 
  filter(is.na(fatigue)) %>%
  filter(is.na(pain)) %>%
  filter(is.na(sleep)) %>%
  filter(is.na(sc1)) %>%
  filter(is.na(sc2)) %>%
  filter(is.na(sc3)) %>%
  filter(is.na(sc4)) %>%
  filter(is.na(sc5)) %>%
  filter(is.na(sc6)) %>%
  filter(is.na(sc7)) %>%
  filter(is.na(sc8)) %>%
  filter(is.na(emo1)) %>%
  filter(is.na(emo2)) %>%
  filter(is.na(emo3)) %>%
  filter(is.na(emo4)) %>%
  filter(is.na(emo5)) %>%
  filter(is.na(emo6)) %>%
  filter(is.na(emo7)) %>%
  filter(is.na(emo8)) %>%
  filter(is.na(emo9)) %>%
  filter(is.na(emo10)) %>%
  filter(is.na(emo11)) %>%
  filter(is.na(pa1)) %>%
  filter(is.na(pa2)) %>%
  filter(is.na(cc1)) %>%
  filter(is.na(cc2)) %>%
  filter(is.na(cc3)) %>%
  filter(is.na(cc4)) %>%
  filter(is.na(cc5)) %>%
  filter(is.na(cc6)) %>%
  filter(is.na(pain1)) %>%
  filter(is.na(pain2)) %>%
  filter(is.na(pain3)) %>%
  filter(is.na(pain4)) %>%
  filter(is.na(pain5)) %>%
  filter(is.na(pain6)) # N = 15 with all outcome data missing

## Filter out missing 
# df_outcomes <- df_outcomes %>%
#   filter(record_id != "1026" & redcap_event_name == "pre_arm_1",)
# 
# df_outcomes <- df_outcomes %>%
#   filter(record_id != "1088" & redcap_event_name == "pre_arm_1",)
# 
# df_outcomes <- df_outcomes %>%
#   filter(record_id != "1128" & redcap_event_name == "post_arm_1",)
# 
# df_outcomes <- df_outcomes %>%
#   filter(record_id != "1129" & redcap_event_name == "pre_arm_1",)
# # 
# 
#            record_id != "1176"&
#            record_id != "1245"&
#            record_id != "1284"&
#            record_id != "1320"&
#            record_id != "1326"&
#            record_id != "1427"&
#            record_id != "1486"&
#            record_id != "3039"&
#            record_id != "3048"&
#            record_id != "4069"&
#            record_id != "5002")


### labels for program
df_outcomes$q10 <- 
  factor(df_outcomes$q10, levels=c(1, 2, 3, 4, 7),
         labels=c("CDSMP", 
                  "CPSMP",
                  "DSMP", 
                  "EnhanceFitness",
                  "Walk with Ease"))


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
#write.csv(df_outcomes,"cleaned_all.csv", row.names = FALSE)

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
  mutate(self_efficacy = (cc1 + cc2 + cc3 + cc4 + cc5 + cc6)/6 )

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

df_outcomes$pre_post <- 
  factor(df_outcomes$pre_post, levels=c("pre", "post"),
         labels=c("Pre", 
                  "Post"))


table(df_outcomes$pre_post) # Pre N = 605, post N = 285

## create enrolled/completers variable
df_outcomes <- df_outcomes %>% group_by(record_id) %>%
  mutate(count=n())

# check variable
table(df_outcomes$count)

df_outcomes <- df_outcomes %>% group_by(record_id) %>% 
  mutate(completers = 
           ifelse(count >1, "Completers", "Enrollees"))


df_outcomes$completers <- 
  factor(df_outcomes$completers, levels=c("Enrollees","Completers"),
         labels=c("Enrollees", 
                  "Completers"))
         
# check variable
table(df_outcomes$completers)

## copy program to post surveys
df_outcomes <- df_outcomes %>% group_by(record_id) %>%
  mutate(across(q10, ~coalesce(.x, first(.x))))

table(df_outcomes$pre_post, df_outcomes$q10)

table(df_outcomes$pre_post, df_outcomes$q10)

table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + social_needs + self_efficacy + pain_interference + diabetes +
          health + fatigue + pain + sleep| q10 + pre_post, data = df_outcomes)


### Tables ##### 
table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + pa1 + pa2 + social_needs + self_efficacy + pain_interference + diabetes +
          health + fatigue + pain + sleep + tech3 +tech4 | q10 + completers, data = df_outcomes)


table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + pa1 + pa2 + social_needs + self_efficacy + pain_interference + diabetes + 
          health + fatigue + pain + sleep | completers + pre_post, data = df_outcomes)


table1( ~ loneliness + soc_isolation + anxiety + depression +
          phys_activity + pa1 + pa2 + social_needs + self_efficacy + pain_interference + diabetes +
          health + fatigue + pain + sleep| q10, data = df_outcomes)


### Individual measures

#### self-rated health
table(df_outcomes$health, useNA = "always")

## bar plot for health
ggplot(df_outcomes, aes(x = health)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Fatigue
table(df_outcomes$fatigue, useNA = "always")

ggplot(df_outcomes, aes(x = fatigue)) +
  scale_x_continuous(breaks = c(1:10)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Pain
table(df_outcomes$pain, useNA = "always")

ggplot(df_outcomes, aes(x = pain)) +
  scale_x_continuous(breaks = c(1:10)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Sleep
table(df_outcomes$sleep, useNA = "always")

ggplot(df_outcomes, aes(x = sleep)) +
  scale_x_continuous(breaks = c(1:10)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

### Loneliness distribution

ggplot(df_outcomes, aes(x = loneliness)) +
  scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Social isolation
table(df_outcomes$soc_isolation, useNA = "always")

ggplot(df_outcomes, aes(x = soc_isolation)) +
  scale_x_continuous(breaks = c(5:25)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()
  
#### Anxiety
table(df_outcomes$anxiety, useNA = "always")

ggplot(df_outcomes, aes(x = anxiety)) +
  scale_x_continuous(breaks = c(0:6)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Depression
table(df_outcomes$depression, useNA = "always")

ggplot(df_outcomes, aes(x = depression)) +
  scale_x_continuous(breaks = c(0:24)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Physical activity
table(df_outcomes$phys_activity, useNA = "always")

ggplot(df_outcomes, aes(x = phys_activity)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Social needs
table(df_outcomes$social_needs, useNA = "always")

ggplot(df_outcomes, aes(x = social_needs)) +
  scale_x_continuous(breaks = c(0:11)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Self-efficacy
table(df_outcomes$self_efficacy, useNA = "always")

ggplot(df_outcomes, aes(x = self_efficacy)) +
  scale_x_continuous(breaks = c(0:10)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Pain interference
table(df_outcomes$pain_interference, useNA = "always")

ggplot(df_outcomes, aes(x = pain_interference)) +
  scale_x_continuous(breaks = c(6:30)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

#### Diabetes symptoms
table(df_outcomes$diabetes, useNA = "always")

ggplot(df_outcomes, aes(x = diabetes)) +
  scale_x_continuous(breaks = c(0:11)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

### Table of health, fatigue, pain, sleep
table1( ~ health +fatigue + pain + sleep | pre_post, data = df_outcomes)

### Physical activity separate
table(df_outcomes$pa1, useNA = "always")

label(df_outcomes$pa1) <- "Number of days aerobic activity reported"

ggplot(df_outcomes, aes(x = pa1)) +
  xlab("Number of days aerobic activity reported") +
  scale_x_continuous(breaks = c(0:11)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

## Pa 2
table(df_outcomes$pa2_recoded, useNA = "always")

label(df_outcomes$pa2) <- "Number of minutes aerobic activity reported"

ggplot(df_outcomes, aes(x = pa2)) +
  xlab("Number of minutes aerobic activity reported") +
  scale_x_continuous(breaks = c(0:90)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_bar()

### Table of pa 1 and pa2
table1( ~ pa1 + pa2 | pre_post, data = df_outcomes)

## 
table(df_outcomes$depression, useNA = "always")

## look at just post data
df_post <- df_outcomes %>%
  filter(str_detect(redcap_event_name, "post")) # N =289 but a lot of these look missing

df_pre <- df_outcomes %>%
  filter(str_detect(redcap_event_name, "pre")) # N = 586

# copy of data to refer back to)
write.csv(df_pre,"pre_only.csv", row.names = FALSE)
write.csv(df_post,"post_only.csv", row.names = FALSE)

## Depression higher than 6

df_completers <- df_outcomes %>%
  filter(completers == "Completers")

## filter out anyone who reported >6 in their pre survey
df_depression6 <- df_completers %>%
  filter(pre_post == "Pre" & depression > 6)


### merge data
df_depression6 <- rbind(df_depression6, df_post)


df_depression6 <- df_completers %>%
  filter(record_id %in% df_depression6$record_id)



df_depression10 <- df_outcomes %>%
  filter(pre_post == "Pre" & depression > 10)

table1( ~ depression | pre_post, data = df_depression10)

### Change in pain
## number of people that changed/improved in 1 point and 2 points or more 

df_completers <- df_completers %>%
  mutate(painchange == df_completers$pain[df_completers$pre_post=="Pre"] - df_completers$pain[df_completers$pre_post=="Post"])


df_completers$painchange <- df_completers$pain[df_completers$pre_post=="Pre"] - df_completers$pain[df_completers$pre_post=="Post"]


# copy of data to refer back to)
write.csv(df_depression6,"phq9_6_data.csv", row.names = FALSE)
write.csv(df_depression10,"phq9_10_data.csv", row.names = FALSE)
write.csv(df_outcomes,"cleaned_data.csv", row.names = FALSE)

