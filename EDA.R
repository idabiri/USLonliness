# Title:
# Author: Iyanu Dabiri
# Research Questions:

# How do differences in social connection and emotional support help explain
# variation in loneliness across gender, racial/ethnic, and regional groups
# in the U.S.?
# How does the frequency of loneliness differ across gender groups?
# How does the frequency of loneliness differ across racial and ethnic groups?
# How does the frequency of loneliness differ across U.S. regions?
# Is the number of close friends a person has associated with how frequently they experience loneliness?
# Is more frequent in-person contact with close friends associated with less loneliness?


# ------------ File & Library Import ------------ 
install.packages("survey")
library(readxl)
library(dplyr)
library(survey) # for the weights used for Race
library(ggplot2)
atp <- read.csv("ATP W154.csv")


# ------------ Data Cleaning ------------

# Subset Dataset to Include Needed (Renamed) Variables for Analysis
atp_final <- atp |> 
  select(F_AGECAT, F_RACETHNMOD, F_GENDER, F_CREGION, WEIGHT_W154, FEELMOD_a_W154, 
         CLOSEFRI_W154, COMMFRI_c_W154, EMOSUPPFAM_a_W154, EMOSUPPFAM_b_W154, EMOSUPPFAM_c_W154, 
         EMOSUPPFAM_d_W154, EMOSUPPOTH_a_W154, EMOSUPPOTH_c_W154) |> 
  rename(FEEL_LONELY = FEELMOD_a_W154,
         CLOSE_FRIEND = CLOSEFRI_W154,
         COMMFRI_IRL = COMMFRI_c_W154,
         EMOSUPP_SPOUSE = EMOSUPPFAM_a_W154,
         EMOSUPP_MOM = EMOSUPPFAM_b_W154,
         EMOSUPP_DAD = EMOSUPPFAM_c_W154,
         EMOSUPP_OTHFAM = EMOSUPPFAM_d_W154,
         EMOSUPP_FRIEND = EMOSUPPOTH_a_W154,
         EMOSUPP_PROF = EMOSUPPOTH_c_W154) |> 
  glimpse()


# Recode 99s to NA (???)
atp_final <- atp_final |> 
  mutate(across(everything(), ~ na_if(.x, 99)))


#Rename variables
atp_final <- atp_final |> 
  mutate(
    F_AGECAT = factor(F_AGECAT, levels = c(1, 2, 3, 4), 
                      labels = c("18-29", "30-49", "50-64", "65+")),
    
    F_RACETHNMOD = factor(F_RACETHNMOD, levels = c(1, 2, 3, 4, 5), 
                          labels = c("White", "Black", "Hispanic", "Other", "Asian")),
    
    F_GENDER = factor(F_GENDER, levels = c(1, 2, 3), 
                      labels = c("Man", "Woman", "Other")),
    
    F_CREGION = factor(F_CREGION, levels = c(1, 2, 3, 4), 
                       labels = c("Northeast", "Midwest", "South", "West"))
  ) |> 
  glimpse()

# Making FEEL_LONELY into a binary variable
atp_final <- atp_final |> 
  mutate(
    IS_LONELY = if_else(FEEL_LONELY != 1 & FEEL_LONELY != 2, 0, 1, missing = NA)
  )

# Import the weights that will be used throughout analysis
weight <- svydesign(ids = ~1, data = atp_final, weights = ~WEIGHT_W154)


# ------------ EDA Frequency Tables ------------

# Check frequencies of raw variables and NAs
table(atp_final$F_AGECAT, useNA = "always")
table(atp_final$F_RACETHNMOD, useNA = "always")
table(atp_final$F_GENDER, useNA = "always")
table(atp_final$F_CREGION, useNA = "always")
table(atp_final$FEEL_LONELY, useNA = "always")
table(atp_final$CLOSE_FRIEND, useNA = "always") 
table(atp_final$COMMFRI_IRL, useNA = "always")
table(atp_final$EMOSUPP_SPOUSE, useNA = "always")
table(atp_final$EMOSUPP_MOM, useNA = "always") 
table(atp_final$EMOSUPP_DAD, useNA = "always")
table(atp_final$EMOSUPP_OTHFAM, useNA = "always")
table(atp_final$EMOSUPP_FRIEND, useNA = "always")
table(atp_final$EMOSUPP_PROF, useNA = "always")


# Check frequencies with weights
svytable(~F_AGECAT, design = weight)
svytable(~F_RACETHNMOD, design = weight)
svytable(~F_GENDER, design = weight)
svytable(~F_CREGION, design = weight)
svytable(~FEEL_LONELY, design = weight)
svytable(~CLOSE_FRIEND, design = weight)
svytable(~COMMFRI_IRL, design = weight)
svytable(~EMOSUPP_SPOUSE, design = weight)
svytable(~EMOSUPP_MOM, design = weight)
svytable(~EMOSUPP_DAD, design = weight)
svytable(~EMOSUPP_OTHFAM, design = weight)
svytable(~EMOSUPP_FRIEND, design = weight)
svytable(~EMOSUPP_PROF, design = weight)


# ------------ EDA Contingency Tables ------------

# Check weighted loneliness by demographics
round(svytable(~IS_LONELY + F_AGECAT, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~IS_LONELY + F_RACETHNMOD, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~IS_LONELY + F_GENDER, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~IS_LONELY + F_CREGION, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Close friends by lonliness
round(svytable(~CLOSE_FRIEND + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# In-person contact by lonliness
round(svytable(~COMMFRI_IRL + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)

# Support from professional by demographics
round(svytable(~EMOSUPP_PROF + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_FRIEND + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_MOM + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_DAD + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_OTHFAM + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)
round(svytable(~EMOSUPP_SPOUSE + IS_LONELY, design = weight) |> prop.table(margin = 2) * 100, digits = 1)


# ------------ Descriptive Visualizations: Outcome ------------

# Loneliness Bar Charts
lonely_total <- svytable(~FEEL_LONELY, design = weight) |> 
  as.data.frame()

print(lonely_total)


lonely_total |> 
  ggplot(aes(x = factor(FEEL_LONELY), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Loneliness Score", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Loneliness...") + 
  theme_minimal() +
  scale_x_discrete(
    labels = 
      c("All of the time", "Most of the Time", "Sometimes", "Hardly Ever", "Never")
  )



# ------------ Descriptive Visualizations: Grouping ------------

# Gender Bar Charts
gender_total <- svytable(~F_GENDER, design = weight) |> 
  as.data.frame()

print(gender_total)


gender_total |> 
  ggplot(aes(x = factor(F_GENDER), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Gender", 
    y = "Number of Adults in each Gender (Weighted)", 
    title = "U.S. Adults...") + 
  theme_minimal()


# Region Bar Charts
region_total <- svytable(~F_CREGION, design = weight) |> 
  as.data.frame()

print(region_total)


region_total |> 
  ggplot(aes(x = factor(F_CREGION), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "U.S Census Region", 
    y = "Number of Adults in each Region (Weighted)", 
    title = "U.S. Adults...") + 
  theme_minimal()

# Race Bar Charts
race_total <- svytable(~F_RACETHNMOD, design = weight) |> 
  as.data.frame()

print(race_total)


race_total |> 
  ggplot(aes(x = factor(F_RACETHNMOD), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Race-Ethnicity", 
    y = "Number of Adults in each Race (Weighted)", 
    title = "U.S. Adults...") + 
  theme_minimal()

# Age Bar Charts
age_total <- svytable(~F_AGECAT, design = weight) |> 
  as.data.frame()

print(age_total)


age_total |> 
  ggplot(aes(x = factor(F_AGECAT), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Age", 
    y = "Number of Adults in each Age Category (Weighted)", 
    title = "U.S. Adults...") + 
  theme_minimal()



# ------------ Descriptive Visualizations: Independents ------------

# Close Friends Bar Chart
closefriends_total <- svytable(~CLOSE_FRIEND, design = weight) |> 
  as.data.frame()

print(closefriends_total)


closefriends_total |> 
  ggplot(aes(x = factor(CLOSE_FRIEND), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Close Friends", 
    y = "Number of Close Friends (Weighted)", 
    title = "U.S. Adults...") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("1", ">1", "None"))


# In-person Contact with Close Friends Bar Chart
friendIRL_total <- svytable(~COMMFRI_IRL, design = weight) |> 
  as.data.frame()

print(friendIRL_total)


friendIRL_total |> 
  ggplot(aes(x = factor(COMMFRI_IRL), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "In-person Contact with Close Friends", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults...") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("At Least Once a Day", "A Few Times a Week", "A Few Times a Month",
               "Once a Month", "Less than a Month", "Never"))

# Spouse Emotional Support Bar Chart
spouse_total <- svytable(~EMOSUPP_SPOUSE, design = weight) |> 
  as.data.frame()

print(spouse_total)


spouse_total |> 
  ggplot(aes(x = factor(EMOSUPP_SPOUSE), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Likliness", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Who Would Turn to Spouse for Emotional Support") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("Extremely Likely", "Very Likely", "Somewhat Likely",
               "Not too Likely", "Not at all Likely", "Does Not Apply"))


# Mom Emotional Support Bar Chart
mom_total <- svytable(~EMOSUPP_MOM, design = weight) |> 
  as.data.frame()

print(mom_total)


mom_total |> 
  ggplot(aes(x = factor(EMOSUPP_MOM), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Likliness", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Who Would Turn to Mother for Emotional Support") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("Extremely Likely", "Very Likely", "Somewhat Likely",
               "Not too Likely", "Not at all Likely", "Does Not Apply"))


# Dad Emotional Support Bar Chart
dad_total <- svytable(~EMOSUPP_DAD, design = weight) |> 
  as.data.frame()

print(dad_total)


dad_total |> 
  ggplot(aes(x = factor(EMOSUPP_DAD), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Likliness", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Who Would Turn to Father for Emotional Support") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("Extremely Likely", "Very Likely", "Somewhat Likely",
               "Not too Likely", "Not at all Likely", "Does Not Apply"))


# Other Family Emotional Support Bar Chart
othfam_total <- svytable(~EMOSUPP_OTHFAM, design = weight) |> 
  as.data.frame()

print(othfam_total)


othfam_total |> 
  ggplot(aes(x = factor(EMOSUPP_OTHFAM), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Likliness", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Who Would Turn to Other Family Members for Emotional Support") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("Extremely Likely", "Very Likely", "Somewhat Likely",
               "Not too Likely", "Not at all Likely", "Does Not Apply"))


# Friend Emotional Support Bar Chart
friend_total <- svytable(~EMOSUPP_FRIEND, design = weight) |> 
  as.data.frame()

print(friend_total)


friend_total |> 
  ggplot(aes(x = factor(EMOSUPP_FRIEND), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Likliness", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Who Would Turn to Friends for Emotional Support") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("Extremely Likely", "Very Likely", "Somewhat Likely",
               "Not too Likely", "Not at all Likely", "Does Not Apply"))


# Professional Support Emotional Support Bar Chart
prof_total <- svytable(~EMOSUPP_PROF, design = weight) |> 
  as.data.frame()

print(prof_total)


prof_total |> 
  ggplot(aes(x = factor(EMOSUPP_PROF), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Freq)), vjust = -1) +
  labs(
    x = "Likliness", 
    y = "Number of Adults (Weighted)", 
    title = "U.S. Adults Who Would Turn to a Mental Health Professional for Emotional Support") + 
  theme_minimal() +
  scale_x_discrete(
    labels = c("Extremely Likely", "Very Likely", "Somewhat Likely",
               "Not too Likely", "Not at all Likely", "Does Not Apply"))



# ------------ Descriptive Visualizations: Loneliness x Gender ------------

# Side by Side Bar Chart
lonely_gender <-
  round(svytable(~IS_LONELY + F_GENDER, design = weight) |>
          prop.table(margin = 2) * 100, digits = 1) |> as.data.frame() |>
  rename(percent = Freq)

print(lonely_gender)


lonely_gender |> 
  ggplot(aes(x = factor(F_GENDER), y = percent, fill = IS_LONELY)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percent, digits = 1)), 
            position = position_dodge(width = 1), vjust = -1) +
  labs(
    x = "Gender", 
    y = "Percentage", 
    title = "Loneliness by Gender",
    fill = "Is Lonely") +
  theme_minimal()

# Chi-Square to Check Association
chisq.test(svytable(~IS_LONELY + F_GENDER, design = weight)) # significant



# ------------ Descriptive Visualizations: Loneliness x Age ------------

# Side by Side Bar Chart
lonely_age <-
  round(svytable(~IS_LONELY + F_AGECAT, design = weight) |>
          prop.table(margin = 2) * 100, digits = 1) |> as.data.frame() |>
  rename(percent = Freq)

print(lonely_age)


lonely_age |> 
  ggplot(aes(x = factor(F_AGECAT), y = percent, fill = IS_LONELY)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percent, digits = 1)), 
            position = position_dodge(width = 1), vjust = -1) +
  labs(
    x = "Age", 
    y = "Percentage", 
    title = "Loneliness by Age",
    fill = "Is Lonely") +
  theme_minimal()

#Chi-Square Test to Check Association
chisq.test(svytable(~IS_LONELY + F_AGECAT, design = weight)) # significant



# ------------ Descriptive Visualizations: Loneliness x Race/Ethnicity ------------

# Side by Side Bar Chart
lonely_race <-
  round(svytable(~IS_LONELY + F_RACETHNMOD, design = weight) |>
          prop.table(margin = 2) * 100, digits = 1) |> as.data.frame() |>
  rename(percent = Freq)

print(lonely_race)


lonely_race |> 
  ggplot(aes(x = factor(F_RACETHNMOD), y = percent, fill = IS_LONELY)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percent, digits = 1)), 
            position = position_dodge(width = 1), vjust = -1) +
  labs(
    x = "Race/Ethnicity", 
    y = "Percentage", 
    title = "Loneliness by Race/Ethnicity",
    fill = "Is Lonely") +
  theme_minimal()

# Chi-square Test to Check Association
chisq.test(svytable(~IS_LONELY + F_RACETHNMOD, design = weight)) # significant



# ------------ Descriptive Visualizations: Loneliness x Region ------------

# Side by Side Bar Chart
lonely_region <-
  round(svytable(~IS_LONELY + F_CREGION, design = weight) |>
          prop.table(margin = 2) * 100, digits = 1) |> as.data.frame() |>
  rename(percent = Freq)

print(lonely_region)


lonely_region|> 
  ggplot(aes(x = factor(F_CREGION), y = percent, fill = IS_LONELY)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percent, digits = 1)), 
            position = position_dodge(width = 1), vjust = -1) +
  labs(
    x = "Cesus Region", 
    y = "Percentage", 
    title = "Loneliness by Region",
    fill = "Is Lonely") +
  theme_minimal()

# Chi-square Test to Check Association
chisq.test(svytable(~IS_LONELY + F_CREGION, design = weight)) # not significant



# ------------ Descriptive Visualizations: Loneliness x Close Friends ------------

# Side by Side Bar Chart
lonely_friend <-
  round(svytable(~IS_LONELY + CLOSE_FRIEND, design = weight) |>
          prop.table(margin = 2) * 100, digits = 1) |> as.data.frame() |>
  rename(percent = Freq)

print(lonely_friend)


lonely_friend|> 
  ggplot(aes(x = factor(CLOSE_FRIEND), y = percent, fill = IS_LONELY)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percent, digits = 1)), 
            position = position_dodge(width = 1), vjust = -1) +
  labs(
    x = "Close Friends", 
    y = "Percentage", 
    title = "Loneliness by Close Friends",
    fill = "Is Lonely") +
  theme_minimal() + 
  scale_x_discrete(
    labels = c("1", ">1", "None")
  )

# Chi-square Test to Check Association
chisq.test(svytable(~IS_LONELY + CLOSE_FRIEND, design = weight)) # significant




# ------------ Descriptive Visualizations: Loneliness x Close Friends IRL ------------

# Side by Side Bar Chart
lonely_irl <-
  round(svytable(~IS_LONELY + COMMFRI_IRL, design = weight) |>
          prop.table(margin = 2) * 100, digits = 1) |> as.data.frame() |>
  rename(percent = Freq)

print(lonely_irl)


lonely_irl|> 
  ggplot(aes(x = factor(COMMFRI_IRL), y = percent, fill = IS_LONELY)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(percent, digits = 1)), 
            position = position_dodge(width = 1), vjust = -1) +
  labs(
    x = "Contact with Close Friends In-person", 
    y = "Percentage", 
    title = "Loneliness by Close Friends In-person",
    fill = "Is Lonely") +
  theme_minimal()  +
  scale_x_discrete(
    labels = c("At Least Once a Day", "A Few Times a Week", "A Few Times a Month",
               "Once a Month", "Less than a Month", "Never"))

# Chi-square Test to Check Association
chisq.test(svytable(~IS_LONELY + COMMFRI_IRL, design = weight)) # significant


# ------------ Normality Checks for Multicollinearity ------------

# Independence of variables
install.packages("car")
library(car)

# Linear regression model
model <- lm(FEEL_LONELY ~ EMOSUPP_MOM + EMOSUPP_DAD + EMOSUPP_OTHFAM, data = atp_final)

# Durbin-Watson test
durbinWatsonTest(model) # low signs of autocorrelation

# VIF for the regression model
vif(model) # low-moderate correlation
