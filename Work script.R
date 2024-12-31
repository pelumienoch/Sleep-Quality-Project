#### first we load the packages to use

pacman::p_load(
  gtsummary,
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)

pacman::p_load(flextable)

### we import our data

here()
sleep <- import(here("Miracle Christian data.xlsx"), which = "Sheet3")


### now, lets view the dataset
head(sleep)  # this shows only the first 6 rows by default
head(sleep, 10)   # this shows the firt 10 rows

### lets skim the data, that is, to provide a summary of the dataset
pacman::p_load(skimr)

skimr::skim(sleep)

### lets check the names of the column
names(sleep)

### The column nmaes are not consistent, hence we will autocorrect

# pipe the raw dataset through the function clean_names(), assign result as "clean_sleep"  
clean_sleep<- sleep %>% 
  janitor::clean_names()

# see the new column names
names(clean_sleep)


### we can as well do a manual cleaning, see the code below
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
clean_sleep2 <- clean_sleep %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(SN              = x1,
         how_often_do_you_experience_the_following_symptoms_headache =
           how_often_do_you_experience_the_following_symptoms_1_never_2_rarely_3_sometimes_4_often_5_always, 
         fatigues =      x20,
         Mood_swing =    x21,
         Difficulty_concentrating =  x22,
         Anxiety_and_stress =    x23,
         Depression =      x24,
         job_performance_is_affected_by_poor_sleep_quality = how_often_do_you_experience_the_following_1_never_2_rarely_3_sometimes_4_often_5_always, 
         Have_you_made_any_errors_at_work_due_to_lack_of_sleep = x27,
         feel_less_productive_at_work_because_of_poor_sleep = x28,
         experience_conflicts_with_colleagues_or_supervisors_due_to_irritability_or_moodiness_from_poor_sleep =
            x29,
         factors_contributing_to_your_poor_sleep_quality_1 = which_of_the_following_factors_do_you_believe_contribute_to_your_poor_sleep_quality_30,
         factors_contributing_to_your_poor_sleep_quality_2 = which_of_the_following_factors_do_you_believe_contribute_to_your_poor_sleep_quality_31,
         factors_contributing_to_your_poor_sleep_quality_3 = x32,
         factors_contributing_to_your_poor_sleep_quality_4 = x33,
         factors_contributing_to_your_poor_sleep_quality_5 = x34,
         factors_contributing_to_your_poor_sleep_quality_6 = x35)
names(clean_sleep2)

skim(clean_sleep2)

#### we need to convert some variable classes for us perform analysis
class(clean_sleep2)

### Transform multiple columns
## Here the transformation as.factor() is applied to specific columns named within across()

clean_sleep3 <- clean_sleep2 %>% 
  mutate(across(.cols = -c(gender, highest_educational_qualification, occupation, 
                           years_of_experience_in_healthcare,
                           factors_contributing_to_your_poor_sleep_quality_1), .fns = as.factor))
skim(clean_sleep3) ### to see if it changed the class of the variables apart from the ones i excluded using '-c'

###yeeppy!!! it worked
### Now, lets recode the variables

#### To recode multiple columns

# Recode values in col1 and col3
clean_sleep4 <- clean_sleep3 %>%
  mutate(
    age_yrs = recode(age_yrs, "1" = "18-25years", "2" = "26-35years", "3" = "36-45years",
                    "4" = "46-55years", "5" = "56above"),
    marital_status = recode(marital_status, "1" = "Single", "2" = "Married", "3" = "Divorced", 
                            "4" = "Widowed", "5" = "Cohabiting"),
    type_of_healthcare_facility = recode(type_of_healthcare_facility, "1" = "Primary Health Care Center"
                                         , "2" = "Private Hospital", "3" = "General Hospital"),
    type_of_shift = recode(type_of_shift, "1" = "Day shift", "2" = "Evening shift", "3" = "Night Shift",
                           "4" = "Rotating Shifts"),
    do_you_work_night_shift = recode(do_you_work_night_shift, "1" = "Yes", "2" = "No"),
    
    how_many_nights_do_you_work_per_week = recode(how_many_nights_do_you_work_per_week, "1" = "1-2",
                                                  "2" = "3-4", "3" = "5-6", "4" = ">7"),
    
  )

class(clean_sleep4$do_you_work_night_shift)


#### To recode multiple columns with the same values at once

# Recode 1 to "A" and 2 to "B" in multiple columns
clean_sleep4 <- clean_sleep4 %>%
  mutate(across(c(during_the_past_month_how_would_you_rate_your_overall_sleep_quality, 
                  during_the_past_month_how_long_in_minutes_has_it_usually_taken_you_to_fall_asleep_after_your_work_shift, 
                  during_the_past_month_how_many_hours_of_sleep_did_you_get_on_average_between_your_work_shifts,
                  during_the_past_month_how_often_did_you_wake_up_in_the_middle_of_your_sleep_or_early_morning_and_had_trouble_getting_back_to_sleep,
                  during_the_past_month_how_often_did_you_experience_sleep_disturbances_such_as_noise_light_or_uncomfortable_room_temperature_while_sleeping,
                  during_the_past_month_how_often_did_you_take_medicine_prescribed_or_over_the_counter_to_help_you_sleep,
                  during_the_past_month_how_often_did_you_feel_sleepy_or_had_trouble_staying_awake_during_your_work_shifts_or_while_commuting_home), ~ recode(.x, "1" = "0", "2" = "1", "3" = "2", "4" = "3")))


clean_sleep4 <- clean_sleep4 %>%
  mutate(across(c(how_often_do_you_experience_the_following_symptoms_headache, 
                  fatigues, 
                  Mood_swing,
                  Difficulty_concentrating,
                  Anxiety_and_stress,
                  Depression, job_performance_is_affected_by_poor_sleep_quality,
                  Have_you_made_any_errors_at_work_due_to_lack_of_sleep,
                  feel_less_productive_at_work_because_of_poor_sleep,
                  experience_conflicts_with_colleagues_or_supervisors_due_to_irritability_or_moodiness_from_poor_sleep
                  ), ~ recode(.x, "1" = "Never", "2" = "Rarely", "3" = "Sometimes", 
                                  "4" = "Often", "5" = "Always")))

clean_sleep4 <- clean_sleep4 %>%
  mutate(have_you_been_diagnosed_with_any_chronic_health_conditions = recode(
    have_you_been_diagnosed_with_any_chronic_health_conditions, "1" = "Yes", "2" = "No"
  ))

### we need to clean the occupation variable
table(clean_sleep4$occupation, useNA = "always")  # print table of all unique values, including missing
### looks so untidy
clean_sleep4 <- clean_sleep4 %>%
  mutate(occupation = recode(occupation, "1" = "Doctor", "2" = "Nurse", "3" = "Laboratory technichian",
                     "4" = "others"))
table(clean_sleep4$occupation, useNA = "always")  # print table of all unique values, including missing

clean_sleep4 <- clean_sleep4 %>% 
  mutate(occupation = recode(occupation,
                           # for reference: OLD = NEW
                           "4(ACCOUNTS)"  = "Accounts",
                           "4(CASHIER)" = "Accounts",
                           "4(ADMIN)"  = "Admin",
                           "4(CLEANER)" = "Cleaner",
                           "4(PHAMARCY)"      = "Pharmacy",
                           "4(PHARMACY)"   = "Pharmacy",
                           "6"        =  "others",
                           "4(SECURITY)"   = "Security",
                           "4(HEALTH RECORD)"    = "Health record",
                           "4(HRO)"           = "Health record",
                           "4(RECORDS)"      = "Health record",
                           "4(NURSE AID)"   = "Nurse",
                           
                          
  ))

table(clean_sleep4$occupation, useNA = "always")  # print table of all unique values, including missing
class (clean_sleep4$occupation) ### lets check the class of our occupation
skim(clean_sleep4) ### lets preview by skimming the dataset
clean_sleep4$years_of_experience_in_healthcare <- as.numeric(clean_sleep4$years_of_experience_in_healthcare) ### i want to convert the class to numeric
class(clean_sleep4$years_of_experience_in_healthcare) ### to confirm if the class has really changed
clean_sleep4$highest_educational_qualification <- as.factor(clean_sleep4$highest_educational_qualification)

clean_sleep4 <- clean_sleep4 %>%
  mutate(highest_educational_qualification = recode(highest_educational_qualification, 
                                                    "1" = "Secondary school", "2" = "Diploma",
                                                    "3" = "Bachelor's Degree", "4" = "Master's Degree",
                                                    "5" = "PhD"))

clean_sleep4$factors_contributing_to_your_poor_sleep_quality_1 <- as.factor(clean_sleep4$factors_contributing_to_your_poor_sleep_quality_1)
clean_sleep4 <- clean_sleep4 %>%
  mutate(across(c(factors_contributing_to_your_poor_sleep_quality_2, factors_contributing_to_your_poor_sleep_quality_1,
                   factors_contributing_to_your_poor_sleep_quality_3, factors_contributing_to_your_poor_sleep_quality_4,
                   factors_contributing_to_your_poor_sleep_quality_5, factors_contributing_to_your_poor_sleep_quality_6), ~ recode(.x, "1" = "Work schedule", "2" = "Work environment (e.g., noise, lighting)",
                              "3" = "Family responsibilities", "4" = "Stress",
                              "5" = "Personal health issues", "6" = "Lifestyle habits (e.g., caffeine, alcohol consumption)")))
clean_sleep5 <- clean_sleep4 %>%
  select(-c(factors_contributing_to_your_poor_sleep_quality_1))