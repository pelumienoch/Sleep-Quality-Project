#### first we load the packages to use

pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)


### we import our data

linelist_raw <- import("linelist_raw.xlsx")

### now, lets view the dataset
head(linelist_raw)  # this shows only the first 6 rows by default
head(linelist_raw, 10)   # this shows the firt 10 rows


### You can use the function skim() from the package skimr to get an overview of
##  the entire dataframe
pacman::p_load(skimr)

skimr::skim(linelist_raw)
`install.packages("BiocManager")
skim(linelist_raw)

### lets check the names of the column
names(linelist_raw)

### The column nmaes are not consistent, hence we will autocorrect

# pipe the raw dataset through the function clean_names(), assign result as "linelist"  
linelist <- linelist_raw %>% 
  janitor::clean_names()

# see the new column names
names(linelist)

### we can as well do a manual cleaning, see the code below
# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)



#### keeping columns 
### Select only the columns you want to remain

# linelist dataset is piped through select() command, and names() prints just the column names
linelist %>% 
  select(case_id, date_onset, date_hospitalisation, fever) %>% 
  names()  # display the column names

#### “tidyselect” helper functions
###  These helper functions exist to make it easy to specify columns to keep, discard, or transform. 

# move date_onset and date_hospitalisation to beginning
linelist %>% 
  select(date_onset, date_hospitalisation, everything()) %>% 
  names()


#### Removing  columns
### ndicate which columns to remove by placing a minus symbol “-” in front of the column name

linelist %>% 
  select(-c(date_onset, fever:vomit)) %>% # remove date_onset and all columns from fever to vomit
  names()

#### Standalone: select() can also be used as an independent command (not in a pipe chain). 
### In this case, the first argument is the original dataframe to be operated upon

# Create a new linelist with id and age-related columns
linelist_age <- select(linelist, case_id, contains("age"))

# display the column names
names(linelist_age)

#### Adding to the pipe chain
###  In the linelist_raw, there are a few columns we do not need: row_num, 
### merged_header, and x28. We remove them with a select() command in the 
### cleaning pipe chain

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  #####################################################

# remove column
select(-c(row_num, merged_header, x28))

### Deduplication
linelist <- linelist %>% 
  distinct()

#### Below, the distinct() command is added to the cleaning pipe chain

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  #####################################################

# de-duplicate
distinct()

#### Column creation and transformation
### The most basic mutate() command to create a new column might look like this.
## It creates a new column new_col where the value in every row is 10
linelist <- linelist %>% 
  mutate(new_col = 10)


### Below, a new column bmi is created to hold the Body Mass Index (BMI) 
## for each case - as calculated using the formula BMI = kg/m^2, 
## using column ht_cm and column wt_kg
linelist <- linelist %>% 
  mutate(bmi = wt_kg / (ht_cm/100)^2)


#### Below are examples of new columns, including ones that consist of values
###from other columns combined using str_glue() from the stringr package

new_col_demo <- linelist %>%                       
  mutate(
    new_var_dup    = case_id,             # new column = duplicate/copy another existing column
    new_var_static = 7,                   # new column = all values the same
    new_var_static = new_var_static + 5,  # you can overwrite a column, and it can be a calculation using other variables
    new_var_paste  = stringr::str_glue("{hospital} on ({date_hospitalisation})") # new column = pasting together values from other columns
  ) %>% 
  select(case_id, hospital, date_hospitalisation, contains("new"))        # show only new columns, for demonstration purposes


### Convert column class
###  the class of the age column is character. To perform quantitative analyses,
## we need these numbers to be recognized as numeric
class(linelist$age)

The class of the date_onset column is also character! To perform analyses, 
these dates must be recognized as dates

class(linelist$date_onset)

#### Here is a basic example, converting or ensuring that the column age 
### is class Numeric:
linelist <- linelist %>% 
  mutate(age = as.numeric(age))
class(linelist$age)

#### In a similar way, you can use as.character() and as.logical(). 
#### To convert to class Factor, you can use factor() from base R or 
###  as_factor() from forcats

### If your data frame is already grouped (see page on Grouping data),
### mutate() may behave differently than if the data frame is not grouped. 
### Any summarizing functions, like mean(), median(), max(), etc. 
### will calculate by group, not by all the rows

# age normalized to mean of ALL rows
linelist %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

# age normalized to mean of hospital group
linelist %>% 
  group_by(hospital) %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

### Transform multiple columns
## Here the transformation as.character() is applied to specific columns named within across()

linelist <- linelist %>% 
  mutate(across(.cols = c(temp, ht_cm, wt_kg), .fns = as.character))
### Here is an example of how one would change all columns to character class:

#to change all columns to character class
linelist <- linelist %>% 
  mutate(across(.cols = everything(), .fns = as.character))

### Convert to character all columns where the name contains the string “date” 
### (note the placement of commas and parentheses)
#to change all columns to character class
linelist <- linelist %>% 
  mutate(across(.cols = contains("date"), .fns = as.character))


### an example of mutating the columns that are currently class POSIXct 
### (a raw datetime class that shows timestamps) - in other words, where the 
### function is.POSIXct() evaluates to TRUE. Then we want to apply the function 
### as.Date() to these columns to convert them to a normal class Date

linelist <- linelist %>% 
  mutate(across(.cols = where(is.POSIXct), .fns = as.Date))

#### Cumulative math

cumulative_case_counts <- linelist %>%  # begin with case linelist
  count(date_onset) %>%                 # count of rows per day, as column 'n'   
  mutate(cumulative_cases = cumsum(n))  # new column, of the cumulative sum at each row
head(cumulative_case_counts, 10)

#### Add to pipe chain
### Below, a new column is added to the pipe chain and some classes are converted

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  ###################################################
# add new column
mutate(bmi = wt_kg / (ht_cm/100)^2) %>% 
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) 



#### RE-CODING VALUES
### Here are a few scenarios where you need to re-code (change) values:

## to edit one specific value (e.g. one date with an incorrect year or format)
## to reconcile values not spelled the same
## to create a new column of categorical values
## to create a new column of numeric categories (e.g. age categories)

## Specific value: Imagine there is a nonsensical date in the data (e.g. “2014-14-15”): 

# fix incorrect values                   # old value       # new value
linelist <- linelist %>% 
  mutate(date_onset = recode(date_onset, 
                             "2014-14-15" = "2014-04-15"))

### In linelist the values in the column “hospital” must be cleaned. 
##  There are several different spellings and many missing values
table(linelist$hospital, useNA = "always")  # print table of all unique values, including missing  

### The recode() command below re-defines the column “hospital” as the current 
## column “hospital”, but with the specified recode changes. 
## Don’t forget commas after each!

linelist <- linelist %>% 
  mutate(hospital = recode(hospital,
                           # for reference: OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  ))

## Now we see the spellings in the hospital column have been corrected and consolidated:

table(linelist$hospital, useNA = "always")

### By logic
## Below we demonstrate how to re-code values in a column using logic and conditions:

##  Using replace(), ifelse() and if_else() for simple logic
## Using case_when() for more complex logic
# Example: change gender of one specific observation to "Female" 
linelist <- linelist %>% 
  mutate(gender = replace(gender, case_id == "2195", "Female"))

### If the value in source is missing, then the value in source_known is set to “unknown”.
linelist <- linelist %>% 
  mutate(source_known = ifelse(!is.na(source), "known", "unknown"))

# Create a date of death column, which is NA if patient has not died.
linelist <- linelist %>% 
  mutate(date_death = if_else(outcome == "Death", date_outcome, NA_real_))

#### COMPLEX LOGICS

linelist <- linelist %>% 
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age unit is years
    age_unit == "months" ~ age/12,    # if age unit is months, divide age by 12
    is.na(age_unit)      ~ age))      # if age unit is missing, assume years
# any other circumstance, assign NA (missing)

### Below is another example of case_when() used to create a new column with 
###the patient classification, according to a case definition for confirmed and suspect cases:
linelist <- linelist %>% 
  mutate(case_status = case_when(
    
    # if patient had lab test and it is positive,
    # then they are marked as a confirmed case 
    ct_blood < 20                   ~ "Confirmed",
    
    # given that a patient does not have a positive lab result,
    # if patient has a "source" (epidemiological link) AND has fever, 
    # then they are marked as a suspect case
    !is.na(source) & fever == "yes" ~ "Suspect",
    
    # any other patient not addressed above 
    # is marked for follow up
    TRUE                            ~ "To investigate"))


#### MISSING VALUES
## Below are special functions for handling missing values in the context of data cleaning.

linelist <- linelist %>% 
  mutate(hospital = replace_na(hospital, "Missing"))

### The easiest way to solve this is to use the forcats function fct_explicit_na() 
# which converts a column to class factor, and converts NA values to the character “(Missing)”.
linelist %>% 
  mutate(hospital = fct_na_value_to_level(hospital))

## Note: na_if() cannot be used for logic criteria 
## (e.g. “all values > 99”) - use replace() or case_when() for this

# Convert temperatures above 40 to NA 
linelist <- linelist %>% 
  mutate(temp = replace(temp, temp > 40, NA))

# Convert onset dates earlier than 1 Jan 2000 to missing
linelist <- linelist %>% 
  mutate(date_onset = replace(date_onset, date_onset > as.Date("2000-01-01"), NA))

## Add to pipe chain
# Below, some new columns and column transformations are added to the pipe chain.

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # de-duplicate
  distinct() %>% 
  
  # add column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>%     
  
  # convert class of columns
  mutate(across(contains("date"), as.Date), 
         generation = as.numeric(generation),
         age        = as.numeric(age)) %>% 
  
  # add column: delay to hospitalisation
  mutate(days_onset_hosp = as.numeric(date_hospitalisation - date_onset)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  ###################################################

# clean values of hospital column
mutate(hospital = recode(hospital,
                         # OLD = NEW
                         "Mitylira Hopital"  = "Military Hospital",
                         "Mitylira Hospital" = "Military Hospital",
                         "Military Hopital"  = "Military Hospital",
                         "Port Hopital"      = "Port Hospital",
                         "Central Hopital"   = "Central Hospital",
                         "other"             = "Other",
                         "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
)) %>% 
  
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  
  # create age_years column (from age and age_unit)
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_))


### NUMERIC CATEGORIES
#check the class of the linelist variable age
class(linelist$age_years)
# examine the distribution
hist(linelist$age_years)
summary(linelist$age_years, na.rm=T)

##AGE CATEGORIES: This can be used with other numerical data

# Simple example
################
pacman::p_load(epikit)                    # load package

linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(             # create new column
      age_years,                            # numeric column to make groups from
      breakers = c(0, 5, 10, 15, 20,        # break points
                   30, 40, 50, 60, 70)))

# show table
table(linelist$age_cat, useNA = "always")

# Include upper ends for the same categories
############################################
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 6, 11, 16, 21, 31, 41, 51, 61, 71)))

# show table
table(linelist$age_cat, useNA = "always")


# With ceiling set to TRUE
##########################
linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      breakers = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70),
      ceiling = TRUE)) # 70 is ceiling, all above become NA

# show table
table(linelist$age_cat, useNA = "always")

### Alternatively, instead of breakers =, you can provide all of lower =, upper =, and by =:

linelist <- linelist %>% 
  mutate(
    age_cat = age_categories(
      age_years, 
      lower = 0,
      upper = 100,
      by = 10))

# show table
table(linelist$age_cat, useNA = "always")

### Re-labeling NA values
linelist <- linelist %>% 
  
  # cut() creates age_cat, automatically of class Factor      
  mutate(age_cat = cut(
    age_years,
    breaks = c(0, 5, 10, 15, 20, 30, 50, 70, 100),          
    right = FALSE,
    include.lowest = TRUE,        
    labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-49", "50-69", "70-100")),
    
    # make missing values explicit
    age_cat = fct_explicit_na(
      age_cat,
      na_level = "Missing age")  # you can specify the label
  )    
###FILTERING ROWS
linelist <- linelist %>% 
  filter(gender == "f")   # keep only rows where gender is equal to "f"

linelist %>% 
  drop_na(case_id, age_years)  # drop rows with missing values for case_id or age_years
### FILTER BY ROW NUMBERS
# View first 100 rows
linelist %>% head(100)     # or use tail() to see the n last rows

# Show row 5 only
linelist %>% filter(row_number() == 5)

# View rows 2 through 20, and three specific columns
linelist %>% filter(row_number() %in% 2:20) %>% select(date_onset, outcome, age)

### ARRANGING LINELIST
linelist %>% 
  arrange(hospital, desc(date_onset))